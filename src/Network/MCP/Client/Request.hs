{-# LANGUAGE OverloadedStrings #-}

module Network.MCP.Client.Request where

import System.IO (hFlush)
import Control.Concurrent.MVar
import Control.Exception (throw)
import Data.Aeson
import Network.MCP.Client.Types
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

-- Validate JSON-RPC request structure
validateJsonRpcRequest :: Value -> Either McpClientError ()
validateJsonRpcRequest v = case v of
    Object obj ->
        let hasMethod = "method" `elem` keys obj
            hasJsonRpc = KeyMap.lookup "jsonrpc" obj == Just (String "2.0")
        in if hasMethod && hasJsonRpc
           then Right ()
           else Left $ ProtocolError "Invalid JSON-RPC request structure"
    _ -> Left $ ProtocolError "Request must be a JSON object"
  where
    keys = map fst . KeyMap.toList

-- Robust request sending and response parsing
sendClientRequest
    :: Client      -- stdin handle
    -> Value       -- request message
    -> IO Value    -- parsed result
sendClientRequest client message = do
    -- Validate request structure before sending
    case validateJsonRpcRequest message of
        Left err -> throw err
        Right () -> return ()

    hstdin <- readMVar (clientStdin client) >>= \case
        Just inp -> do return inp
        Nothing -> throw $ ConnectionError "Stdin not available"

    hstdout <- readMVar (clientStdout client) >>= \case
        Just outp -> do return outp
        Nothing -> throw $ ConnectionError "Stdout not available"

    -- Encode and send message
    let reqBs = BL.toStrict $ encode message
    C8.hPut hstdin reqBs
    C8.hPutStrLn hstdin (C8.pack "")
    hFlush hstdin

    -- Read and parse response
    responseStr <- C8.hGetLine hstdout
    let responseBS = BL.fromStrict responseStr

    -- Comprehensive response parsing
    case eitherDecode responseBS of
        Left decodeErr ->
            throw $ ProtocolError $ T.pack decodeErr
        Right (Object resp) ->
            parseResponse resp
        Right _ ->
            throw $ ProtocolError "Response must be a JSON object"

-- Parse the response, handling both success and error cases
parseResponse :: KeyMap.KeyMap Value -> IO Value
parseResponse resp =
    case (KeyMap.lookup "result" resp, KeyMap.lookup "error" resp) of
        (Just result, Nothing) ->
            return result
        (Nothing, Just (Object errorObj)) -> do
            let code = case KeyMap.lookup "code" errorObj of
                         Just (Number n) -> Just (round n)
                         _ -> Nothing
                message = case KeyMap.lookup "message" errorObj of
                            Just (String msg) -> msg
                            _ -> "Unknown server error"
                details = KeyMap.lookup "data" errorObj

            throw $ ServerError
                { serverErrorCode = code
                , serverErrorMessage = message
                , serverErrorData = details
                }
        (Nothing, Nothing) ->
            throw $ ProtocolError "Response lacks both result and error"
        _ ->
            throw $ ProtocolError "Invalid response structure"

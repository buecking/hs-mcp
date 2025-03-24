{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Network.MCP.Client
    ( createClient
    , connectClient
    , disconnectClient
    , listTools
    , callTool
    , listResources
    , readResource
    , listPrompts
    , getPrompt
    ) where

import Control.Concurrent.MVar
import Control.Exception (throw)
import Control.Monad (void)
import Data.Aeson
import System.IO
import System.Process
import Network.MCP.Client.Request
import Network.MCP.Client.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BLC

-- | Create a new client
createClient :: ClientConfig -> IO Client
createClient config = Client
    <$> pure config
    <*> newMVar Nothing
    <*> newMVar Nothing
    <*> newMVar Nothing

-- | Connect to the MCP server via StdIO
connectClient :: Client -> FilePath -> [String] -> IO Client
connectClient client@Client{..} cmd args = do

    -- TODO:
    --
    --   Create a pipe for interprocess communication and return a (readEnd,
    --   writeEnd) Handle pair.
    --
    --   WinIO Support hen this function is used with WinIO enabled it's
    --   Wthe caller's responsibility to register the handles with the
    --   WI/O manager. If this is not done the operation will deadlock.
    --   WAssociation can be done as follows:
    --
    -- https://hackage.haskell.org/package/process-1.6.25.0/docs/System-Process.html#g:8

    (Just hstdin, Just hstdout, Nothing, ph) <-
        createProcess (proc cmd args)
            { std_in = CreatePipe
            , std_out = CreatePipe
            -- was std_err meant to be used for debugging or comms?
            -- our implementation requires stderr be use
            , std_err = Inherit
            }

    -- Send initialization message
    let initMessage = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("initialize" :: T.Text)
            , "params" .= object
                [ "clientInfo" .= object
                    [ "name" .= clientName clientConfig
                    , "version" .= clientVersion clientConfig
                    ]
                , "capabilities" .= clientCapabilities clientConfig
                ]
            , "id" .= (1 :: Int)
            ]

    -- Write initialization message
    BLC.hPutStrLn hstdin (encode initMessage)
    hFlush hstdin

    void $ swapMVar clientProcess (Just ph)
    void $ swapMVar clientStdin (Just hstdin)
    void $ swapMVar clientStdout (Just hstdout)

    return client

-- | Disconnect from the server
disconnectClient :: Client -> IO ()
disconnectClient Client{..} = do
    -- Close stdin and stdout
    readMVar clientStdin >>= mapM_ hClose
    readMVar clientStdout >>= mapM_ hClose

    -- Terminate process
    readMVar clientProcess >>= mapM_ terminateProcess

-- | List available tools
listTools :: Client -> IO [Value]
listTools client = do
    let message = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/list" :: T.Text)
            , "id" .= (2 :: Int)
            ]

    -- Send message and handle response
    response <- sendClientRequest client message

    -- Extract tools from response
    case fromJSON response of
        Success tools -> return tools
        Error err -> throw $ ProtocolError (T.pack err)

-- | Call a tool with arguments
callTool :: Client -> T.Text -> Map.Map T.Text Value -> IO Value
callTool client toolName args = do
    let message = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/call" :: T.Text)
            , "params" .= object
                [ "name" .= toolName
                , "arguments" .= args
                ]
            , "id" .= (3 :: Int)
            ]

    -- Send message and handle response
    sendClientRequest client message

-- | List available resources
listResources :: Client -> IO [Value]
listResources client = do
    let message = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("resources/list" :: T.Text)
            , "id" .= (4 :: Int)
            ]

    -- Send message and handle response
    response <- sendClientRequest client message

    -- Extract resources from response
    case fromJSON response of
        Success resources -> return resources
        Error err -> throw $ ProtocolError (T.pack err)

-- | Read a specific resource
readResource :: Client -> T.Text -> IO Value
readResource client resourceUri = do
    let message = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("resources/read" :: T.Text)
            , "params" .= object
                [ "uri" .= resourceUri ]
            , "id" .= (5 :: Int)
            ]

    -- Send message and handle response
    sendClientRequest client message




listPrompts :: Client -> IO [Value]
listPrompts client = do
    -- Get stdin handle

    -- Create the request
    let message = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("prompts/list" :: T.Text)
            , "id" .= (6 :: Int)
            ]

    -- Send message and handle response
    response <- sendClientRequest client message

    -- Extract prompts from response
    case fromJSON response of
        Success prompts -> return prompts
        Error err -> throw $ ProtocolError (T.pack err)

-- | Get a specific prompt
getPrompt :: Client -> T.Text -> Map.Map T.Text Value -> IO Value
getPrompt client promptName args = do
    let message = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("prompts/get" :: T.Text)
            , "params" .= object
                [ "name" .= promptName
                , "arguments" .= args
                ]
            , "id" .= (7 :: Int)
            ]

    -- Send message and handle response
    sendClientRequest client message

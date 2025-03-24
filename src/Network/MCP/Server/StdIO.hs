{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.MCP.Server.StdIO
  ( runServerWithSTDIO
  ) where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM
import Data.Aeson
import Network.MCP.Server
import Network.MCP.Transport.StdIO
import Network.MCP.Transport.Types
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T

-- | Run an MCP server using STDIO transport
runServerWithSTDIO :: Server -> IO ()
runServerWithSTDIO server = do

  -- Configure stdin/stdout for better performance
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8

  -- Create message handler
  let onMessage msg = case msg of
        RequestMessage request -> do
          -- Log the received request
          BLC.hPutStrLn stderr $ "Received request: " <> encode request

          -- Process the request
          handleServerRequest server request

        NotificationMessage notification -> do
          -- Log the received notification
          BLC.hPutStrLn stderr $ "Received notification: " <> encode notification

          -- Process the notification
          handleServerNotification server notification

        _ -> BLC.hPutStrLn stderr "Received unexpected message type"

  -- Create a channel for server-to-transport communication
  _messageQueue <- newTQueueIO

  -- Handle transport closure
  let onClosed = putStrLn "STDIO transport closed"

  -- Handle transport errors
  let onError err = BLC.hPutStrLn stderr $ "STDIO transport error: " <> BLC.pack (show err)

  -- Create the STDIO transport
  transport <- newSTDIOTransport onMessage onClosed onError

  -- Start the transport
  runWithSTDIOTransport transport

  -- Wait for the server to complete (this won't happen with STDIO until the process is terminated)
  forever $ threadDelay 1000000  -- 1 second

-- | Handle a server request
handleServerRequest :: Server -> Request -> IO ()
handleServerRequest server request = do
  -- Process the request
  result <- handleRequest server request

  -- Send the response
  case result of
    Right response -> do
      BLC.hPutStrLn stdout $ encode $ ResponseMessage response
      hFlush stdout

    Left err -> do
      -- Create an error response
      let errorResponse = Response
            { responseJsonrpc = JSONRPC "2.0"
            , responseId = requestId request
            , responseResult = Nothing
            , responseError = Just $ ErrorResponse
                { errorCode = -32603
                , errorMessage = T.pack $ show err
                , errorData = Nothing
                }
            }

      BLC.hPutStrLn stdout $ encode $ ResponseMessage errorResponse
      hFlush stdout

-- | Handle a server notification
handleServerNotification :: Server -> Notification -> IO ()
handleServerNotification _server _notification = do
  -- TODO: Implement notification handling
  return ()

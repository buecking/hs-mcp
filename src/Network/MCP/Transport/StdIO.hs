{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.MCP.Transport.StdIO
  ( STDIOTransport(..)
  , newSTDIOTransport
  , runWithSTDIOTransport
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, throwIO)
import Control.Exception (toException)
import Control.Monad (forever, void, when)
import Data.Aeson
import Network.MCP.Transport.Types
import System.IO
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL

-- | STDIO implementation of the Transport interface
data STDIOTransport = STDIOTransport
  { stdinHandle :: Handle
  , stdoutHandle :: Handle
  , stderrHandle :: Handle
  , messageQueue :: TQueue Message
  , onReceiveMessage :: Message -> IO ()
  , onTransportClosed :: IO ()
  , onTransportError :: SomeException -> IO ()
  , transportClosed :: TVar Bool
  }

-- | Create a new STDIO transport with the given message handler
newSTDIOTransport
    :: (Message -> IO ())
    -> IO ()
    -> (SomeException -> IO ())
    -> IO STDIOTransport
newSTDIOTransport onReceive onClosed onError = do
  -- Configure handles for better performance
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8

  queue <- newTQueueIO
  closed <- newTVarIO False

  return $ STDIOTransport
    { stdinHandle = stdin
    , stdoutHandle = stdout
    , stderrHandle = stderr
    , messageQueue = queue
    , onReceiveMessage = onReceive
    , onTransportClosed = onClosed
    , onTransportError = onError
    , transportClosed = closed
    }

-- | Start running the STDIO transport
runWithSTDIOTransport :: STDIOTransport -> IO ()
runWithSTDIOTransport transport = do
  -- Start reader thread
  void $ forkIO $ readThread transport `catch` \(e :: SomeException) -> do
    BS8.hPutStrLn (stderrHandle transport) $ "Reader thread error: " <> BS8.pack (show e)
    onTransportError transport e
    atomically $ writeTVar (transportClosed transport) True
    onTransportClosed transport

  -- Start writer thread
  void $ forkIO $ writeThread transport `catch` \(e :: SomeException) -> do
    BS8.hPutStrLn (stderrHandle transport) $ "Writer thread error: " <> BS8.pack (show e)
    onTransportError transport e
    atomically $ writeTVar (transportClosed transport) True
    onTransportClosed transport

readThread :: STDIOTransport -> IO ()
readThread transport = forever $ do
  isClosed <- readTVarIO (transportClosed transport)
  when isClosed $ return ()

  -- Use Lazy ByteString version:
  line <- BS8.hGetLine (stdinHandle transport)
  case eitherDecode $ BS8.fromStrict line of
    Left err -> do
      BS8.hPutStrLn (stderrHandle transport) $ "Error decoding message: " <> BS8.pack  err
      -- onTransportError transport (userError $ "JSON decode error: " ++ err)
      onTransportError transport (toException $ userError $ "JSON decode error: " ++ err)


    Right msg -> onReceiveMessage transport msg

-- | Thread for writing to stdout
writeThread :: STDIOTransport -> IO ()
writeThread transport = forever $ do
  isClosed <- readTVarIO (transportClosed transport)
  when isClosed $ return ()

  msg <- atomically $ readTQueue (messageQueue transport)
  BL.hPut (stdoutHandle transport) (encode msg)
  BL.hPut (stdoutHandle transport) "\n"
  hFlush (stdoutHandle transport)

instance Transport STDIOTransport where

  readMessage transport = do
    line <- BS8.hGetLine (stdinHandle transport)
    case eitherDecode (BS8.fromStrict line) of
      Left err ->
        -- On parse error, log and try again with a default error message
        throwIO $ toException $ userError ("JSON decode error: " ++ err)
      Right msg ->
        return msg

  -- | Send a message through the transport
  sendMessage transport msg = do
    isClosed <- readTVarIO (transportClosed transport)
    if isClosed
      then return $ Left $ TransportError "Transport closed"
      else do
        atomically $ writeTQueue (messageQueue transport) msg
        return $ Right ()

  -- | Close the transport
  closeTransport transport = do
    atomically $ writeTVar (transportClosed transport) True
    onTransportClosed transport
    return ()

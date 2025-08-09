{-# LANGUAGE OverloadedStrings #-}
module Network.MCP.Client.Types
    ( Client(..)
    , ClientConfig(..)
    , McpClientError(..)
    ) where

import Control.Concurrent.MVar
import Control.Exception (Exception)
import Data.Aeson (Value)
import System.IO (Handle)
import System.Process
import qualified Data.Text as T

-- Precise error type for client requests
data McpClientError
    = ConnectionError T.Text
    | ProtocolError T.Text
    | ServerError
        { serverErrorCode :: Maybe Int
        , serverErrorMessage :: T.Text
        , serverErrorData :: Maybe Value
        }
    deriving (Show)

instance Exception McpClientError

-- | Client configuration
-- Includes protocol version for initialize handshake
data ClientConfig = ClientConfig
    { clientName :: T.Text
    , clientVersion :: T.Text
    , clientCapabilities :: Value
    , clientProtocolVersion :: T.Text
    }

-- | Client state
data Client = Client
    { clientConfig :: ClientConfig
    , clientProcess :: MVar (Maybe ProcessHandle)
    , clientStdin :: MVar (Maybe Handle)
    , clientStdout :: MVar (Maybe Handle)
    } 
module Network.MCP.Client.Types
    ( Client(..)
    , ClientConfig(..)
--    , ClientError(..)
    , McpClientError(..)
    ) where

import Control.Concurrent.MVar
import Control.Exception (Exception, throw)
import Data.Aeson (Value)
import System.IO (Handle,  hFlush, hGetLine)
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
data ClientConfig = ClientConfig
    { clientName :: T.Text
    , clientVersion :: T.Text
    , clientCapabilities :: Value
    }

{-
-- | Custom client errors
data ClientError
    = ConnectionError T.Text
    | ProtocolError T.Text
    | ServerError T.Text
    deriving (Show)
instance Exception ClientError
-}


-- | Client state
data Client = Client
    { clientConfig :: ClientConfig
    , clientProcess :: MVar (Maybe ProcessHandle)
    , clientStdin :: MVar (Maybe Handle)
    , clientStdout :: MVar (Maybe Handle)
    }


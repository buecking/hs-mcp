{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.MCP.Transport.Types
  ( Transport(..)
  , Message(..)
  , Request(..)
  , Response(..)
  , Notification(..)
  , ErrorResponse(..)
  , TransportError(..)
  , JSONRPC(..)
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics

-- JSON-RPC protocol constant
newtype JSONRPC = JSONRPC { unJSONRPC :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON JSONRPC where
  toJSON = toJSON . unJSONRPC

instance FromJSON JSONRPC where
  parseJSON = withText "JSONRPC" $ \t ->
    if t == "2.0"
      then return $ JSONRPC t
      else fail $ "Expected JSONRPC version 2.0, got: " ++ show t

-- | JSON-RPC Request
data Request = Request
  { requestJsonrpc :: JSONRPC
  , requestId :: Value
  , requestMethod :: Text
  , requestParams :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON Request where
  toJSON Request{..} = object $
    [ "jsonrpc" .= requestJsonrpc
    , "id" .= requestId
    , "method" .= requestMethod
    ] ++
    [ "params" .= p | p <- maybeToList requestParams ]

instance FromJSON Request where
  parseJSON = withObject "Request" $ \o -> do
    jsonrpc <- o .: "jsonrpc"
    id' <- o .: "id"
    method <- o .: "method"
    params <- o .:? "params"
    return $ Request jsonrpc id' method params

-- | JSON-RPC Notification (no ID)
data Notification = Notification
  { notificationJsonrpc :: JSONRPC
  , notificationMethod :: Text
  , notificationParams :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON Notification where
  toJSON Notification{..} = object $
    [ "jsonrpc" .= notificationJsonrpc
    , "method" .= notificationMethod
    ] ++
    [ "params" .= p | p <- maybeToList notificationParams ]

instance FromJSON Notification where
  parseJSON = withObject "Notification" $ \o -> do
    jsonrpc <- o .: "jsonrpc"
    method <- o .: "method"
    params <- o .:? "params"
    return $ Notification jsonrpc method params

-- | JSON-RPC Error Response
data ErrorResponse = ErrorResponse
  { errorCode :: Int
  , errorMessage :: Text
  , errorData :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON ErrorResponse where
  toJSON ErrorResponse{..} = object $
    [ "code" .= errorCode
    , "message" .= errorMessage
    ] ++
    [ "data" .= d | d <- maybeToList errorData ]

instance FromJSON ErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \o -> do
    code <- o .: "code"
    message <- o .: "message"
    errorData <- o .:? "data"
    return $ ErrorResponse code message errorData

-- | JSON-RPC Response
data Response = Response
  { responseJsonrpc :: JSONRPC
  , responseId :: Value
  , responseResult :: Maybe Value
  , responseError :: Maybe ErrorResponse
  } deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON Response{..} = object $
    [ "jsonrpc" .= responseJsonrpc
    , "id" .= responseId
    ] ++
    [ "result" .= r | r <- maybeToList responseResult ] ++
    [ "error" .= e | e <- maybeToList responseError ]

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    jsonrpc <- o .: "jsonrpc"
    id' <- o .: "id"
    result <- o .:? "result"
    error' <- o .:? "error"
    return $ Response jsonrpc id' result error'

-- | Combined message type for the transport layer
data Message
  = RequestMessage Request
  | ResponseMessage Response
  | NotificationMessage Notification
  deriving (Show, Eq)

instance ToJSON Message where
  toJSON (RequestMessage req) = toJSON req
  toJSON (ResponseMessage res) = toJSON res
  toJSON (NotificationMessage notif) = toJSON notif

instance FromJSON Message where
    parseJSON val = do
        obj <- parseJSON val
        id' <- obj .:? "id" :: Parser (Maybe Value)
        method <- obj .:? "method" :: Parser (Maybe Text)
        case (id', method) of
            (Just _, Just _) -> RequestMessage <$> parseJSON val
            (Just _, Nothing) -> ResponseMessage <$> parseJSON val
            (Nothing, Just _) -> NotificationMessage <$> parseJSON val
            _ -> fail "Invalid JSON-RPC message"

-- | Transport error
data TransportError = TransportError String
  deriving (Show, Eq)

-- | Transport interface for all transport implementations
class Transport t where
  -- | Send a message through the transport
  sendMessage :: t -> Message -> IO (Either TransportError ())

  -- | Close the transport
  closeTransport :: t -> IO ()
  readMessage :: t -> IO Message


-- Helper function
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

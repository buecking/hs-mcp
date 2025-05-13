{-# LANGUAGE OverloadedStrings #-}

module Network.MCP.Server.Types where

import Control.Exception (SomeException)
import Data.Aeson
import Data.Map.Strict (Map)
import Control.Concurrent.STM
import Data.Text (Text)
import Network.MCP.Types

-- | Server type with resources, tools, prompts and handlers
data Server = Server
  { serverInfo :: ServerInfo                               -- ^ Server information
  , serverCapabilities :: ServerCapabilities               -- ^ Server capabilities
  , serverResources :: TVar [Resource]                     -- ^ Available resources
  , serverTools :: TVar [Tool]                             -- ^ Available tools
  , serverPrompts :: TVar [Prompt]                         -- ^ Available prompts
  , serverInstructions :: Text                             -- ^ Instructions
  , serverResourceReadHandler :: TVar (Maybe ResourceReadHandler)  -- ^ Resource read handler
  , serverToolCallHandler :: TVar (Maybe ToolCallHandler)         -- ^ Tool call handler
  , serverPromptHandler :: TVar (Maybe PromptHandler)             -- ^ Prompt handler
  , serverMessageHandlers :: TVar (Map Text (Value -> Server -> IO (Either SomeException Value)))  -- ^ Request handlers by method
  }

-- | Server handler for message processing
type ServerHandler = Server -> Value -> IO Value

-- | Resource read handler type
type ResourceReadHandler = ReadResourceRequest -> IO ReadResourceResult

-- | Tool call handler type
type ToolCallHandler = CallToolRequest -> IO CallToolResult

-- | Prompt handler type
type PromptHandler = GetPromptRequest -> IO GetPromptResult

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Network.MCP.Types
  ( -- * Server Information
    ServerInfo
  , ClientInfo
  , Implementation(..)
    -- * Capabilities
  , ServerCapabilities(..)
  , ClientCapabilities(..)
  , ResourcesCapability(..)
  , ToolsCapability(..)
  , PromptsCapability(..)
  , SamplingCapability(..)
  , RootsCapability(..)
    -- * Resources
  , Resource(..)
  , ResourceContent(..)
  , ResourceContentType(..)
    -- * Tools
  , Tool(..)
  , ToolContent(..)
  , ToolContentType(..)
    -- * Prompts
  , Prompt(..)
  , PromptArgument(..)
  , PromptMessage(..)
  , PromptContentType(..)
  , PromptContent(..)
    -- * Roots
  , Root(..)
    -- * Protocol Versions
  , ProtocolVersion
  , supportedVersions
    -- * Initialization
  , ServerInitializeOptions(..)
  , ClientInitializeOptions(..)
  , ServerInitializeResult
  , ClientInitializeResult
    -- * Resource Requests
  , ListResourcesRequest(..)
  , ListResourcesResult(..)
  , ReadResourceRequest(..)
  , ReadResourceResult(..)
  , SubscribeResourceRequest(..)
  , SubscribeResourceResult(..)
  , UnsubscribeResourceRequest(..)
  , UnsubscribeResourceResult(..)
    -- * Tool Requests
  , ListToolsRequest(..)
  , ListToolsResult(..)
  , CallToolRequest(..)
  , CallToolResult(..)
    -- * Prompt Requests
  , ListPromptsRequest(..)
  , ListPromptsResult(..)
  , GetPromptRequest(..)
  , GetPromptResult(..)
    -- * Roots Requests
  , ListRootsRequest(..)
  , ListRootsResult(..)
    -- * Notifications
  , ResourcesListChangedNotification(..)
  , ResourceUpdatedNotification(..)
  , ToolsListChangedNotification(..)
  , PromptsListChangedNotification(..)
  ) where

import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map.Strict as Map

-- | Protocol version
type ProtocolVersion = Text

-- | Supported protocol versions
supportedVersions :: [ProtocolVersion]
supportedVersions =
  [ "2024-11-05"
  ]

-- | Implementation information
data Implementation = Implementation
  { serverName :: Text     -- ^ Name of the implementation
  , serverVersion :: Text  -- ^ Version of the implementation
  } deriving (Show, Eq, Generic)

instance ToJSON Implementation where
  toJSON Implementation{..} = object
    [ "name" .= serverName
    , "version" .= serverVersion
    ]

instance FromJSON Implementation where
  parseJSON = withObject "Implementation" $ \o -> do
    name <- o .: "name"
    version <- o .: "version"
    return $ Implementation name version

-- | Server information sent during initialization
type ServerInfo = Implementation

-- | Client information sent during initialization
type ClientInfo = Implementation

-- | Resources capability configuration
data ResourcesCapability = ResourcesCapability
  { resourcesListChanged :: Bool  -- ^ Server can notify when resources list changes
  } deriving (Show, Eq, Generic)

instance ToJSON ResourcesCapability where
  toJSON ResourcesCapability{..} = object
    [ "listChanged" .= resourcesListChanged
    ]

instance FromJSON ResourcesCapability where
  parseJSON = withObject "ResourcesCapability" $ \o -> do
    listChanged <- o .: "listChanged"
    return $ ResourcesCapability listChanged

-- | Tools capability configuration
data ToolsCapability = ToolsCapability
  { toolsListChanged :: Bool  -- ^ Server can notify when tools list changes
  } deriving (Show, Eq, Generic)

instance ToJSON ToolsCapability where
  toJSON ToolsCapability{..} = object
    [ "listChanged" .= toolsListChanged
    ]

instance FromJSON ToolsCapability where
  parseJSON = withObject "ToolsCapability" $ \o -> do
    listChanged <- o .: "listChanged"
    return $ ToolsCapability listChanged

-- | Prompts capability configuration
data PromptsCapability = PromptsCapability
  { promptsListChanged :: Bool  -- ^ Server can notify when prompts list changes
  } deriving (Show, Eq, Generic)

instance ToJSON PromptsCapability where
  toJSON PromptsCapability{..} = object
    [ "listChanged" .= promptsListChanged
    ]

instance FromJSON PromptsCapability where
  parseJSON = withObject "PromptsCapability" $ \o -> do
    listChanged <- o .: "listChanged"
    return $ PromptsCapability listChanged

-- | Sampling capability configuration
data SamplingCapability = SamplingCapability
  deriving (Show, Eq, Generic)

instance ToJSON SamplingCapability where
  toJSON _ = object []

instance FromJSON SamplingCapability where
  parseJSON = withObject "SamplingCapability" $ \_ ->
    return SamplingCapability

-- | Roots capability configuration
data RootsCapability = RootsCapability
  deriving (Show, Eq, Generic)

instance ToJSON RootsCapability where
  toJSON _ = object []

instance FromJSON RootsCapability where
  parseJSON = withObject "RootsCapability" $ \_ ->
    return RootsCapability

-- | Server capabilities
data ServerCapabilities = ServerCapabilities
  { resourcesCapability :: Maybe ResourcesCapability  -- ^ Resources support
  , toolsCapability :: Maybe ToolsCapability          -- ^ Tools support
  , promptsCapability :: Maybe PromptsCapability      -- ^ Prompts support
  } deriving (Show, Eq, Generic)

instance ToJSON ServerCapabilities where
  toJSON ServerCapabilities{..} = object $
    [ "resources" .= resources | resources <- maybeToList resourcesCapability ] ++
    [ "tools" .= tools | tools <- maybeToList toolsCapability ] ++
    [ "prompts" .= prompts | prompts <- maybeToList promptsCapability ]

instance FromJSON ServerCapabilities where
  parseJSON = withObject "ServerCapabilities" $ \o -> do
    resources <- o .:? "resources"
    tools <- o .:? "tools"
    prompts <- o .:? "prompts"
    return $ ServerCapabilities resources tools prompts

-- | Client capabilities
data ClientCapabilities = ClientCapabilities
  { clientRootsCapability :: Maybe RootsCapability        -- ^ Roots support
  , clientSamplingCapability :: Maybe SamplingCapability  -- ^ Sampling support
  } deriving (Show, Eq, Generic)

instance ToJSON ClientCapabilities where
  toJSON ClientCapabilities{..} = object $
    [ "roots" .= roots | roots <- maybeToList clientRootsCapability ] ++
    [ "sampling" .= sampling | sampling <- maybeToList clientSamplingCapability ]

instance FromJSON ClientCapabilities where
  parseJSON = withObject "ClientCapabilities" $ \o -> do
    roots <- o .:? "roots"
    sampling <- o .:? "sampling"
    return $ ClientCapabilities roots sampling

-- | Resource definition
data Resource = Resource
  { resourceUri :: Text                -- ^ URI of the resource
  , resourceName :: Text               -- ^ Human-readable name
  , resourceDescription :: Maybe Text  -- ^ Optional description
  , resourceMimeType :: Maybe Text     -- ^ Optional MIME type
  , resourceTemplate :: Maybe Text     -- ^ Optional URI template definition
  } deriving (Show, Eq, Generic)

instance ToJSON Resource where
  toJSON Resource{..} = object $
    [ "uri" .= resourceUri
    , "name" .= resourceName
    ] ++
    [ "description" .= d | d <- maybeToList resourceDescription ] ++
    [ "mimeType" .= m | m <- maybeToList resourceMimeType ] ++
    [ "template" .= t | t <- maybeToList resourceTemplate ]

instance FromJSON Resource where
  parseJSON = withObject "Resource" $ \o -> do
    uri <- o .: "uri"
    name <- o .: "name"
    description <- o .:? "description"
    mimeType <- o .:? "mimeType"
    template <- o .:? "template"
    return $ Resource uri name description mimeType template

-- | Resource content type
data ResourceContentType = TextContent | BlobContent
  deriving (Show, Eq, Generic)

-- | Resource content
data ResourceContent = ResourceContent
  { resourceContentUri :: Text                    -- ^ URI of the resource
  , resourceContentMimeType :: Maybe Text         -- ^ Optional MIME type
  , resourceContentText :: Maybe Text             -- ^ Text content (if TextContent)
  , resourceContentBlob :: Maybe Text             -- ^ Blob content (base64 encoded, if BlobContent)
  } deriving (Show, Eq, Generic)

instance ToJSON ResourceContent where
  toJSON ResourceContent{..} = object $
    [ "uri" .= resourceContentUri
    ] ++
    [ "mimeType" .= m | m <- maybeToList resourceContentMimeType ] ++
    [ "text" .= t | t <- maybeToList resourceContentText ] ++
    [ "blob" .= b | b <- maybeToList resourceContentBlob ]

instance FromJSON ResourceContent where
  parseJSON = withObject "ResourceContent" $ \o -> do
    uri <- o .: "uri"
    mimeType <- o .:? "mimeType"
    text <- o .:? "text"
    blob <- o .:? "blob"
    return $ ResourceContent uri mimeType text blob

-- | Tool definition
data Tool = Tool
  { toolName :: Text                -- ^ Name of the tool
  , toolDescription :: Maybe Text   -- ^ Optional description
  , toolInputSchema :: Value        -- ^ JSON schema for tool parameters
  } deriving (Show, Eq, Generic)

instance ToJSON Tool where
  toJSON Tool{..} = object $
    [ "name" .= toolName
    , "inputSchema" .= toolInputSchema
    ] ++
    [ "description" .= d | d <- maybeToList toolDescription ]

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    inputSchema <- o .: "inputSchema"
    return $ Tool name description inputSchema

-- | Tool content type
data ToolContentType = TextualContent | ImageContent | EmbeddedResource
  deriving (Show, Eq, Generic)

instance ToJSON ToolContentType where
  toJSON TextualContent = String "text"
  toJSON ImageContent = String "image"
  toJSON EmbeddedResource = String "resource"

instance FromJSON ToolContentType where
  parseJSON = withText "ToolContentType" $ \case
    "text" -> return TextualContent
    "image" -> return ImageContent
    "resource" -> return EmbeddedResource
    _ -> fail "Invalid tool content type"

-- | Tool content
data ToolContent = ToolContent
  { toolContentType :: ToolContentType        -- ^ Type of content
  , toolContentText :: Maybe Text             -- ^ Text content (if TextContent)
  -- More fields would be here for image and resource, simplified for brevity
  } deriving (Show, Eq, Generic)

instance ToJSON ToolContent where
  toJSON ToolContent{..} = object $
    [ "type" .= toolContentType
    ] ++
    [ "text" .= t | t <- maybeToList toolContentText ]

instance FromJSON ToolContent where
  parseJSON = withObject "ToolContent" $ \o -> do
    contentType <- o .: "type"
    text <- o .:? "text"
    return $ ToolContent contentType text

-- | Prompt argument definition
data PromptArgument = PromptArgument
  { promptArgumentName :: Text                -- ^ Argument name
  , promptArgumentDescription :: Maybe Text   -- ^ Optional description
  , promptArgumentRequired :: Bool            -- ^ Whether the argument is required
  } deriving (Show, Eq, Generic)

instance ToJSON PromptArgument where
  toJSON PromptArgument{..} = object $
    [ "name" .= promptArgumentName
    , "required" .= promptArgumentRequired
    ] ++
    [ "description" .= d | d <- maybeToList promptArgumentDescription ]

instance FromJSON PromptArgument where
  parseJSON = withObject "PromptArgument" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    required <- o .: "required"
    return $ PromptArgument name description required

-- | Prompt definition
data Prompt = Prompt
  { promptName :: Text                   -- ^ Name of the prompt
  , promptDescription :: Maybe Text      -- ^ Optional description
  , promptArguments :: [PromptArgument]  -- ^ Arguments for the prompt
  } deriving (Show, Eq, Generic)

instance ToJSON Prompt where
  toJSON Prompt{..} = object $
    [ "name" .= promptName
    , "arguments" .= promptArguments
    ] ++
    [ "description" .= d | d <- maybeToList promptDescription ]

instance FromJSON Prompt where
  parseJSON = withObject "Prompt" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    arguments <- o .: "arguments"
    return $ Prompt name description arguments

-- | Prompt content type
data PromptContentType = TextPromptContent | ResourcePromptContent
  deriving (Show, Eq, Generic)

instance ToJSON PromptContentType where
  toJSON TextPromptContent = String "text"
  toJSON ResourcePromptContent = String "resource"

instance FromJSON PromptContentType where
  parseJSON = withText "PromptContentType" $ \case
    "text" -> return TextPromptContent
    "resource" -> return ResourcePromptContent
    _ -> fail "Invalid prompt content type"

-- | Prompt content
data PromptContent = PromptContent
  { promptContentType :: PromptContentType   -- ^ Type of content
  , promptContentText :: Text                -- ^ Content text
  } deriving (Show, Eq, Generic)

instance ToJSON PromptContent where
  toJSON PromptContent{..} = object
    [ "type" .= promptContentType
    , "text" .= promptContentText
    ]

instance FromJSON PromptContent where
  parseJSON = withObject "PromptContent" $ \o -> do
    contentType <- o .: "type"
    text <- o .: "text"
    return $ PromptContent contentType text

-- | Prompt message
data PromptMessage = PromptMessage
  { promptMessageRole :: Text               -- ^ Message role (user/assistant)
  , promptMessageContent :: PromptContent   -- ^ Message content
  } deriving (Show, Eq, Generic)

instance ToJSON PromptMessage where
  toJSON PromptMessage{..} = object
    [ "role" .= promptMessageRole
    , "content" .= promptMessageContent
    ]

instance FromJSON PromptMessage where
  parseJSON = withObject "PromptMessage" $ \o -> do
    role <- o .: "role"
    content <- o .: "content"
    return $ PromptMessage role content

-- | Root definition
data Root = Root
  { rootUri :: Text      -- ^ URI for the root
  , rootName :: Text     -- ^ Human-readable name
  } deriving (Show, Eq, Generic)

instance ToJSON Root where
  toJSON Root{..} = object
    [ "uri" .= rootUri
    , "name" .= rootName
    ]

instance FromJSON Root where
  parseJSON = withObject "Root" $ \o -> do
    uri <- o .: "uri"
    name <- o .: "name"
    return $ Root uri name

-- | Server initialize options
data ServerInitializeOptions = ServerInitializeOptions
  { serverInitProtocolVersion :: ProtocolVersion  -- ^ Protocol version
  , serverInitInfo :: Implementation              -- ^ Server info
  , serverInitCapabilities :: ServerCapabilities  -- ^ Server capabilities
  , serverInitInstructions :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ServerInitializeOptions where
  toJSON ServerInitializeOptions{..} = object
    [ "protocolVersion" .= serverInitProtocolVersion
    , "serverInfo" .= serverInitInfo
    , "capabilities" .= serverInitCapabilities
    , "instructions" .= serverInitInstructions
    ]

instance FromJSON ServerInitializeOptions where
  parseJSON = withObject "ServerInitializeOptions" $ \o -> do
    version <- o .: "protocolVersion"
    impl <- o .: "serverInfo"
    capabilities <- o .: "capabilities"
    instructions <- o .: "instructions"
    return $ ServerInitializeOptions version impl capabilities instructions

-- | Client initialize options
data ClientInitializeOptions = ClientInitializeOptions
  { clientInitProtocolVersion :: ProtocolVersion  -- ^ Protocol version
  , clientInitInfo :: Implementation              -- ^ Client info
  , clientInitCapabilities :: ClientCapabilities  -- ^ Client capabilities
  } deriving (Show, Eq, Generic)

instance ToJSON ClientInitializeOptions where
  toJSON ClientInitializeOptions{..} = object
    [ "protocolVersion" .= clientInitProtocolVersion
    , "clientInfo" .= clientInitInfo
    , "capabilities" .= clientInitCapabilities
    ]

instance FromJSON ClientInitializeOptions where
  parseJSON = withObject "ClientInitializeOptions" $ \o -> do
    version <- o .: "protocolVersion"
    impl <- o .: "clientInfo"
    capabilities <- o .: "capabilities"
    return $ ClientInitializeOptions version impl capabilities

-- | Server initialize result
type ServerInitializeResult = ClientInitializeOptions

-- | Client initialize result
type ClientInitializeResult = ServerInitializeOptions

-- | List resources request
data ListResourcesRequest = ListResourcesRequest
  deriving (Show, Eq, Generic)

instance ToJSON ListResourcesRequest where
  toJSON _ = object []

instance FromJSON ListResourcesRequest where
  parseJSON = withObject "ListResourcesRequest" $ \_ ->
    return ListResourcesRequest

-- | List resources result
data ListResourcesResult = ListResourcesResult
  { listResourcesResult :: [Resource]  -- ^ Available resources
  } deriving (Show, Eq, Generic)

instance ToJSON ListResourcesResult where
  toJSON ListResourcesResult{..} = object
    [ "resources" .= listResourcesResult
    ]

instance FromJSON ListResourcesResult where
  parseJSON = withObject "ListResourcesResult" $ \o -> do
    resources <- o .: "resources"
    return $ ListResourcesResult resources

-- | Read resource request
data ReadResourceRequest = ReadResourceRequest
  { resourceReadUri :: Text  -- ^ URI of the resource to read
  } deriving (Show, Eq, Generic)

instance ToJSON ReadResourceRequest where
  toJSON ReadResourceRequest{..} = object
    [ "uri" .= resourceReadUri
    ]

instance FromJSON ReadResourceRequest where
  parseJSON = withObject "ReadResourceRequest" $ \o -> do
    uri <- o .: "uri"
    return $ ReadResourceRequest uri

-- | Read resource result
data ReadResourceResult = ReadResourceResult
  { readResourceContents :: [ResourceContent]  -- ^ Resource contents
  } deriving (Show, Eq, Generic)

instance ToJSON ReadResourceResult where
  toJSON ReadResourceResult{..} = object
    [ "contents" .= readResourceContents
    ]

instance FromJSON ReadResourceResult where
  parseJSON = withObject "ReadResourceResult" $ \o -> do
    contents <- o .: "contents"
    return $ ReadResourceResult contents

-- | Subscribe resource request
data SubscribeResourceRequest = SubscribeResourceRequest
  { subscribeResourceUri :: Text  -- ^ URI of the resource to subscribe to
  } deriving (Show, Eq, Generic)

instance ToJSON SubscribeResourceRequest where
  toJSON SubscribeResourceRequest{..} = object
    [ "uri" .= subscribeResourceUri
    ]

instance FromJSON SubscribeResourceRequest where
  parseJSON = withObject "SubscribeResourceRequest" $ \o -> do
    uri <- o .: "uri"
    return $ SubscribeResourceRequest uri

-- | Subscribe resource result
data SubscribeResourceResult = SubscribeResourceResult
  deriving (Show, Eq, Generic)

instance ToJSON SubscribeResourceResult where
  toJSON _ = object []

instance FromJSON SubscribeResourceResult where
  parseJSON = withObject "SubscribeResourceResult" $ \_ ->
    return SubscribeResourceResult

-- | Unsubscribe resource request
data UnsubscribeResourceRequest = UnsubscribeResourceRequest
  { unsubscribeResourceUri :: Text  -- ^ URI of the resource to unsubscribe from
  } deriving (Show, Eq, Generic)

instance ToJSON UnsubscribeResourceRequest where
  toJSON UnsubscribeResourceRequest{..} = object
    [ "uri" .= unsubscribeResourceUri
    ]

instance FromJSON UnsubscribeResourceRequest where
  parseJSON = withObject "UnsubscribeResourceRequest" $ \o -> do
    uri <- o .: "uri"
    return $ UnsubscribeResourceRequest uri

-- | Unsubscribe resource result
data UnsubscribeResourceResult = UnsubscribeResourceResult
  deriving (Show, Eq, Generic)

instance ToJSON UnsubscribeResourceResult where
  toJSON _ = object []

instance FromJSON UnsubscribeResourceResult where
  parseJSON = withObject "UnsubscribeResourceResult" $ \_ ->
    return UnsubscribeResourceResult

-- | List tools request
data ListToolsRequest = ListToolsRequest
  deriving (Show, Eq, Generic)

instance ToJSON ListToolsRequest where
  toJSON _ = object []

instance FromJSON ListToolsRequest where
  parseJSON = withObject "ListToolsRequest" $ \_ ->
    return ListToolsRequest

-- | List tools result
data ListToolsResult = ListToolsResult
  { listToolsResult :: [Tool]  -- ^ Available tools
  } deriving (Show, Eq, Generic)

instance ToJSON ListToolsResult where
  toJSON ListToolsResult{..} = object
    [ "tools" .= listToolsResult
    ]

instance FromJSON ListToolsResult where
  parseJSON = withObject "ListToolsResult" $ \o -> do
    tools <- o .: "tools"
    return $ ListToolsResult tools

-- | Call tool request
data CallToolRequest = CallToolRequest
  { callToolName :: Text                  -- ^ Name of the tool to call
  , callToolArguments :: Map Text Value   -- ^ Tool arguments
  } deriving (Show, Eq, Generic)

instance ToJSON CallToolRequest where
  toJSON CallToolRequest{..} = object
    [ "name" .= callToolName
    , "arguments" .= callToolArguments
    ]

instance FromJSON CallToolRequest where
  parseJSON = withObject "CallToolRequest" $ \o -> do
    name <- o .: "name"
    arguments <- o .: "arguments"
    return $ CallToolRequest name arguments

-- | Call tool result
data CallToolResult = CallToolResult
  { callToolContent :: [ToolContent]  -- ^ Tool execution result content
  , callToolIsError :: Bool           -- ^ Whether the result is an error
  } deriving (Show, Eq, Generic)

instance ToJSON CallToolResult where
  toJSON CallToolResult{..} = object
    [ "content" .= callToolContent
    , "isError" .= callToolIsError
    ]

instance FromJSON CallToolResult where
  parseJSON = withObject "CallToolResult" $ \o -> do
    content <- o .: "content"
    isError <- o .: "isError"
    return $ CallToolResult content isError

-- | List prompts request
data ListPromptsRequest = ListPromptsRequest
  deriving (Show, Eq, Generic)

instance ToJSON ListPromptsRequest where
  toJSON _ = object []

instance FromJSON ListPromptsRequest where
  parseJSON = withObject "ListPromptsRequest" $ \_ ->
    return ListPromptsRequest

-- | List prompts result
data ListPromptsResult = ListPromptsResult
  { listPromptsResult :: [Prompt]  -- ^ Available prompts
  } deriving (Show, Eq, Generic)

instance ToJSON ListPromptsResult where
  toJSON ListPromptsResult{..} = object
    [ "prompts" .= listPromptsResult
    ]

instance FromJSON ListPromptsResult where
  parseJSON = withObject "ListPromptsResult" $ \o -> do
    prompts <- o .: "prompts"
    return $ ListPromptsResult prompts

-- | Get prompt request
data GetPromptRequest = GetPromptRequest
  { getPromptName :: Text                 -- ^ Name of the prompt to get
  , getPromptArguments :: Map Text Text   -- ^ Prompt arguments
  } deriving (Show, Eq, Generic)

instance ToJSON GetPromptRequest where
  toJSON GetPromptRequest{..} = object
    [ "name" .= getPromptName
    , "arguments" .= getPromptArguments
    ]

instance FromJSON GetPromptRequest where
  parseJSON = withObject "GetPromptRequest" $ \o -> do
    name <- o .: "name"
    arguments <- o .:? "arguments" .!= Map.empty
    return $ GetPromptRequest name arguments

-- | Get prompt result
data GetPromptResult = GetPromptResult
  { getPromptDescription :: Maybe Text    -- ^ Optional description
  , getPromptMessages :: [PromptMessage]  -- ^ Prompt messages
  } deriving (Show, Eq, Generic)

instance ToJSON GetPromptResult where
  toJSON GetPromptResult{..} = object $
    [ "messages" .= getPromptMessages
    ] ++
    [ "description" .= d | d <- maybeToList getPromptDescription ]

instance FromJSON GetPromptResult where
  parseJSON = withObject "GetPromptResult" $ \o -> do
    description <- o .:? "description"
    messages <- o .: "messages"
    return $ GetPromptResult description messages

-- | List roots request
data ListRootsRequest = ListRootsRequest
  deriving (Show, Eq, Generic)

instance ToJSON ListRootsRequest where
  toJSON _ = object []

instance FromJSON ListRootsRequest where
  parseJSON = withObject "ListRootsRequest" $ \_ ->
    return ListRootsRequest

-- | List roots result
data ListRootsResult = ListRootsResult
  { listRootsResult :: [Root]  -- ^ Available roots
  } deriving (Show, Eq, Generic)

instance ToJSON ListRootsResult where
  toJSON ListRootsResult{..} = object
    [ "roots" .= listRootsResult
    ]

instance FromJSON ListRootsResult where
  parseJSON = withObject "ListRootsResult" $ \o -> do
    roots <- o .: "roots"
    return $ ListRootsResult roots

-- | Resources list changed notification
data ResourcesListChangedNotification = ResourcesListChangedNotification
  deriving (Show, Eq, Generic)

instance ToJSON ResourcesListChangedNotification where
  toJSON _ = object []

instance FromJSON ResourcesListChangedNotification where
  parseJSON = withObject "ResourcesListChangedNotification" $ \_ ->
    return ResourcesListChangedNotification

-- | Resource updated notification
data ResourceUpdatedNotification = ResourceUpdatedNotification
  { resourceUpdatedUri :: Text  -- ^ URI of the updated resource
  } deriving (Show, Eq, Generic)

instance ToJSON ResourceUpdatedNotification where
  toJSON ResourceUpdatedNotification{..} = object
    [ "uri" .= resourceUpdatedUri
    ]

instance FromJSON ResourceUpdatedNotification where
  parseJSON = withObject "ResourceUpdatedNotification" $ \o -> do
    uri <- o .: "uri"
    return $ ResourceUpdatedNotification uri

-- | Tools list changed notification
data ToolsListChangedNotification = ToolsListChangedNotification
  deriving (Show, Eq, Generic)

instance ToJSON ToolsListChangedNotification where
  toJSON _ = object []

instance FromJSON ToolsListChangedNotification where
  parseJSON = withObject "ToolsListChangedNotification" $ \_ ->
    return ToolsListChangedNotification

-- | Prompts list changed notification
data PromptsListChangedNotification = PromptsListChangedNotification
  deriving (Show, Eq, Generic)

instance ToJSON PromptsListChangedNotification where
  toJSON _ = object []

instance FromJSON PromptsListChangedNotification where
  parseJSON = withObject "PromptsListChangedNotification" $ \_ ->
    return PromptsListChangedNotification

-- Helper function
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

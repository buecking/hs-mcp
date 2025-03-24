{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson
import Data.Text (Text)
import Network.MCP.Server
import Network.MCP.Server.Types
import Network.MCP.Transport.Types
import Network.MCP.Types
import System.Exit
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts + failures counts > 0
    then exitFailure
    else exitSuccess

allTests :: Test
allTests = TestList
  [ TestLabel "JSON Serialization Tests" jsonTests
  , TestLabel "Protocol Tests" protocolTests
  ]

-- JSON Serialization Tests
jsonTests :: Test
jsonTests = TestList
  [ TestLabel "Protocol Version" testProtocolVersion
  , TestLabel "Server Info" testServerInfo
  , TestLabel "Client Info" testClientInfo
  , TestLabel "Resource" testResource
  , TestLabel "Tool" testTool
  , TestLabel "Prompt" testPrompt
  , TestLabel "Request/Response" testRequests
  ]

testProtocolVersion :: Test
testProtocolVersion = TestCase $ do
  let version = ProtocolVersion 0 1
      encoded = encode version
      decoded = decode encoded :: Maybe ProtocolVersion

  assertEqual "Protocol version should encode and decode correctly" (Just version) decoded

testServerInfo :: Test
testServerInfo = TestCase $ do
  let info = Implementation "test-server" "1.0.0"
      encoded = encode info
      decoded = decode encoded :: Maybe Implementation

  assertEqual "Server info should encode and decode correctly" (Just info) decoded

testClientInfo :: Test
testClientInfo = TestCase $ do
  let info = Implementation "test-client" "1.0.0"
      encoded = encode info
      decoded = decode encoded :: Maybe Implementation

  assertEqual "Client info should encode and decode correctly" (Just info) decoded

testResource :: Test
testResource = TestCase $ do
  let resource = Resource
        { resourceUri = "file:///test/resource"
        , resourceName = "Test Resource"
        , resourceDescription = Just "A test resource"
        , resourceMimeType = Just "text/plain"
        , resourceTemplate = Nothing
        }
      encoded = encode resource
      decoded = decode encoded :: Maybe Resource

  assertEqual "Resource should encode and decode correctly" (Just resource) decoded

testTool :: Test
testTool = TestCase $ do
  let tool = Tool
        { toolName = "test-tool"
        , toolDescription = Just "A test tool"
        , toolInputSchema = "{\"type\":\"object\",\"properties\":{\"param\":{\"type\":\"string\"}}}"
        }
      encoded = encode tool
      decoded = decode encoded :: Maybe Tool

  assertEqual "Tool should encode and decode correctly" (Just tool) decoded

testPrompt :: Test
testPrompt = TestCase $ do
  let promptArg = PromptArgument
        { promptArgumentName = "testArg"
        , promptArgumentDescription = Just "A test argument"
        , promptArgumentRequired = True
        }
      prompt = Prompt
        { promptName = "test-prompt"
        , promptDescription = Just "A test prompt"
        , promptArguments = [promptArg]
        }
      encoded = encode prompt
      decoded = decode encoded :: Maybe Prompt

  assertEqual "Prompt should encode and decode correctly" (Just prompt) decoded

testRequests :: Test
testRequests = TestList
  [ TestLabel "Initialize Request" testInitializeRequest
  , TestLabel "List Resources Request/Result" testListResourcesRequestResult
  , TestLabel "Read Resource Request/Result" testReadResourceRequestResult
  , TestLabel "Call Tool Request/Result" testCallToolRequestResult
  ]

testInitializeRequest :: Test
testInitializeRequest = TestCase $ do
  let clientOpts = ClientInitializeOptions
        { clientInitProtocolVersion = ProtocolVersion 0 1
        , clientInitInfo = Implementation "test-client" "1.0.0"
        , clientInitCapabilities = ClientCapabilities Nothing Nothing
        }
      encoded = encode clientOpts
      decoded = decode encoded :: Maybe ClientInitializeOptions

  assertEqual "Initialize options should encode and decode correctly" (Just clientOpts) decoded

testListResourcesRequestResult :: Test
testListResourcesRequestResult = TestCase $ do
  let request = ListResourcesRequest
      resource = Resource
        { resourceUri = "file:///test/resource"
        , resourceName = "Test Resource"
        , resourceDescription = Just "A test resource"
        , resourceMimeType = Just "text/plain"
        , resourceTemplate = Nothing
        }
      result = ListResourcesResult [resource]
      encodedRequest = encode request
      encodedResult = encode result
      decodedRequest = decode encodedRequest :: Maybe ListResourcesRequest
      decodedResult = decode encodedResult :: Maybe ListResourcesResult

  assertEqual "List resources request should encode and decode correctly" (Just request) decodedRequest
  assertEqual "List resources result should encode and decode correctly" (Just result) decodedResult

testReadResourceRequestResult :: Test
testReadResourceRequestResult = TestCase $ do
  let request = ReadResourceRequest "file:///test/resource"
      content = ResourceContent
        { resourceContentUri = "file:///test/resource"
        , resourceContentMimeType = Just "text/plain"
        , resourceContentText = Just "Test content"
        , resourceContentBlob = Nothing
        }
      result = ReadResourceResult [content]
      encodedRequest = encode request
      encodedResult = encode result
      decodedRequest = decode encodedRequest :: Maybe ReadResourceRequest
      decodedResult = decode encodedResult :: Maybe ReadResourceResult

  assertEqual "Read resource request should encode and decode correctly" (Just request) decodedRequest
  assertEqual "Read resource result should encode and decode correctly" (Just result) decodedResult

testCallToolRequestResult :: Test
testCallToolRequestResult = TestCase $ do
  let request = CallToolRequest "test-tool" (Map.singleton "param" (String "value"))
      content = ToolContent
        { toolContentType = TextualContent
        , toolContentText = Just "Test result"
        }
      result = CallToolResult [content] False
      encodedRequest = encode request
      encodedResult = encode result
      decodedRequest = decode encodedRequest :: Maybe CallToolRequest
      decodedResult = decode encodedResult :: Maybe CallToolResult

  assertEqual "Call tool request should encode and decode correctly" (Just request) decodedRequest
  assertEqual "Call tool result should encode and decode correctly" (Just result) decodedResult

-- Protocol Tests
protocolTests :: Test
protocolTests = TestList
  [ TestLabel "Server Creation" testServerCreation
  , TestLabel "Resource Registration" testResourceRegistration
  , TestLabel "Tool Registration" testToolRegistration
  , TestLabel "Prompt Registration" testPromptRegistration
  ]

testServerCreation :: Test
testServerCreation = TestCase $ do
  let serverImp = Implementation "test-server" "1.0.0"
      serverCap = ServerCapabilities
        { resourcesCapability = Just $ ResourcesCapability True
        , toolsCapability = Just $ ToolsCapability True
        , promptsCapability = Just $ PromptsCapability True
        }

  server <- createServer serverImp serverCap

  assertEqual "Server name should match" "test-server" (serverName $ serverInfo server)
  assertEqual "Server version should match" "1.0.0" (serverVersion $ serverInfo server)

  let caps = serverCapabilities server
  assertEqual "Resources capability should be enabled"
             (Just $ ResourcesCapability True)
             (resourcesCapability caps)
  assertEqual "Tools capability should be enabled"
             (Just $ ToolsCapability True)
             (toolsCapability caps)
  assertEqual "Prompts capability should be enabled"
             (Just $ PromptsCapability True)
             (promptsCapability caps)

testResourceRegistration :: Test
testResourceRegistration = TestCase $ do
  let serverInfo = Implementation "test-server" "1.0.0"
      serverCapabilities = ServerCapabilities
        { resourcesCapability = Just $ ResourcesCapability True
        , toolsCapability = Nothing
        , promptsCapability = Nothing
        }
      resource = Resource
        { resourceUri = "file:///test/resource"
        , resourceName = "Test Resource"
        , resourceDescription = Just "A test resource"
        , resourceMimeType = Just "text/plain"
        , resourceTemplate = Nothing
        }

  server <- createServer serverInfo serverCapabilities
  registerResources server [resource]

  resources <- readTVarIO (serverResources server)
  assertEqual "Resources should be registered" 1 (length resources)
  assertEqual "Resource URI should match" "file:///test/resource" (resourceUri $ head resources)

testToolRegistration :: Test
testToolRegistration = TestCase $ do
  let serverInfo = Implementation "test-server" "1.0.0"
      serverCapabilities = ServerCapabilities
        { resourcesCapability = Nothing
        , toolsCapability = Just $ ToolsCapability True
        , promptsCapability = Nothing
        }
      tool = Tool
        { toolName = "test-tool"
        , toolDescription = Just "A test tool"
        , toolInputSchema = "{\"type\":\"object\",\"properties\":{\"param\":{\"type\":\"string\"}}}"
        }

  server <- createServer serverInfo serverCapabilities
  registerTools server [tool]

  tools <- readTVarIO (serverTools server)
  assertEqual "Tools should be registered" 1 (length tools)
  assertEqual "Tool name should match" "test-tool" (toolName $ head tools)

testPromptRegistration :: Test
testPromptRegistration = TestCase $ do
  let serverInfo = Implementation "test-server" "1.0.0"
      serverCapabilities = ServerCapabilities
        { resourcesCapability = Nothing
        , toolsCapability = Nothing
        , promptsCapability = Just $ PromptsCapability True
        }
      promptArg = PromptArgument
        { promptArgumentName = "testArg"
        , promptArgumentDescription = Just "A test argument"
        , promptArgumentRequired = True
        }
      prompt = Prompt
        { promptName = "test-prompt"
        , promptDescription = Just "A test prompt"
        , promptArguments = [promptArg]
        }

  server <- createServer serverInfo serverCapabilities
  registerPrompts server [prompt]

  prompts <- readTVarIO (serverPrompts server)
  assertEqual "Prompts should be registered" 1 (length prompts)
  assertEqual "Prompt name should match" "test-prompt" (promptName $ head prompts)

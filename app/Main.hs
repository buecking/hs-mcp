{-# LANGUAGE OverloadedStrings #-}

module AppMain where

import Data.Aeson (Value(..))
import Network.MCP.Server
-- import System.IO (hPutStrLn, stderr)
import Network.MCP.Server.StdIO
import Network.MCP.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- A simple echo server for demonstration purposes
main :: IO ()
main = do

  -- Using stderr for  MCP may be a mistake
  -- hPutStrLn stderr "Starting MCP Echo Server..."

  -- Create server with basic capabilities
  let serverInfo = Implementation
        { serverName = "echo-server"
        , serverVersion = "1.0.0"
        }
      serverCapabilities = ServerCapabilities
        { resourcesCapability = Just $ ResourcesCapability True
        , toolsCapability = Just $ ToolsCapability True
        , promptsCapability = Just $ PromptsCapability True
        }

  -- Create server instance
  server <- createServer serverInfo serverCapabilities

  -- Register resources
  let textResource = Resource
        { resourceUri = "echo://text"
        , resourceName = "Echo Text"
        , resourceDescription = Just "Echoes text back"
        , resourceMimeType = Just "text/plain"
        , resourceTemplate = Nothing
        }
      paramResource = Resource
        { resourceUri = "echo://param/{text}"
        , resourceName = "Echo With Parameter"
        , resourceDescription = Just "Echoes the parameter back"
        , resourceMimeType = Just "text/plain"
        , resourceTemplate = Just "{\"text\":\"string\"}"
        }

  registerResources server [textResource, paramResource]

  -- Register resource read handler
  registerResourceReadHandler server $ \request -> do
    let uri = resourceReadUri request

    -- Generate appropriate response based on URI
    content <- case T.stripPrefix "echo://param/" uri of
      Just param -> return $ "Echo parameter: " `T.append` param
      Nothing -> return $ "Echo response for: " `T.append` uri

    return $ ReadResourceResult
      [ ResourceContent
        { resourceContentUri = uri
        , resourceContentMimeType = Just "text/plain"
        , resourceContentText = Just content
        , resourceContentBlob = Nothing
        }
      ]

  -- Register tools
  let echoTool = Tool
        { toolName = "echo"
        , toolDescription = Just "Echoes text back"
        , toolInputSchema = "{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}},\"required\":[\"text\"]}"
        }
      calculatorTool = Tool
        { toolName = "calculator"
        , toolDescription = Just "Performs basic arithmetic"
        , toolInputSchema = "{\"type\":\"object\",\"properties\":{\"operation\":{\"type\":\"string\",\"enum\":[\"add\",\"subtract\",\"multiply\",\"divide\"]},\"a\":{\"type\":\"number\"},\"b\":{\"type\":\"number\"}},\"required\":[\"operation\",\"a\",\"b\"]}"
        }

  registerTools server [echoTool, calculatorTool]

  -- Register tool call handler
  registerToolCallHandler server $ \request -> do
    let name = callToolName request
        args = callToolArguments request

    -- Handle different tools
    result <- case name of
      "echo" -> do
        let text = case Map.lookup "text" args of
                     Just (String t) -> t
                     _ -> "No input provided"
        return $ "Echo: " `T.append` text

      "calculator" -> do
        let operation = case Map.lookup "operation" args of
                          Just (String op) -> op
                          _ -> "unknown"
            a = case Map.lookup "a" args of
                  Just (Number n) -> n
                  _ -> 0
            b = case Map.lookup "b" args of
                  Just (Number n) -> n
                  _ -> 0

            result = case operation of
                      "add" -> a + b
                      "subtract" -> a - b
                      "multiply" -> a * b
                      "divide" -> if b /= 0 then a / b else 0
                      _ -> 0

        return $ T.pack $ "Result: " ++ show result

      _ -> return $ "Unknown tool: " `T.append` name

    return $ CallToolResult
      [ ToolContent
        { toolContentType = TextualContent
        , toolContentText = Just result
        }
      ]
      False

  -- Register prompts
  let greetingPrompt = Prompt
        { promptName = "greeting"
        , promptDescription = Just "Generate a greeting"
        , promptArguments =
            [ PromptArgument
              { promptArgumentName = "name"
              , promptArgumentDescription = Just "Person's name"
              , promptArgumentRequired = True
              }
            ]
        }

  registerPrompts server [greetingPrompt]

  -- Register prompt handler
  registerPromptHandler server $ \request -> do
    let name = getPromptName request
        args = getPromptArguments request

    case name of
      "greeting" -> do
        let personName = Map.findWithDefault "friend" "name" args

        return $ GetPromptResult
          { getPromptDescription = Just $ "Greeting for " `T.append` personName
          , getPromptMessages =
              [ PromptMessage
                { promptMessageRole = "user"
                , promptMessageContent = PromptContent
                  { promptContentType = TextPromptContent
                  , promptContentText = "Write a friendly greeting for " `T.append` personName
                  }
                }
              ]
          }

      _ -> return $ GetPromptResult
        { getPromptDescription = Just "Unknown prompt"
        , getPromptMessages = []
        }

  -- Start the server with StdIO transport
  -- hPutStrLn stderr "Server initialized, waiting for connections..."
  runServerWithSTDIO server

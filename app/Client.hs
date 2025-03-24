{-# LANGUAGE OverloadedStrings #-}

module AppClient where

import Data.Aeson
import Network.MCP.Client
import Network.MCP.Client.Types
--  import Network.MCP.Types
import qualified Data.Map.Strict as Map
-- import qualified Data.Text as T

main :: IO ()
main = do
    -- Create client configuration
    let config = ClientConfig
            { clientName = "example-client"
            , clientVersion = "1.0.0"
            , clientCapabilities = object
                [ "resources" .= object ["listChanged" .= True]
                , "tools" .= object ["listChanged" .= True]
                , "prompts" .= object ["listChanged" .= True]
                ]
            }

    -- Create and connect client
    client <- createClient config
    _ <- connectClient client "./dist-newstyle/build/x86_64-linux/ghc-9.6.6/hs-mcp-0.1.0.0/x/mcp-echo-server/build/mcp-echo-server/mcp-echo-server" []

    -- List available tools
    tools <- listTools client
    putStrLn "Available Tools:"
    mapM_ print tools

    -- Call echo tool
    echoResult <- callTool client "echo"
        (Map.fromList [("text", String "Hello, MCP!")])
    putStrLn "Echo Result:"
    print echoResult

    -- Call calculator tool
    calcResult <- callTool client "calculator"
        (Map.fromList
            [ ("operation", String "add")
            , ("a", Number 5)
            , ("b", Number 3)
            ])
    putStrLn "Calculator Result:"
    print calcResult

    -- List resources
    resources <- listResources client
    putStrLn "Available Resources:"
    mapM_ print resources

    -- Read a resource
    resourceContent <- readResource client "echo://text"
    putStrLn "Resource Content:"
    print resourceContent

    -- List prompts
    prompts <- listPrompts client
    putStrLn "Available Prompts:"
    mapM_ print prompts

    -- Get greeting prompt
    greetingPrompt <- getPrompt client "greeting"
        (Map.fromList [("name", String "MCP User")])
    putStrLn "Greeting Prompt:"
    print greetingPrompt

    -- Disconnect
    disconnectClient client

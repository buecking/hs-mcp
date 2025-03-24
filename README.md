# MCP-Haskell (hs-mcp)

A Haskell implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/).

## Overview

MCP-Haskell (hs-mcp) provides a Haskell implementation of the Model Context Protocol, allowing Haskell applications to expose tools, resources, and prompts to MCP-compatible clients like Claude.

Key features:

- Full implementation of MCP protocol
- StdIO transport for local process communication
- JSON-RPC messaging
- Support for resources, tools, and prompts
- Comprehensive test suite

## Installation

```bash
# Clone the repository
git clone github.com:buecking/hs-mcp.git
cd hs-mcp

# direnv
# echo 'use flake' > .envrc
direnv allow

# nix
nix develop

# Build the project
cabal build
```

## Usage

### Creating a simple server

```haskell
import Network.MCP.Server
import Network.MCP.Types
import Network.MCP.Server.StdIO

main :: IO ()
main = do
  -- Create server
  let serverInfo = Implementation "my-server" "1.0.0"
      serverCapabilities = ServerCapabilities
        { resourcesCapability = Just $ ResourcesCapability True
        , toolsCapability = Just $ ToolsCapability True
        , promptsCapability = Nothing
        }
  
  server <- createServer serverInfo serverCapabilities
  
  -- Register resources (optional)
  let resource = Resource 
        { resourceUri = "my://resource"
        , resourceName = "My Resource"
        , resourceDescription = Just "Description"
        , resourceMimeType = Just "text/plain"
        , resourceTemplate = Nothing
        }
  
  registerResources server [resource]
  
  -- Register resource read handler
  registerResourceReadHandler server $ \request -> do
    -- Implement resource reading logic
    ...

  -- Register tools (optional)
  let tool = Tool 
        { toolName = "my-tool"
        , toolDescription = Just "My tool"
        , toolInputSchema = "{...}" -- JSON schema
        }
  
  registerTools server [tool]
  
  -- Register tool call handler
  registerToolCallHandler server $ \request -> do
    -- Implement tool execution logic
    ...
  
  -- Start the server with StdIO transport
  runServerWithSTDIO server
```

### Example Server

The project includes an example echo server that demonstrates the MCP functionality:

```bash
# Build and run the example server
cabal run mcp-echo-server
```

You can test it with the [MCP Inspector](https://github.com/modelcontextprotocol/inspector) or Claude Desktop.

## Testing

Run the test suite:

```bash
cabal test
```

## Protocol Compatibility

This implementation follows the [Model Context Protocol specification](https://spec.modelcontextprotocol.io/) and is compatible with:

- Claude Desktop
- MCP Inspector
- Other MCP clients following the specification

## Project Structure

- `src/Network/MCP/Types.hs` - Core MCP types
- `src/Network/MCP/Transport/` - Transport implementations
- `src/Network/MCP/Server/` - Server implementation
- `Examples/` - Example implementations
- `Test/` - Test suite

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the BSD-3-Clause License - see the LICENSE file for details.

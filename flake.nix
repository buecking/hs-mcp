{
  description = "Haskell implementation of Model Context Protocol (MCP)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellOverlay = final: prev: {
          haskellPackages = prev.haskellPackages.override {
            overrides = hfinal: hprev: {
              hs-mcp = hfinal.callCabal2nix "hs-mcp" ./. {};
            };
          };
        };
        
        overlaidPkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellOverlay ];
        };
      in {
        packages.hs-mcp = overlaidPkgs.haskellPackages.hs-mcp;
        packages.default = self.packages.${system}.hs-mcp;

        devShells.default = overlaidPkgs.haskellPackages.shellFor {
          packages = p: [ p.hs-mcp ];
          buildInputs = with overlaidPkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.hoogle
            haskellPackages.cabal-fmt
            haskellPackages.fourmolu
            # Non-Haskell tools
            nixpkgs-fmt
          ];
          # Make external Nix libraries like zlib available to cabal
          withPackages = ps: with ps; [
            zlib
          ];
          # Enables Hoogle with all the packages in the environment
          withHoogle = true;
        };

        apps.hs-mcp = flake-utils.lib.mkApp {
          drv = self.packages.${system}.hs-mcp;
          name = "mcp-echo-server";
        };
        apps.default = self.apps.${system}.hs-mcp;

        # For compatibility with older Nix versions
        devShell = self.devShells.${system}.default;
      }
    );
}

{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell
            ghc cabal-install haskell-language-server

            # Project
            just haskellPackages.fourmolu haskellPackages.implicit-hie

            zlib
          ];
        };
      }
    );
}

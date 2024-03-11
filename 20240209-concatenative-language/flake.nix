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
            ghc
            cabal-install
            haskell-language-server
            (haskell.lib.justStaticExecutables haskellPackages.fourmolu)
            
            (agda.withPackages [ agdaPackages.standard-library ])
          ];
        };
      }
    );
}

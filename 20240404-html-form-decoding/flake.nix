{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
        ghcVersion = "964";
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            (haskellPackages.ghcWithPackages (p: [
              p.barbies
              p.comonad
              p.contravariant
              p.monoidal-containers
              p.mtl
            ]))
            cabal-install
            haskell-language-server
          ];
        };
      }
    );
}

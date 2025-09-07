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
            haskellPackages.ghc
            cabal-install
            haskell-language-server

            just
            haskellPackages.fourmolu
            haskellPackages.implicit-hie
            fd

            # gi-gtk4
            pkg-config
            gtk4
            gobject-introspection
            libsysprof-capture
            pcre2
            expat
            xorg.libXdmcp
            util-linux
            libselinux
            libsepol
            fribidi
            libthai
            libdeflate
            lerc
            libdatrie
            xz # liblzma
            zstd
            libwebp
          ];
        };
      }
    );
}

{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };
  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            rust-overlay.overlays.default
          ];
        };

        rustVersion = "1.81.0";

      in {
        devShell =
          pkgs.mkShell {
            LIBX11_MAN = "${pkgs.xorg.libX11.man}/share/man";
            XORG_MAN = "${pkgs.xorg.xorgdocs}/share/man";

            # LD_LIBRARY_PATH = "${pkgs.xorg.libX11}/lib";
            DEV_LIBRARIES = "${pkgs.xorg.xorgproto}/include";

            buildInputs = [
              (pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override {
                extensions = [
                  "cargo"
                  "clippy"
                  "rustc"
                  "rust-src"
                  "rustfmt"
                  "rust-analyzer"
                ];

                targets = [
                  "x86_64-unknown-linux-gnu"
                  "x86_64-pc-windows-msvc"
                  "x86_64-apple-darwin"
                ];
              }))

              pkgs.xorg.libX11.dev
            ];
          };
      }
    );
}

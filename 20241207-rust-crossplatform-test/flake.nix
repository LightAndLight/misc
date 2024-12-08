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
            ];
          };
      }
    );
}

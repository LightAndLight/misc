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

        rustVersion = "1.86.0";

      in {
        devShell =
          pkgs.mkShell {
            buildInputs = with pkgs; [
              (rust-bin.stable.${rustVersion}.default.override {
                extensions = [
                  "cargo"
                  "clippy"
                  "rustc"
                  "rust-src"
                  "rustfmt"
                  "rust-analyzer"
                ];
              })

              pkg-config
              gtk4
              libadwaita
              librsvg
            ];
          };
      }
    );
}

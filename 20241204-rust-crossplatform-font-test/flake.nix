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
            # Fixes `DRM kernel driver 'nvidia-drm' in use. NVK requires nouveau.`
            # Is this a NixOS misconfiguration?
            #
            # Reference: <https://discourse.nixos.org/t/nvk-error-when-using-prop-nvidia-drivers/43300/5>
            VK_DRIVER_FILES="/run/opengl-driver/share/vulkan/icd.d/nvidia_icd.x86_64.json";
            
            shellHook = ''
              # https://github.com/rust-windowing/winit/issues/3603
              export LD_LIBRARY_PATH="$LD_LIBRARY_PATH":${with pkgs; lib.concatMapStringsSep ":" (x: "${x}/lib") [
                xorg.libX11
                xorg.libXcursor
                xorg.libxcb
                xorg.libXi
                libxkbcommon              
                vulkan-loader
              ]}
            '';

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
              fontconfig

              # Driver debugging
              vulkan-tools

              # GPU debugging
              renderdoc
            ];
          };
      }
    );
}

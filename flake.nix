{
  description = "clash-hash development shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, ... }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      forEachSystem = nixpkgs.lib.genAttrs systems;

      mkPackages = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnsupportedSystem = true;
          };
          pkgsUnstable = import nixpkgs-unstable {
            inherit system;
            config.allowUnsupportedSystem = true;
          };

          yosysPkg = pkgsUnstable.yosys;
          synthCli = pkgs.writeShellApplication {
            name = "synth";
            runtimeInputs = [ pkgs.python3 ];
            text = ''
              set -euo pipefail
              script_path="$PWD/scripts/synth_verilog.py"
              if [ ! -f "$script_path" ]; then
                echo "error: scripts/synth_verilog.py not found; run from the repository root" >&2
                exit 1
              fi
              exec ${pkgs.python3}/bin/python "$script_path" "$@"
            '';
          };
        in {
          inherit yosysPkg synthCli;
          default = yosysPkg;
        };
    in {
      packages = forEachSystem mkPackages;

      devShells = forEachSystem (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnsupportedSystem = true;
          };
          pkgSet = mkPackages system;
          gccShim = pkgs.writeShellScriptBin "gcc" ''
            exec ${pkgs.clang}/bin/clang "$@"
          '';
        in {
          default = pkgs.mkShell {
            packages = [
              pkgSet.yosysPkg
              pkgs.python3
              pkgSet.synthCli
              pkgs.clang
              pkgs.stack
              pkgs.pkg-config
              pkgs.git
              gccShim
            ];
            shellHook = ''
              export NANGATE45_LIB=$PWD/lib/nangate45/NangateOpenCellLibrary_typical.lib
              export CC=${pkgs.clang}/bin/clang
              export CXX=${pkgs.clang}/bin/clang++
              export MACOSX_DEPLOYMENT_TARGET=15.0
              export NIX_LDFLAGS="-Wl,-platform_version,macos,15.0,15.0 $NIX_LDFLAGS"
            '';
          };
        });

      apps = forEachSystem (system:
        let
          pkgSet = mkPackages system;
        in {
          synth = {
            type = "app";
            program = "${pkgSet.synthCli}/bin/synth";
          };
        });
    };
}

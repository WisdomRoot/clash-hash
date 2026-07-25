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
            runtimeInputs = [ pkgs.python3 yosysPkg ];
            text = ''
              set -euo pipefail
              script_path="$PWD/scripts/synth.py"
              if [ ! -f "$script_path" ]; then
                echo "error: scripts/synth.py not found; run from the repository root" >&2
                exit 1
              fi
              exec ${pkgs.python3}/bin/python "$script_path" "$@"
            '';
          };
          benchCli = pkgs.writeShellApplication {
            name = "bench";
            runtimeInputs = [ pkgs.python3 pkgs.stack yosysPkg synthCli ];
            text = ''
              set -euo pipefail
              script_path="$PWD/scripts/bench.py"
              if [ ! -f "$script_path" ]; then
                echo "error: scripts/bench.py not found; run from the repository root" >&2
                exit 1
              fi
              exec ${pkgs.python3}/bin/python "$script_path" "$@"
            '';
          };
          staCli = pkgs.writeShellApplication {
            name = "sta";
            runtimeInputs = [ pkgs.python3 yosysPkg ];
            text = ''
              set -euo pipefail
              script_path="$PWD/scripts/sta.py"
              if [ ! -f "$script_path" ]; then
                echo "error: scripts/sta.py not found; run from the repository root" >&2
                exit 1
              fi
              exec ${pkgs.python3}/bin/python "$script_path" "$@"
            '';
          };
        in {
          inherit yosysPkg synthCli benchCli staCli;
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
              pkgSet.benchCli
              pkgSet.staCli
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
              export NIX_LDFLAGS="-w $NIX_LDFLAGS"

              # Set KYBER_PY_PATH for external reference tests
              export KYBER_PY_PATH=$PWD/../kyber-py/src

              # Setup Python virtual environment for kyber-py
              if [ ! -d .venv ]; then
                echo "Creating Python virtual environment..."
                ${pkgs.python3}/bin/python -m venv .venv
                .venv/bin/pip install --quiet kyber-py
              fi
              source .venv/bin/activate
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
          bench = {
            type = "app";
            program = "${pkgSet.benchCli}/bin/bench";
          };
          sta = {
            type = "app";
            program = "${pkgSet.staCli}/bin/sta";
          };
        });
    };
}

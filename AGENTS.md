# HaskellNixCabalStarter: Developer Guide

## Commands
- **Check Compilation**: `./lint.sh` (reads compiler warnings/errors from the background build runner). ALWAYS use this to verify compilation. Do NOT run `cabal build` or `nix-shell --run "cabal build"` — use `./lint.sh` (with `bash ./lint.sh` if not executable).
- **Build**: `nix-build`
- **Run**: `nix-shell --run "cabal run"`
- **Enter dev shell**: `nix-shell`

## Build Configuration
- Do NOT modify `HaskellNixCabalStarter.cabal` directly -- it is auto-generated. Only modify `package.yaml` for dependency and build configuration changes.
- Do NOT search/grep inside `/nix/store`. Ask the user if nix-related source lookups are required.

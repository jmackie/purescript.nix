{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc864"
, returnShellEnv ? pkgs.lib.inNixShell
}:
let
  haskellPackages =
    pkgs.haskell.packages.${compiler}.override {
      overrides = self: super: {
      };
    };

  # ghcid with support for cabal new-repl
  ghcid_ =
    let newRepl = ''
      cabal new-repl \
        --ghc-options=-fno-break-on-exception \
        --ghc-options=-fno-break-on-error \
        --ghc-options=-v1 \
        --ghc-options=-ferror-spans \
        --ghc-options=-j
    '';
    # NOTE: not using `--ghc-options=-fno-code`
    # because we might want to pass `--run`
    in pkgs.writeShellScriptBin "ghcid_" ''
      ${haskellPackages.ghcid}/bin/ghcid \
        --restart elaborator.cabal \
        --command "${newRepl}" \
        --setup ":l Main" \
        "$@"
  '';

  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "ec5dd0536a5e4c3a99c797b86180f7261197c124";
    sha256 = "0k2r8y21rn4kr5dmddd3906x0733fs3bb8hzfpabkdav3wcy3klv";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

  name =
    "elaborator";

  src =
    gitignoreSource ./.;

  drv =
    haskellPackages.callCabal2nix name src {};

  env = haskellPackages.shellFor {
    packages = p: [ drv ];
    buildInputs = [
      haskellPackages.ghcid ghcid_
      haskellPackages.stylish-haskell
    ];
  };
in
if returnShellEnv then env else drv

# https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/generic-builder.nix
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix

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
        --ghc-options=-fno-code \
        --ghc-options=-fno-break-on-exception \
        --ghc-options=-fno-break-on-error \
        --ghc-options=-v1 \
        --ghc-options=-ferror-spans \
        --ghc-options=-j
    '';
    in pkgs.writeShellScriptBin "ghcid_" ''
      ${haskellPackages.ghcid}/bin/ghcid \
        --restart elaborator.cabal \
        --command "${newRepl}" \
        --setup ":l Main"
  '';
  name =
    "elaborator";

  src =
    pkgs.nix-gitignore.gitignoreSource [] ./.;

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

{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc864"
, returnShellEnv ? pkgs.lib.inNixShell
}:
let
  kesha =
   with builtins.fromJSON (builtins.readFile ./kesha-src.json);
    builtins.fetchTarball {
      inherit name sha256;
      url = "${url}/archive/${rev}.tar.gz";
    };

  haskellPackages =
    pkgs.haskell.packages.${compiler}.override {
      overrides = self: super: {
        kesha = super.callCabal2nix "kesha" kesha {};
      };
    };

  name =
    "elaborator";

  src =
    builtins.filterSource (path: _type:
      builtins.elem (builtins.baseNameOf path)  ["Main.hs" "Setup.hs" "elaborator.cabal"]) ./.;

  drv =
    haskellPackages.callCabal2nix name src {};

  env = haskellPackages.shellFor {
    packages = p: [ drv ];
    buildInputs = [];
  };
in
if returnShellEnv then env else drv

# https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/generic-builder.nix
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix

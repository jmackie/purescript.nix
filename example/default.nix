{ pkgs ? import <nixpkgs> {} }:
let
  purescript = import ./.. { inherit pkgs; };
  # purescript = import (pkgs.fetchFromGitHub {
  #   owner = "jmackie";
  #   repo = "purescript-nix";
  #   rev = "COMMIT";
  #   sha256 = "HASH";
  # }) { inherit pkgs; } ;

  dependencies = [
    "prelude"
    "console"
  ];

in
purescript.compile {
  name = "example";
  src = ./.;
  srcDirs = [
    "src"
  ];
  inherit dependencies;
  package-set = purescript.package-sets."psc-0.12.3-20190409";
}

{ pkgs ? import <nixpkgs> {} }:
let
  purescript = import ./.. { inherit pkgs; };
  # purescript = import (pkgs.fetchFromGitHub {
  #   owner = "jmackie";
  #   repo = "purescript-nix";
  #   rev = "COMMIT";
  #   sha256 = "HASH";
  # }) { inherit pkgs; } ;
in
purescript.compile {
  name = "example";
  src = ./.;
  srcDirs = [
    "src"
  ];
  dependencies = [
    "prelude"
    "console"
  ];
}

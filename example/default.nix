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
{
  project = purescript.compile {
    name = "example";
    src = ./.;
    srcDirs = [
      "src"
    ];
    inherit dependencies;
  };

  deps = purescript.dumpDependencies "example-deps" dependencies;
}

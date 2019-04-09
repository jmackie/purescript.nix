{ nixpkgs ? import <nixpkgs> {} }:
let
  purescript = import ../. {};
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

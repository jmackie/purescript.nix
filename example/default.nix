{ nixpkgs ? import <nixpkgs> {} }:
let
  purescript = import (nixpkgs.fetchFromGitHub {
    owner = "jmackie";
    repo = "purescript-nix";
    rev = "2d87289f22bc0f823033fb59c16df6009de09038";
    sha256 = "1ia36ssl642nkmnqg4iqz2jmmgnl7n3zcb3hrmq3ybgnv81ygr1k";
  }) { inherit nixpkgs; } ;
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

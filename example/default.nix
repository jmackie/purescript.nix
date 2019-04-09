{ nixpkgs ? import <nixpkgs> {} }:
let
  purescript = import (nixpkgs.fetchFromGitHub {
    owner = "jmackie";
    repo = "purescript-nix";
    rev = "03083c39259e5e120a227108337a20df047f4a67";
    sha256 = "0hsmhj6ivy74a7arwrviwszllccz4fkycgvfr7clj733jsjjzir2";
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

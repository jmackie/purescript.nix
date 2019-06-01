{ pkgs ? import <nixpkgs> {} }:
let
  purescript = import ./.. { purs = "v0.12.3"; inherit pkgs; };
  # purescript = import (pkgs.fetchFromGitHub {
  #   owner = "jmackie";
  #   repo = "purescript-nix";
  #   rev = "COMMIT";
  #   sha256 = "HASH";
  # }) { inherit pkgs; } ;

  # Here's how you can modify a package-set
  additions = {
    "selection-foldable" =  {
      dependencies = [
        "prelude"
        "maybe"
        "foldable-traversable"
        "filterable"
      ];
      repo = "https://github.com/jamieyung/purescript-selection-foldable";
      version = "v0.2.0";
      sha256 = "1ai4k1h3d6y305qvjngl51h9imrnq3dhqn2f6ghp3nmhjxwli5fy";
      sources = {
        "Data.SelectionFoldable" = "src/Data/SelectionFoldable.purs";
        "Data.SelectionFoldableWithData" = "src/Data/SelectionFoldableWithData.purs";
      };
      foreigns = [];
    };
  };

  package-set = purescript.package-sets."psc-0.12.3-20190409" // additions;

in
purescript.compile {
  name = "example";
  # https://github.com/siers/nix-gitignore#usage
  # See also --> https://github.com/hercules-ci/gitignore
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
  srcDirs = [
    "src"
  ];
  dependencies = [
    "prelude"
    "console"
    "selection-foldable"
  ];
  inherit package-set;
}

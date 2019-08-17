{ pkgs ? import <nixpkgs> {} }:
let
  jmackie =
    import (builtins.fetchTarball "https://github.com/jmackie/nur-packages/archive/master.tar.gz") { inherit pkgs; };

  purs = jmackie.purs."v0.13.2";

  purescript = import ./.. { inherit pkgs purs; };
  # purescript = import (pkgs.fetchFromGitHub {
  #   owner = "jmackie";
  #   repo = "purescript.nix";
  #   rev = "COMMIT";
  #   sha256 = "HASH";
  # }) { inherit pkgs; } ;

  package-set = purescript.loadPackageSet {
    url = "https://github.com/purescript/package-sets";
    rev = "psc-0.13.2-20190815";
    sha256 = "03qkgmrpam7573xinrm1c9m2jlpz2xmwzi09ajp0sppyb74bhsyy";
  };

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
in
purescript.compile {
  name = "example";
  # https://github.com/siers/nix-gitignore#usage
  # See also --> https://github.com/hercules-ci/gitignore
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
  srcDirs = [
    "src"
  ];

  package-set = package-set // additions;
  dependencies = [
    "prelude"
    "console"
    "selection-foldable"
  ];
}

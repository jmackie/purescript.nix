# PureScript <> Nix

Build your PureScript stuff with Nix.

## Getting started

```nix
# default.nix
{ nixpkgs ? import <nixpkgs> {} }:
let
  # $ nix-prefetch-git https://github.com/jmackie/purescript-nix
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
  package-set = purescript.package-sets."psc-0.12.3-20190427";
}
```

See the [example](https://github.com/jmackie/purescript-nix/tree/master/example).

## Building an "elaborated" package set

We can't use the [standard package sets](https://github.com/purescript/package-sets)
as they are because Nix needs to know what repo hash we expect. So there is a
little Haskell executable under [`elaborator`](https://github.com/jmackie/purescript-nix/tree/master/example)
that takes as input a standard package set and outputs that same package set
with commit hashes and stuff.

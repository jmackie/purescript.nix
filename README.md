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
    rev = "616e0ad7a14164b3f1fc26ecb9cead792b8e35f6";
    sha256 = "0b0c6g5f8bxw4q7zsqjk28n408akx8fzam6z1kxh46bmvc0hmdfi";
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

## What's the point?

- Caching

## Building an "elaborated" package set

We can't use the [standard package sets](https://github.com/purescript/package-sets)
as they are because Nix needs to know what repo hash we expect. So there is a
little Haskell executable under [`elaborator`](https://github.com/jmackie/purescript-nix/tree/master/example)
that takes as input a standard package set and outputs that same package set
with commit hashes and stuff.

## TODO

- [ ] Should probably start versioning this stuff
- [ ] Could actually invoke the `elaborator` through Nix...?
- [ ] More `purs` derivations, aligning with versions supported in `package-sets`

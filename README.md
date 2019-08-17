# PureScript <> Nix

Build your PureScript stuff with Nix.

## Getting started

TODO

See the [example](https://github.com/jmackie/purescript-nix/tree/master/example).

## What's the point?

- Caching

## Building an "elaborated" package set

We can't use the [standard package sets](https://github.com/purescript/package-sets)
as they are because Nix needs to know what repo hash we expect. So there is a
little Haskell executable under [`elaborator`](https://github.com/jmackie/purescript-nix/tree/master/elaborator)
that takes as input a standard package set and outputs that same package set
with commit hashes and stuff.

## TODO

- [ ] Should probably start versioning this stuff
- [ ] Could actually invoke the `elaborator` through Nix...?
- [ ] Fix shell stuff so `purs compile` doesn't rebuild everything
- [ ] Add CI testing

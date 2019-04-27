## Installing

```bash
nix-env -f https://github.com/jmackie/purescript-nix/archive/master.tar.gz -iA elaborator

elaborate-purescript-packages --help
```

## Using it

```bash
elaborate-purescript-packages <(curl https://raw.githubusercontent.com/purescript/package-sets/master/packages.json)
```

## Developing

You probably want to use the provided Nix shell

```bash
nix-shell
```

Once in the shell, there's a convenient `ghcid` wrapper

```bash
ghcid_
```

Or more succinctly

```bash
nix-shell --run ghcid_
```

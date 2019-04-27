```bash
cabal new-run elaborator -- --help
cabal new-run elaborator -- <(curl https://raw.githubusercontent.com/purescript/package-sets/master/packages.json)
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

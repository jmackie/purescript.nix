```bash
nix-build -j auto -A project
```

## Local development (TODO)

This doesn't work very well yet.

```bash
nix-build -A deps
cp -r --dereference --no-preserve=mode,timestamps result/src bower_components
cp -r --dereference --no-preserve=mode,timestamps result/output output
purs compile "src/**/*.purs" "bower_components/**/*.purs"
```

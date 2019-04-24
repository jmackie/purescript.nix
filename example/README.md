```bash
nix-build -j auto
```

## Local development

```bash
nix-shell -A env
purs compile $PURS_FILES 'src/**/*.purs'
purs docs --format html $PURS_FILES 'src/**/*.purs'
```

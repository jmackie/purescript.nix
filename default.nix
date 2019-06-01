{ pkgs ? import <nixpkgs> {}
, purs
}:
let
  compiler = (import ./purs.nix { inherit pkgs; }).${purs};

  getExtension = path: with builtins;
    let parts = pkgs.lib.splitString "." path;
    in elemAt parts (length parts - 1);

  isPurs = path: getExtension path == "purs";
  isJs   = path: getExtension path == "js";

  walkFiles = root: with builtins;
    concatLists (attrValues (mapAttrs (key: value:
      if value == "regular"
      then [(root + "/${key}")]
      else walkFiles (root + "/${key}")) (readDir root)));

  walkFilesRel = root: with builtins;
    map (path: substring (stringLength (toString root))
                         (stringLength (toString path))
                         (toString path))
        (walkFiles root);

  tsortDeps = package-set: deps:
    (pkgs.lib.toposort (a: b: builtins.elem a package-set.${b}.dependencies) deps).result;

  flattenDeps = package-set: with pkgs.lib;
    foldl' (accum: dep: unique (accum ++ [dep] ++ flattenDeps package-set package-set.${dep}.dependencies)) [];

  flattenCompiledDeps  = deps: builtins.attrValues (flattenCompiledDeps' deps);
  flattenCompiledDeps' = with pkgs.lib;
    foldl' (accum: dep: accum // { ${dep._args.name} = dep; } // flattenCompiledDeps' dep._args.dependencies) {};

  compilePackage =
    { name
    , src
    , dependencies
    , sources        # list of relative (stringy) paths
    , foreigns       # list of relative (stringy) paths
    } @ args :
    let
      deps = flattenCompiledDeps dependencies;
      depInputs = map (dep: map (f: "${dep.src}/${f}") dep._args.sources) deps;
      srcInputs = map (f: "${src}/${f}") sources;
      pursInputs = depInputs ++ srcInputs;
    in
    pkgs.stdenv.mkDerivation {
      name = "purescript-" + name;
      inherit src;
      buildInputs = [ compiler ];
      buildCommand = with builtins; ''
        mkdir -p $out

        ${if length deps == 0 then "" else ''
        # TODO: This would be more efficient if we could uniq on basename
        for outputDir in ${toString (map (dep: "${dep}/*") deps)}; do
          if [ ! -e "$out/$(basename $outputDir)" ]; then
            ln -s $outputDir $out
          fi
        done
        ''}

        purs compile --output $out ${toString pursInputs}
      '';

    } // {
      _args = args; # Need to hang on to these

      shell = pkgs.mkShell {
        # purs compile $PURS_FILES 'src/**/*.purs'
        # purs docs --format html $PURS_FILES 'src/**/*.purs'
        PURS_FILES = toString depInputs;

        buildInputs = [
          compiler
        ];

        shellHook = let for = xs: f: map f xs; in with builtins; ''
          purs-dump() {
            ${if length deps == 0 then "exit 0" else
              concatStringsSep "\n"
                (["echo Dumping dependency sources..."] ++
                (for deps (dep: ''
                  mkdir -p ./packages/${baseNameOf dep}

                  (HERE=$(pwd) && cd ${dep.src} && cp --parents --no-preserve=mode,timestamps \
                    ${toString (dep._args.sources ++ dep._args.foreigns)} $HERE/packages/${baseNameOf dep}/)

                  #tar cf - -C ${dep.src} ${toString (dep._args.sources ++ dep._args.foreigns)} | \
                  #  tar xf - --no-same-permissions -C ./packages/${baseNameOf dep}
                '')) ++

                ["echo Dumping outputs..."
                ''
                mkdir -p ./output
                for outputDir in ${toString (map (dep: "${dep}/*") deps)}; do
                  if [ ! -e "./output/$(basename $outputDir)" ]; then
                    cp -r --dereference --no-preserve=mode,timestamps $outputDir ./output/

                    #tar chf - -C $(dirname $outputDir) $(basename $outputDir) | tar xf - -C ./output
                  fi
                done
                ''
                ]
                )
            }
          }
        '';
      };
    };

  compileDependencies = package-set: deps:
    builtins.attrValues (
      builtins.foldl'
        (accum: name: let info = package-set.${name}; in accum // {
          ${name} = compilePackage {
            inherit name;
            src = pkgs.fetchgit {
              url = info.repo;
              rev = info.version;
              sha256 = info.sha256;
            };
            # deps is topologically sorted, so listed dependencies should
            # already be in accum
            dependencies = builtins.map (dep: accum.${dep}) info.dependencies;
            sources = builtins.attrValues info.sources;
            foreigns = info.foreigns;
          };
        })
        {}
        (tsortDeps package-set (flattenDeps package-set deps))
    );
in
{
  compile = { name, src, srcDirs, dependencies, package-set }:
    let
      inputs =
        builtins.concatMap
          (dir: map (f: dir + f) (walkFilesRel (src + "/${dir}")))
          srcDirs;
    in
    compilePackage {
        name = name;
        inherit src;
        dependencies = compileDependencies package-set dependencies;
        sources = builtins.filter isPurs inputs;
        foreigns = builtins.filter isJs inputs;
    };

  package-sets = with pkgs.lib;
    attrsets.mapAttrs'
      (name: value:
      attrsets.nameValuePair
        (strings.removeSuffix ".json" name)
        (builtins.fromJSON (builtins.readFile (./package-sets + "/${name}"))))
      (attrsets.filterAttrs
        (name: value: value != "directory" && strings.hasSuffix ".json" name)
        (builtins.readDir ./package-sets));

  elaborator = import ./elaborator { inherit pkgs; returnShellEnv = false; };
}

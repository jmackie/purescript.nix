{ pkgs }:
rec {
  # NOTE: We use `sha1` here as that's what purescript uses for releases
  # https://github.com/purescript/purescript/blob/master/.travis.yml
  # (See the default use of `shasum`, which uses the sha1 algorithm)
  fetch = { version, url, sha1 }:
    pkgs.stdenv.mkDerivation rec {
      name = "purs";
      inherit version;
      src = pkgs.fetchurl { inherit url sha1; };
      buildInputs = with pkgs; [
        zlib
        gmp
        ncurses5
      ];
      buildCommand = ''
        tar -zxf $src --strip-components 1 purescript/purs

        mkdir -p $out/bin
        PURS="$out/bin/purs"
        install -D -m555 -T purs $PURS

        ${if pkgs.stdenv.isDarwin then "" else ''
        chmod u+w $PURS
        patchelf --interpreter ${pkgs.stdenv.cc.bintools.dynamicLinker} \
          --set-rpath ${pkgs.lib.makeLibraryPath buildInputs} $PURS
        chmod u-w $PURS
        ''}
      '';
    };

  "v0.12.4" = let version = "v0.12.4"; in
    if pkgs.stdenv.isDarwin then
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/macos.tar.gz";
        sha1 = "4adbaf91ff54d18c001b9ebca05935ef15ba9ad7";
      }
    else
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/linux64.tar.gz";
        sha1 = "0d98444f82428c3e2c6918bae346d16ab9c381c6";
      };

  "v0.12.3" = let version = "v0.12.3"; in
    if pkgs.stdenv.isDarwin then
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/macos.tar.gz";
        sha1 = "f82522f89ae4a4e51da695ac958dbe77ef9be7e9";
      }
    else
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/linux64.tar.gz";
        sha1 = "f0c9fae808ae27973de7c77519f87ae6e4837312";
      };
}

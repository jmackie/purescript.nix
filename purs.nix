{ pkgs ? import <nixpkgs> {} }:
rec {
  # NOTE: We use `sha1` here as that's what purescript uses for releases
  # https://github.com/purescript/purescript/blob/master/.travis.yml
  # (See the default use of `shasum`, which uses the sha1 algorithm)
  fetch = { version, url, sha1 }:
    pkgs.stdenv.mkDerivation rec {
      name = "purs-${version}";
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

  # for url in https://github.com/purescript/purescript/releases/download/v0.12.{0..5}/{linux64,macos}.sha; do
  #     echo $url
  #     curl -s -L $url
  # done

  "v0.12.5" = let version = "v0.12.5"; in
    if pkgs.stdenv.isDarwin then
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/macos.tar.gz";
        sha1 = "926ab4ed98070f0847b866c1f9c676ce60ac22bc";
      }
    else
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/linux64.tar.gz";
        sha1 = "1969df7783f1e95b897f5b36ab1e12ab9cbc9166";
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

  "v0.12.2" = let version = "v0.12.2"; in
    if pkgs.stdenv.isDarwin then
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/macos.tar.gz";
        sha1 = "65bb346579538dd900504685b89bb58f13270cd3";
      }
    else
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/linux64.tar.gz";
        sha1 = "5075eced1436d4d5f7a47823f5c4333c1f1d3edc";
      };

  "v0.12.1" = let version = "v0.12.1"; in
    if pkgs.stdenv.isDarwin then
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/macos.tar.gz";
        sha1 = "f008364a7efb4d4e4b78ebc5558749536251f02e";
      }
    else
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/linux64.tar.gz";
        sha1 = "8b93532f0668a58c1c42b816c5fff11962fd8197";
      };

  "v0.12.0" = let version = "v0.12.0"; in
    if pkgs.stdenv.isDarwin then
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/macos.tar.gz";
        sha1 = "bd646189f4b3eb6cf0fa937db3dda020575769a7";
      }
    else
      fetch {
        inherit version;
        url = "https://github.com/purescript/purescript/releases/download/${version}/linux64.tar.gz";
        sha1 = "08d4839f2800a6fdb398ec45b7182eada112ea89";
      };
}

{ nixpkgs, version, url, sha256 }:

nixpkgs.stdenv.mkDerivation rec {
  name = "purs";
  inherit version;
  src = nixpkgs.fetchurl { inherit url sha256; };
  buildInputs = with nixpkgs; [
    zlib
    gmp
    ncurses5
  ];
  libPath = nixpkgs.lib.makeLibraryPath buildInputs;
  dontStrip = true;
  installPhase = ''
    mkdir -p $out/bin
    PURS="$out/bin/purs"
    install -D -m555 -T purs $PURS

    ${if nixpkgs.stdenv.isDarwin then "" else ''
    chmod u+w $PURS
    patchelf --interpreter ${nixpkgs.stdenv.cc.bintools.dynamicLinker} --set-rpath ${libPath} $PURS
    chmod u-w $PURS
    ''}
  '';
}

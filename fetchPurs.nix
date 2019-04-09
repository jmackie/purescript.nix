{ nixpkgs, version, url, sha256 }:

let
  dynamic-linker =
    nixpkgs.stdenv.cc.bintools.dynamicLinker;

  patchelf = libPath:
    if nixpkgs.stdenv.isDarwin then "" else ''
      chmod u+w $PURS
      patchelf --interpreter ${dynamic-linker} --set-rpath ${libPath} $PURS
      chmod u-w $PURS
    '';
in
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
    ${patchelf libPath}
  '';
}

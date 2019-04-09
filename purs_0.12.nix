{ nixpkgs }:
import ./fetchPurs.nix ({ inherit nixpkgs; version = "v0.12.3"; } // (
  if nixpkgs.stdenv.isDarwin then
    {
      url = "https://github.com/purescript/purescript/releases/download/v0.12.3/macos.tar.gz";
      sha256 = "1f916gv4fz571l4jvr15xjnsvjyy4nljv2ii9njwlm7k6yr5m0qn";
    }
  else
    {
      url = "https://github.com/purescript/purescript/releases/download/v0.12.3/linux64.tar.gz";
      sha256 = "1fad862a2sv4njxbbcfzibbi585m6is3ywb94nmjl8ax254baj3i";
    }
))

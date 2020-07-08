{ pkgs, mkDerivation, async, base, bytestring, containers, directory
, filepath, generic-random, hspec, lens, lens-aeson, lens-action, lens-properties, lens-regex-pcre, mtl
, QuickCheck, stdenv, stm, text, time
}:
mkDerivation {
  pname = "optics-by-example";
  version = "0.1.0.0";
  src = pkgs.lib.cleanSourceWith {
    src = ./.;
    filter = path: type:
      let baseName = baseNameOf path; in
      !( type == "directory"
         && builtins.elem baseName [".git" ".cabal-sandbox" "dist" "dist-newstyle"])
      &&
      !(    type == "unknown"
         || baseName == "cabal.sandbox.config"
         || baseName == "result"
         || baseName == "README.md"
         || baseName == ".gitignore"
         || baseName == "tags"
         || pkgs.stdenv.lib.hasSuffix ".hi" path
         || pkgs.stdenv.lib.hasSuffix ".o" path);
  };
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [
    base
    lens lens-aeson lens-action lens-regex-pcre
  ];
  libraryHaskellDepends = [
    async base bytestring containers directory filepath lens lens-aeson lens-action lens-regex-pcre
    mtl stm text time
  ];
  testHaskellDepends = [
    base generic-random hspec lens lens-aeson lens-regex-pcre lens-properties QuickCheck
  ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}

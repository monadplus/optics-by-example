{ pkgs, mkDerivation, async, base, bytestring, containers, directory
, filepath, generic-random, hspec, lens, lens-aeson, lens-properties, mtl
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
  libraryHaskellDepends = [
    async base bytestring containers directory filepath lens lens-aeson
    mtl stm text time
  ];
  testHaskellDepends = [
    base generic-random hspec lens lens-aeson lens-properties QuickCheck
  ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}

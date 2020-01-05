{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "ghc822"
, doBenchmark ? false
}:

let

  inherit (nixpkgs) pkgs;

  baseHaskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  readDirectory = import ./nix/readDirectory.nix;

  haskellPackages =
    let
      manualOverrides = haskellPackagesNew: haskellPackagesOld: {
        Diff =
          pkgs.haskell.lib.dontCheck haskellPackagesOld.Diff;
        aeson =
          pkgs.haskell.lib.addBuildDepend haskellPackagesOld.aeson haskellPackagesOld.contravariant;
      };
    in
      baseHaskellPackages.override {
        overrides = pkgs.lib.composeExtensions ( readDirectory ./nix/sources ) manualOverrides;
      };

  doBench  = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  doStatic = pkgs.haskell.lib.justStaticExecutables;
in
  doStatic (
    doBench (
      haskellPackages.callPackage ./optics-by-example.nix {}
    )
  )

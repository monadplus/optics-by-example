For more information about the book: https://leanpub.com/optics-by-example

### Development

```bash
$ nix-shell
shell> cabal configure && cabal build && cabal run

# You can add some options:
$ nix-shell --argstr compiler ghc864 --arg doBenchmark true
# On shell, all binaries will be compiled with ghc864
```

cabal2nix:

```bash
# To create the base nix file for your project
$ cabal2nix . > optics-by-example.nix

# There is a script to update your nix file
$ bash nix/update

# cabal2nix from sources
$ cabal2nix /path/to/turtle > turtle.nix

# cabal2nix from cabal
$ cabal2nix cabal://turtle-1.3.2 > ./nix/sources/turtle.nix

# cabal2nix from github
$ cabal2nix https://github.com/Gabriel439/Haskell-Turtle-Library.git --revision ba9c992933ae625cef40a88ea16ee857d1b93e13 > turtle-2.nix
```

If you want to use a newer version of a dependency or one that is not present in haskellPackages:

```bash
# Pick whatever dependency you like.
$ cabal2nix cabal://turtle-1.3.2 > ./nix/sources/turtle.nix
```

There is a little bit of magic on `default.nix`, take a look!

> It is possible that your new version doesn't work out of the box. Probably this new version depends on newer version of dependencies that are not present in the packages and you must also add them to your project.

If you have any problem compiling one of the dependencies, you must modify
`default.nix` (there are a couple of examples there). The list of available options can be found [here](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix).

### To compile the project

```bash
$ nix-build

# You can add some options:
$ nix-build --argstr compiler ghc864 --arg doBenchmark true
```

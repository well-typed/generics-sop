## Staged sums of products

`staged-sop` is a new variant of `generics-sop` employing staging
(via Typed Template Haskell) to allow defining generic functions
at a high level that perform equally fast as instances that are written
by hand.

The library is currently a prototype, as it requires a few extensions
to Typed Template Haskell that are not yet integrated into GHC, and
only implemented in a branch.

### Build instructions

You need the GHC branch at

https://gitlab.haskell.org/mpickering/ghc/-/tree/tth-with-constraints

The library and examples have been tested to build successfully with
commit [a5cb6991d1eb4479e862046301b0fd7cd2d578c3](https://gitlab.haskell.org/mpickering/ghc/-/commit/a5cb6991d1eb4479e862046301b0fd7cd2d578c3)
of that branch.

Build instructions if you are using Nix (note: this is not a necessity;
you can build GHC in some other way):
``` bash
/work $ git clone --recurse-submodules --single-branch --branch tth-with-constraints https://gitlab.haskell.org/mpickering/ghc.git
/work $ cd ghc
/work/ghc $ nix-shell https://github.com/alpmestan/ghc.nix/archive/master.tar.gz
[nix:ghc-shell-for-ghc-buildenv-8.9] /work/ghc $ ./boot
[nix:ghc-shell-for-ghc-buildenv-8.9] /work/ghc $ configure_ghc
[nix:ghc-shell-for-ghc-buildenv-8.9] /work/ghc $ hadrian/build -j
```
If successful, you'll then have `ghc` in `_build/stage1/bin/ghc`:
``` bash
[nix:ghc-shell-for-ghc-buildenv-8.9] /work/ghc $ _build/stage1/bin/ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.11.0.20200511
```

Make this compiler the default temporarily in your `PATH`, then build
`staged-sop` and `staged-sop-examples` in the root dir of this repo via:
``` bash
$ cabal v2-build --enable-bench staged-sop staged-sop-examples
```

You can run some benchmarks by saying:
``` bash
$ cabal v2-bench staged-sop-examples:bench:comparison
```


## Staged sums of products

[![Build Status](https://github.com/well-typed/generics-sop/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/well-typed/generics-sop/actions/workflows/haskell-ci.yml)
[![Hackage](https://img.shields.io/hackage/v/generics-sop.svg)](https://hackage.haskell.org/package/generics-sop)

`staged-sop` is a new variant of `generics-sop` employing staging
(via Typed Template Haskell) to allow defining generic functions
at a high level that perform equally fast as instances that are written
by hand.

The library is currently a prototype, as it requires a few extensions
to Typed Template Haskell that are not yet integrated into GHC, and
only implemented in a branch.

### Build instructions

You need the GHC branch at

https://gitlab.haskell.org/mpickering/ghc/-/tree/wip/typed-th-more-working

The library and examples have been tested to build successfully with
commit a5cb6991d1eb4479e862046301b0fd7cd2d578c3 of that branch.

Build instructions if you are using Nix, and if you have a checkout of
the GHC branch:
``` bash
$ nix-shell https://github.com/alpmestan/ghc.nix/archive/master.tar.gz
[nix:ghc-shell-for-ghc-buildenv-8.9] $ ./boot
[nix:ghc-shell-for-ghc-buildenv-8.9] $ configure_ghc
[nix:ghc-shell-for-ghc-buildenv-8.9] $ hadrian/build -j
```

If successful, you'll then have `ghc` in `_build/stage1/bin/ghc`:
``` bash
$ _build/stage1/bin/ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.11.0.20200511
```

Make this compiler the default temporarily in your `PATH`, then build
`staged-sop` and `staged-sop-examples`:
``` bash
$ cabal v2-build --enable-bench staged-sop staged-sop-examples
```

You can run some benchmarks by saying:
``` bash
$ cabal v2-bench staged-sop-examples:bench:comparison
```

 
Examples of using `generics-sop` are provided by the following
packages:
 
  * [basic-sop][2] basic examples,
  * [pretty-sop][3] generic pretty printing,
  * [lens-sop][4] generically computed lenses,
  * [json-sop][5] generic JSON conversions.
 
A detailed description of the ideas behind this library is provided by
the paper:
 
  * Edsko de Vries and Andres Löh.
    [True Sums of Products][6].
    Workshop on Generic Programming (WGP) 2014.
 
[0]: https://hackage.haskell.org/package/generics-sop
[1]: https://hackage.haskell.org/package/generics-sop/docs/Generics-SOP.html
[2]: https://github.com/well-typed/basic-sop
[3]: https://github.com/well-typed/pretty-sop
[4]: https://github.com/well-typed/lens-sop
[5]: https://github.com/well-typed/json-sop
[6]: http://www.andres-loeh.de/TrueSumsOfProducts

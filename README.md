## sop-core and generics-sop

[![Build Status](https://github.com/well-typed/generics-sop/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/well-typed/generics-sop/actions/workflows/haskell-ci.yml)
[![Hackage](https://img.shields.io/hackage/v/generics-sop.svg)](https://hackage.haskell.org/package/generics-sop)

`generics-sop` is a library to support the definition of generic functions.
Datatypes are viewed in a uniform, structured way: the choice between
constructors is represented using an n-ary sum, and the arguments of each
constructor are represented using an n-ary product.

Since version 0.4.0.0, `generics-sop` is now based on `sop-core`. The core
package contains all the functionality of n-ary sums and products, whereas
`generics-sop` provides the datatype-generic programming support on top.

This is the development repository for the packages. For releases, look on
[Hackage][0].
 
The module [`Generics.SOP`][1] is the main module of this library
and contains more detailed documentation.
 
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

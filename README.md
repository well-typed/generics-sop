A library to support the definition of generic functions.
Datatypes are viewed in a uniform, structured way: the choice
way: the choice between constructors is represented using an n-ary
sum, and the arguments of each constructor are represented using
an n-ary product.

This is the development repository. For releases, look on
[Hackage][0].
 
The module [`Generics.SOP`][1] is the main module of this library
and contains more detailed documentation.
 
Examples of using this library are provided by the following
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
 
[0]: https://hackage.haskell.org/packages/generics-sop
[1]: https://github.com/well-typed/generics-sop/blob/master/src/Generics/SOP.hs
[2]: https://github.com/well-typed/basic-sop
[3]: https://github.com/well-typed/pretty-sop
[4]: https://github.com/well-typed/lens-sop
[5]: https://github.com/well-typed/json-sop
[6]: http://www.andres-loeh.de/TrueSumsOfProducts

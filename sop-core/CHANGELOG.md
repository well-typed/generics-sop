# 0.5.0.1 (2020-03-29)

* Compatiblity with GHC-8.10 (thanks to Ryan Scott).

# 0.5.0.0 (2019-05-09)

* Add `ejections` that computes a product of functions
  that try to extract an element out of an n-ary sum.
  (See #91.)

* Change the definition of `SameShapeAs` to be
  non-recursive and thereby improve compiler performance.
  (See #105.)

# 0.4.0.0 (2018-10-20)

* Split into `sop-core` and `generics-sop` packages.

* Drop support for GHC < 8.0.2, bump `base` dependency
  to `>= 4.9` and remove dependency on `transformers`.

* Simplify `All2 c` to `All (All c)` and simplify
  `SListI xs` to `All Top xs`, and some implied
  refactoring.

* Add `Semigroup` and `Monoid` instances for various
  datatypes.

# 0.3.2.0 (2018-01-08)

* Make TH `deriveGenericFunctions` work properly with
  parameterized types (note that the more widely used
  `deriveGeneric` was already working correctly).

* Make TH `deriveGeneric` work properly with empty
  types.

* Add `compare_NS`, `ccompare_NS`, `compare_SOP`, and
  `ccompare_SOP` to better support comparison of sum
  structures.

* Add `hctraverse_` and `hctraverse'` as well as their
  unconstrained variants and a number of derived functions,
  to support effectful traversals.

# 0.3.1.0 (2017-06-11)

* Add `AllZip`, `htrans`, `hcoerce`, `hfromI`, `htoI`.
  These functions are for converting between related
  structures that do not have common signatures.

  The most common application of these functions seems
  to be the scenario where a datatype has components
  that are all wrapped in a common type constructor
  application, e.g. a datatype where every component
  is a `Maybe`. Then we can use `hfromI` after `from`
  to turn the generically derived `SOP` of `I`s into
  an `SOP` of `Maybe`s (and back).

* Add `IsProductType`, `IsEnumType`, `IsWrappedType`
  and `IsNewtype` constraint synonyms capturing
  specific classes of datypes.

# 0.3.0.0 (2017-04-29)

* No longer compatible with GHC 7.6, due to the lack of
  support for type-level literals.

* Support type-level metadata. This is provided by the
  `Generics.SOP.Type.Metadata` module. The two modules
  `Generics.SOP.Metadata` and `Generics.SOP.Type.Metadata`
  export nearly the same names, so for backwards compatibility,
  we keep exporting `Generics.SOP.Metadata` directly from
  `Generics.SOP`, whereas `Generics.SOP.Type.Metadata` is
  supposed to be imported explicitly (and qualified).

  Term-level metadata is still available, but is now usually
  computed automatically from the type-level metadata which
  contains the same information, using the function
  `demoteDatatypeInfo`. Term-level metadata is unchanged
  from generics-sop-0.2, so in most cases, even if your
  code makes use of metadata, you should not need to change
  anything.

  If you use TH deriving, then both type-level metadata and
  term-level metadata is generated for you automatically,
  for all supported GHC versions.

  If you use GGP deriving, then type-level metadata is
  available if you use GHC 8.0 or newer. If you use GHC 7.x,
  then GHC.Generics supports only term-level metadata, so
  we cannot translate that into type-level metadata. In
  this combination, you cannot use code that relies on
  type-level metadata, so you should either upgrade GHC or
  switch to TH-based deriving.

# 0.2.5.0 (2017-04-21)

* GHC 8.2 compatibility.

* Make `:.:` an instance of `Applicative`, `Foldable` and
  `Traversable`.

* Add functions `apInjs'_NP` and `apInjs'_POP`. These are
  variants of `apInjs_NP` and `apInjs'_POP` that return their
  result as an n-ary product, rather than collapsing it into
  a list.

* Add `hexpand` (and `expand_NS` and `expand_SOP`). These
  functions expand sums into products, given a default value
  to fill the other slots.

* Add utility functions such as `mapII` or `mapIK` that lift
  functions into different combinations of identity and
  constant functors.

* Add `NFData` (and lifted variants) instances for basic functors,
  products and sums.

# 0.2.4.0 (2017-02-02)

* Add `hindex` (and `index_NS` and `index_SOP`).

* Add `hapInjs` as a generalization of `apInjs_NP` and `apInjs_POP`.

* Make basic functors instances of lifted classes (such as `Eq1` etc).

# 0.2.3.0 (2016-12-04)

* Add various metadata getters

* Add `hdicts`.

* Add catamorphisms and anamorphisms for `NP` and `NS`.

* TH compatibility changes for GHC 8.1 (master).

# 0.2.2.0 (2016-07-10)

* Introduced `unZ` to destruct a unary sum.

* Add Haddock `@since` annotations for various functions.

# 0.2.1.0 (2016-02-08)

* Now includes a CHANGELOG.

* Should now work with ghc-8.0.1-rc1 and -rc2 (thanks to
  Oleg Grenrus).

* Introduced `hd` and `tl` to project out of a product, and
  `Projection` and `projections` as duals of `Injection` and
  `injections`.

# 0.2.0.0 (2015-10-23)

* Now tested with ghc-7.10

* Introduced names `hmap`, `hcmap`, `hzipWith`, `hczipWith` for
  `hliftA`, `hcliftA`, `hliftA2`, `hcliftA2`, respectively.
  Similarly for the specialized versions of these functions.

* The constraint transformers `All` and `All2` are now defined
  as type classes, not type families. As a consequence, the
  partial applications `All c` and `All2 c` are now possible.

* Because of the redefinition of `All` and `All2`, some special
  cases are no longer necessary. For example, `cpure_POP` can
  now be implemented as a nested application of `pure_NP`.

* Because of the redefinition of `All` and `All2`, the functions
  `hcliftA'` and variants (with prime!) are now deprecated.
  One can easily use the normal versions instead.
  For example, the definition of `hcliftA'` is now simply

      hcliftA' p = hcliftA (allP p)
        where
          allP :: proxy c -> Proxy (All c)
          allP _ = Proxy

* Because `All` and `All2` are now type classes, they now have
  superclass constraints implying that the type-level lists they
  are ranging over must have singletons.

      class (SListI xs,  ...) => All c xs
      class (SListI xss, ...) => All2 c xss

  Some type signatures can be simplified due to this.

* The `SingI` typeclass and `Sing` datatypes are now deprecated.
  The replacements are called `SListI` and `SList`.
  The `sing` method is now called `sList`. The difference
  is that the new versions reveal only the spine of the list, and
  contain no singleton representation for the elements anymore.

  For one-dimensional type-level lists, replace

      SingI xs => ...

  by

      SListI xs => ...

  For two-dimensional type-level lists, replace

      SingI xss => ...

  by

      All SListI xss => ...

  Because All itself implies `SListI xss` (see above), this
  constraint is equivalent to the old `Sing xss`.

  The old names are provided for (limited) backward
  compatibility. They map to the new constructs. This will
  work in some, but not all scenarios.

  The function `lengthSing` has also been renamed to
  `lengthSList` for consistency, and the old name is
  deprecated.

* All `Proxy c` arguments have been replaced by `proxy c`
  flexible arguments, so that other type constructors can be
  used as proxies.

* Class-level composition (`Compose`), pairing (`And`), and
  a trivial constraint (`Top`) have been added. Type-level map
  (`Map`) has been removed. Occurrences such as

      All c (Map f xs)

  should now be replaced with

      All (c `Compose` f) xs

* There is a new module called `Generics.SOP.Dict` that contains
  functions for manipulating dictionaries explicitly. These can
  be used to prove theorems about non-trivial class constraints
  such as the ones that get built using `All` and `All2`. Some
  such theorems are provided.

* There is a new TH function `deriveGenericFunctions` that
  derives the code of a datatype and conversion functions, but
  does not create a class instance. (Contributed by Oleg Grenrus.)

* There is a new TH function `deriveMetadataValue` that
  derives a `DatatypeInfo` value for a datatype, but does
  not create an instance of `HasDatatypeInfo`. (Contributed by
  Oleg Grenrus.)

* There is a very simple example file. (Contributed by Oleg
  Grenrus.)

* The function `hcollapse` for `NS` now results in an `a` rather
  than an `I a`, matching the specialized version `collapse_NS`.
  (Suggested by Roman Cheplyaka.)

# 0.1.1.2 (2015-03-27)

* Updated version bounds for ghc-prim (for ghc-7.10).

# 0.1.1.1 (2015-03-20)

* Preparations for ghc-7.10.

* Documentation fix. (Contributed by Roman Cheplyaka.)

# 0.1.1 (2015-01-06)

* Documentation fixes.

* Add superclass constraint (TODO).

* Now derive tuple instance for tuples up to 30 components.
  (Contributed by Michael Orlitzky.)


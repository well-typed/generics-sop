{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Generate @generics-sop@ boilerplate instances using Template Haskell.
module Generics.SOP.TH
  ( deriveGeneric
  , deriveGenericOnly
  , deriveGenericSubst
  , deriveGenericOnlySubst
  , deriveGenericFunctions
  , deriveMetadataValue
  , deriveMetadataType
  ) where

import Control.Monad (join, replicateM, unless, zipWithM)
import Data.Char (isAlpha, isAlphaNum, isUpper, ord, toUpper)
import qualified Data.Kind as Kind
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Proxy

-- importing in this order to avoid unused import warning
import Language.Haskell.TH.Datatype.TyVarBndr
import Language.Haskell.TH
import Language.Haskell.TH.Datatype as TH
import Language.Haskell.TH.Datatype.TyVarBndr

import Generics.SOP.BasicFunctors
import qualified Generics.SOP.Metadata as SOP
import qualified Generics.SOP.Type.Metadata as SOP.T
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Universe

-- | Generate @generics-sop@ boilerplate for the given datatype.
--
-- This function takes the name of a datatype and generates:
--
--   * a 'Code' instance
--   * a 'Generic' instance
--   * a 'HasDatatypeInfo' instance
--
-- Note that the generated code will require the @TypeFamilies@,
-- @DataKinds@ and @InstanceSigs@ extensions to be enabled for the module.
-- If the datatype is kind-polymorphic, @PolyKinds@ is required as well,
-- because the generated type synonyms then abstract over kinds.
--
-- Everything in the generated types that scales with the number of
-- constructors or the number of fields of the datatype is bound to a
-- type synonym, so that the types themselves stay small. The names of
-- these synonyms are derived from the names of the datatype, its
-- constructors and its fields, qualified by the module the datatype is
-- defined in. The qualification makes the names predictable; the code
-- additionally gets an unqualified alias, unless that name is taken.
--
-- /Example:/ If you have the datatype
--
-- > data Tree = Leaf Int | Node Tree Tree
--
-- and say
--
-- > deriveGeneric ''Tree
--
-- then you get code that is equivalent to:
--
-- > type MainTreeLeafCode = Int ': '[]
-- > type MainTreeNodeCode1 = Tree ': '[]
-- > type MainTreeNodeCode = Tree ': MainTreeNodeCode1
-- > type MainTreeCode1 = MainTreeNodeCode ': '[]
-- > type MainTreeCode = MainTreeLeafCode ': MainTreeCode1
-- > type TreeCode = MainTreeCode
-- >
-- > instance Generic Tree where
-- >
-- >   type Code Tree = TreeCode
-- >
-- >   from :: Tree -> SOP I TreeCode
-- >   from (Leaf x)   = SOP (   Z (I x :* Nil))
-- >   from (Node l r) = SOP (S (Z (I l :* I r :* Nil)))
-- >
-- >   to :: SOP I TreeCode -> Tree
-- >   to (SOP    (Z (I x :* Nil)))         = Leaf x
-- >   to (SOP (S (Z (I l :* I r :* Nil)))) = Node l r
-- >   to (SOP (S (S x)))                   = x `seq` error "inaccessible"
-- >
-- > type MainTreeConstructorLeaf = T.Constructor "Leaf"
-- > type MainTreeConstructorNode = T.Constructor "Node"
-- > type MainTreeConstructors1 = MainTreeConstructorNode ': '[]
-- > type MainTreeConstructors = MainTreeConstructorLeaf ': MainTreeConstructors1
-- >
-- > instance HasDatatypeInfo Tree where
-- >   type DatatypeInfoOf Tree =
-- >     T.ADT "Main" "Tree" MainTreeConstructors MainTreeStrictnesses
-- >
-- >   datatypeInfo _ =
-- >     T.demoteDatatypeInfo (Proxy :: Proxy (DatatypeInfoOf Tree))
--
-- (The synonyms for the strictness information are elided above; they
-- follow the same scheme.)
--
-- /Limitations:/ Generation does not work for GADTs, for
-- datatypes that involve existential quantification, for
-- datatypes with unboxed fields.
--
-- Because the generated type synonyms are top-level definitions, their
-- names can in principle still clash, in which case they are primed until
-- they are free. In particular, deriving for two instances of the same
-- data family in a single module will clash, as the names are derived from
-- the name of the data family.
--
deriveGeneric :: Name -> Q [Dec]
deriveGeneric n =
  deriveGenericSubst n varT

-- | Like 'deriveGeneric', but omit the 'HasDatatypeInfo' instance.
deriveGenericOnly :: Name -> Q [Dec]
deriveGenericOnly n =
  deriveGenericOnlySubst n varT

-- | Variant of 'deriveGeneric' that allows to restrict the type parameters.
--
-- Experimental function, exposed primarily for benchmarking.
--
deriveGenericSubst :: Name -> (Name -> Q Type) -> Q [Dec]
deriveGenericSubst n f = do
  dec <- reifyDatatype n
  ds1 <- withDataDec dec (deriveGenericForDataDec  f)
  ds2 <- withDataDec dec (deriveMetadataForDataDec f)
  return (ds1 ++ ds2)

-- | Variant of 'deriveGenericOnly' that allows to restrict the type parameters.
--
-- Experimental function, exposed primarily for benchmarking.
--
deriveGenericOnlySubst :: Name -> (Name -> Q Type) -> Q [Dec]
deriveGenericOnlySubst n f = do
  dec <- reifyDatatype n
  withDataDec dec (deriveGenericForDataDec f)

-- | Like 'deriveGenericOnly', but don't derive class instance, only functions.
--
-- /Example:/ If you say
--
-- > deriveGenericFunctions ''Tree "TreeCode" "fromTree" "toTree"
--
-- then you get code that is equivalent to:
--
-- > type MainTreeLeafCode = Int ': '[]
-- > type MainTreeNodeCode1 = Tree ': '[]
-- > type MainTreeNodeCode = Tree ': MainTreeNodeCode1
-- > type MainTreeCode1 = MainTreeNodeCode ': '[]
-- > type TreeCode = MainTreeLeafCode ': MainTreeCode1
-- >
-- > fromTree :: Tree -> SOP I TreeCode
-- > fromTree (Leaf x)   = SOP (   Z (I x :* Nil))
-- > fromTree (Node l r) = SOP (S (Z (I l :* I r :* Nil)))
-- >
-- > toTree :: SOP I TreeCode -> Tree
-- > toTree (SOP    (Z (I x :* Nil)))         = Leaf x
-- > toTree (SOP (S (Z (I l :* I r :* Nil)))) = Node l r
-- > toTree (SOP (S (S x)))                   = x `seq` error "inaccessible"
--
-- @since 0.2
--
deriveGenericFunctions :: Name -> String -> String -> String -> Q [Dec]
deriveGenericFunctions n codeName fromName toName = do
  let codeName' = mkName codeName
  let fromName' = mkName fromName
  let toName'   = mkName toName
  dec <- reifyDatatype n
  withDataDec dec $ \_variant _cxt name bndrs instTys cons -> do
    -- The code is bound to the synonym the caller asked for, so we only let
    -- 'codeFor' share its proper suffixes.
    (syns, codeType) <- withSharing (codeFor bndrs (typeBase name) varT cons)
    let origType = appTysSubst varT name instTys         -- Tree
    let repType  = [t| SOP I $(appTyVars varT codeName' bndrs) |] -- SOP I TreeCode
    decs <- sequence
      [ tySynD codeName' bndrs                          -- type TreeCode = TreeLeafCode ': TreeCode1
          (SigT codeType <$> [t| [[Kind.Type]] |])
      , sigD fromName' [t| $origType -> $repType |]     -- fromTree :: Tree -> SOP I TreeCode
      , embedding fromName' cons                        -- fromTree ... =
      , sigD toName' [t| $repType -> $origType |]       -- toTree :: SOP I TreeCode -> Tree
      , projection toName' cons                         -- toTree ... =
      ]
    return (syns ++ decs)

-- | Derive @DatatypeInfo@ value for the type.
--
-- /Example:/ If you say
--
-- > deriveMetadataValue ''Tree "TreeCode" "treeDatatypeInfo"
--
-- then you get code that is equivalent to:
--
-- > treeDatatypeInfo :: DatatypeInfo TreeCode
-- > treeDatatypeInfo = ADT "Main" "Tree"
-- >     (Constructor "Leaf" :* Constructor "Node" :* Nil)
--
-- /Note:/ CodeType needs to be derived with 'deriveGenericFunctions'.
--
-- @since 0.2
--
deriveMetadataValue :: Name -> String -> String -> Q [Dec]
deriveMetadataValue n codeName datatypeInfoName = do
  let codeName'  = mkName codeName
  let datatypeInfoName' = mkName datatypeInfoName
  dec <- reifyDatatype n
  withDataDec dec $ \variant _cxt name bndrs _instTys cons -> do
    sequence [ sigD datatypeInfoName' [t| SOP.DatatypeInfo $(appTyVars varT codeName' bndrs) |] -- treeDatatypeInfo :: DatatypeInfo TreeCode
             , funD datatypeInfoName' [clause [] (normalB $ metadata' variant name cons) []]    -- treeDatatypeInfo = ...
             ]
{-# DEPRECATED deriveMetadataValue "Use 'deriveMetadataType' and 'demoteDatatypeInfo' instead." #-}

-- | Derive @DatatypeInfo@ type for the type.
--
-- /Example:/ If you say
--
-- > deriveMetadataType ''Tree "TreeDatatypeInfo"
--
-- then you get code that is equivalent to:
--
-- > type TreeDatatypeInfo =
-- >   T.ADT "Main" "Tree"
-- >     [ T.Constructor "Leaf", T.Constructor "Node" ]
--
-- @since 0.3.0.0
--
deriveMetadataType :: Name -> String -> Q [Dec]
deriveMetadataType n datatypeInfoName = do
  let datatypeInfoName' = mkName datatypeInfoName
  dec <- reifyDatatype n
  withDataDec dec $ \ variant _ctx name bndrs _instTys cons -> do
    (syns, mdType) <-
      withSharing (metadataType' bndrs (typeBase name) variant name cons)
    syn <- tySynD datatypeInfoName' [] (return mdType)
    return (syns ++ [syn])

deriveGenericForDataDec ::
  (Name -> Q Type) -> DatatypeVariant -> Cxt -> Name -> [TyVarBndrVis] -> [Type] -> [TH.ConstructorInfo] -> Q [Dec]
deriveGenericForDataDec f _variant _cxt name bndrs instTys cons = do
  let typ = appTysSubst f name instTys
  deriveGenericForDataType f bndrs name typ cons

deriveGenericForDataType ::
  (Name -> Q Type) -> [TyVarBndrVis] -> Name -> Q Type -> [TH.ConstructorInfo] -> Q [Dec]
deriveGenericForDataType f bndrs name typ cons = do
  (syns, code) <- withSharing (sharedCodeFor bndrs name f cons)
  -- We give 'from' and 'to' instance signatures mentioning the synonym for
  -- the code, rather than letting them refer to 'Code' via 'Rep'.
  let rep = [t| SOP I $(return code) |]
  inst <- instanceD
            (cxt [])
            [t| Generic $typ |]
            [ tySynInstDCompat ''Generics.SOP.Universe.Code Nothing [typ] (return code)
            , sigD 'from [t| $typ -> $rep |]
            , embedding 'from cons
            , sigD 'to   [t| $rep -> $typ |]
            , projection 'to cons
            ]
  return (syns ++ [inst])

deriveMetadataForDataDec ::
  (Name -> Q Type) -> DatatypeVariant -> Cxt -> Name -> [TyVarBndrVis] -> [Type] -> [TH.ConstructorInfo] -> Q [Dec]
deriveMetadataForDataDec f variant _cxt name bndrs instTys cons = do
  let typ = appTysSubst f name instTys
  deriveMetadataForDataType variant bndrs (typeBase name) name typ cons

deriveMetadataForDataType ::
  DatatypeVariant -> [TyVarBndrVis] -> String -> Name -> Q Type -> [TH.ConstructorInfo] -> Q [Dec]
deriveMetadataForDataType variant bndrs tyBase name typ cons = do
  (syns, mdType) <- withSharing (metadataType' bndrs tyBase variant name cons)
  md   <- instanceD (cxt [])
            [t| HasDatatypeInfo $typ |]
            [ tySynInstDCompat ''DatatypeInfoOf Nothing [typ] (return mdType)
            , funD 'datatypeInfo
                [ clause [wildP]
                  (normalB [| SOP.T.demoteDatatypeInfo (Proxy :: Proxy (DatatypeInfoOf $typ)) |])
                  []
                ]
            ]
            -- [metadata variant name cons]
  return (syns ++ [md])

{-------------------------------------------------------------------------------
  Sharing generated types via type synonyms

  The types we generate scale with the number of constructors and the number
  of fields of the datatype. Rather than producing one large type, we bind
  everything that scales to a top-level type synonym, so that each of the
  generated types stays small.
-------------------------------------------------------------------------------}

-- | The type synonyms generated so far.
data ShareState =
  ShareState
    { shareDecs :: [Dec]
      -- ^ Generated synonyms, in reverse order of generation.
    , shareSeen :: [(Type, Type)]
      -- ^ Right hand sides already bound, and references to the synonyms
      -- they have been bound to. Structurally equal types are bound only
      -- once; this matters in particular for the strictness metadata, where
      -- one and the same 'SOP.T.StrictnessInfo' is typically repeated for
      -- every field.
    , shareNames :: [String]
      -- ^ Names claimed so far. Names generated earlier in the same splice
      -- are not yet in scope, so we have to keep track of them ourselves.
    }

-- | 'Q' extended with the ability to introduce top-level type synonyms.
newtype ShareQ a = ShareQ { runShareQ :: ShareState -> Q (a, ShareState) }

instance Functor ShareQ where
  fmap f (ShareQ g) = ShareQ $ \ s -> fmap (\ (a, s') -> (f a, s')) (g s)

instance Applicative ShareQ where
  pure a = ShareQ $ \ s -> pure (a, s)
  ShareQ g <*> ShareQ h = ShareQ $ \ s -> do
    (f, s' ) <- g s
    (a, s'') <- h s'
    return (f a, s'')

instance Monad ShareQ where
  ShareQ g >>= k = ShareQ $ \ s -> do
    (a, s') <- g s
    runShareQ (k a) s'

liftQ :: Q a -> ShareQ a
liftQ q = ShareQ $ \ s -> do
  a <- q
  return (a, s)

-- | Run a computation, returning the type synonyms it generated (innermost
-- first) in addition to its result.
withSharing :: ShareQ a -> Q ([Dec], a)
withSharing m = do
  (a, s) <- runShareQ m (ShareState [] [] [])
  return (reverse (shareDecs s), a)

-- | Claim a name for a new type synonym.
--
-- The name we would like to use can already be taken: by the module we are
-- splicing into, or by a synonym we generated earlier. This is rare, but it
-- does happen, for example if a module derives for two datatypes that have
-- the same name but originate in different modules. We then prime the name
-- until it is free. (We cannot use a numeric suffix, because those are what
-- we use to name the suffixes of a list.)
claimName :: String -> ShareQ Name
claimName base = do
  free <- freeName base
  if free
    then do
      ShareQ $ \ s -> return ((), s { shareNames = base : shareNames s })
      return (mkName base)
    else claimName (base ++ "'")

-- | Is this name still available for a new type synonym?
freeName :: String -> ShareQ Bool
freeName s = do
  taken <- ShareQ $ \ st -> return (shareNames st, st)
  if s `elem` taken
    then return False
    else do
      existing <- liftQ (lookupTypeName s)
      case existing of
        Just _  -> return False
        Nothing -> return True

-- | Bind a type to a top-level type synonym of the given name, and return a
-- reference to that synonym.
--
-- Types that consist of a single type constructor (or variable, or literal)
-- are returned unchanged: there is nothing to be gained from naming them.
-- Types that have been bound before are not bound again.
--
-- The synonym abstracts over those of the datatype's binders that actually
-- occur in the right hand side. The right hand side is annotated with its
-- kind, because that kind is in general not inferrable: the code of an
-- enumeration type, for example, consists entirely of empty lists.
--
share :: [TyVarBndrVis] -> String -> Q Kind -> Q Type -> ShareQ Type
share = shareIf (not . isAtomicType)

-- | Like 'share', but introduce the synonym even for a type that consists of
-- a single type constructor. We do this for the code itself, so that there
-- is a name to refer to even for a datatype without constructors.
shareAlways :: [TyVarBndrVis] -> String -> Q Kind -> Q Type -> ShareQ Type
shareAlways = shareIf (const True)

shareIf :: (Type -> Bool) -> [TyVarBndrVis] -> String -> Q Kind -> Q Type -> ShareQ Type
shareIf p bndrs base knd ty = do
  t <- liftQ ty
  if not (p t)
    then return t
    else do
      k <- liftQ knd
      let rhs = SigT t k
      seen <- ShareQ $ \ s -> return (shareSeen s, s)
      case lookup rhs seen of
        Just ref -> return ref
        Nothing  -> do
          n <- claimName base
          let vs  = relevantBndrs bndrs rhs
              ref = foldl' AppT (ConT n) (map (VarT . tvName) vs)
          dec <- liftQ (tySynD n vs (return rhs))
          ShareQ $ \ s ->
            return ((), s { shareDecs = dec        : shareDecs s
                          , shareSeen = (rhs, ref) : shareSeen s
                          })
          return ref

-- | Introduce a second, shorter name for a type synonym we just generated,
-- unless that name is already taken.
--
-- The generated synonyms are qualified by the module the datatype comes
-- from, so that their names are predictable. That makes them long, and the
-- unqualified name is the one worth referring to, so we provide it as well
-- whenever we can.
aliasFor :: [TyVarBndrVis] -> String -> Type -> ShareQ Type
aliasFor bndrs base t = do
  free <- freeName base
  if not free
    then return t
    else do
      n <- claimName base
      let vs  = relevantBndrs bndrs t
          ref = foldl' AppT (ConT n) (map (VarT . tvName) vs)
      dec <- liftQ (tySynD n vs (return t))
      ShareQ $ \ s -> return ((), s { shareDecs = dec : shareDecs s })
      return ref

-- | The binders that actually occur in the given type.
relevantBndrs :: [TyVarBndrVis] -> Type -> [TyVarBndrVis]
relevantBndrs bndrs t =
    filter ((`elem` vs) . tvName) bndrs
  where
    vs = freeVariables t

-- | Does this type consist of a single type constructor (or variable, or
-- literal)?
isAtomicType :: Type -> Bool
isAtomicType (ConT _)           = True
isAtomicType (VarT _)           = True
isAtomicType (PromotedT _)      = True
isAtomicType (LitT _)           = True
isAtomicType (TupleT _)         = True
isAtomicType (PromotedTupleT _) = True
isAtomicType PromotedNilT       = True
isAtomicType ListT              = True
isAtomicType ArrowT             = True
isAtomicType StarT              = True
isAtomicType (SigT t _)         = isAtomicType t
isAtomicType _                  = False

-- | How to name and kind the suffixes of a promoted list that is shared via
-- type synonyms.
data ListShape =
  ListShape
    { listBndrs :: [TyVarBndrVis]
    , listName  :: Int -> String
      -- ^ Name of the suffix starting at the given position.
    , listElemK :: Q Kind
    }

-- | The body of a promoted list: every proper suffix of the list is bound to
-- a type synonym, but the list itself is returned unshared.
listBody :: ListShape -> Int -> [Q Type] -> ShareQ Type
listBody _  _ []       = liftQ promotedNilT
listBody ls i (t : ts) = do
  rest <- sharedList ls (i + 1) ts
  liftQ [t| $promotedConsT $t $(return rest) |]

-- | Like 'listBody', but bind the list itself to a type synonym as well.
sharedList :: ListShape -> Int -> [Q Type] -> ShareQ Type
sharedList ls i ts = do
  body <- listBody ls i ts
  share (listBndrs ls) (listName ls i) (appT listT (listElemK ls)) (return body)

-- | Names the suffix of a list that starts at the given position. The list
-- itself gets the unadorned name.
suffixName :: String -> Int -> String
suffixName base 0 = base
suffixName base i = base ++ show i

-- | The prefix of the names of all type synonyms generated for a datatype:
-- the name of the datatype, qualified by the module it is defined in.
--
-- The qualification makes the names predictable --- they do not depend on
-- what else happens to be derived in the same module --- at the price of
-- being long. The code additionally gets an unqualified alias, see
-- 'shortTypeBase'.
typeBase :: Name -> String
typeBase n = moduleComponent n ++ shortTypeBase n

-- | The unqualified prefix, used for the alias of the code.
shortTypeBase :: Name -> String
shortTypeBase n =
  case identComponent n of
    s@(c : _) | isUpper c -> s
    s                     -> 'T' : s

-- | The module a name is defined in, as a single identifier. Note that this
-- is the module the datatype actually comes from, which for datatypes from
-- @base@ is often an internal one.
moduleComponent :: Name -> String
moduleComponent n =
  concatMap encodeChar (filter (/= '.') (nameModule' n))

-- | Datatype, constructor and field names become part of the names of the
-- generated type synonyms. Names that are not alphanumeric identifiers (such
-- as @[]@, or the operator @:+:@) are encoded, so that distinct names remain
-- distinct.
identComponent :: Name -> String
identComponent n =
  case nameBase n of
    c : cs | isAlpha c -> toUpper c : concatMap encodeChar cs
    cs                 -> concatMap encodeChar cs

encodeChar :: Char -> String
encodeChar c
  | isAlphaNum c || c == '_' || c == '\'' = [c]
  | otherwise                             =
      fromMaybe ('_' : show (ord c)) (lookup c symbolNames)

symbolNames :: [(Char, String)]
symbolNames =
  [ ('(', "L"),     (')', "R"),      ('[', "LB"),      (']', "RB")
  , (',', "Comma"), (':', "Colon"),  ('.', "Dot"),     ('-', "Minus")
  , ('>', "Gt"),    ('<', "Lt"),     ('+', "Plus"),    ('*', "Star")
  , ('/', "Slash"), ('=', "Equals"), ('|', "Bar"),     ('&', "Amp")
  , ('^', "Hat"),   ('%', "Percent"),('$', "Dollar"),  ('!', "Bang")
  , ('#', "Hash"),  ('?', "Question"),('@', "At"),     ('~', "Tilde")
  , ('\\', "Backslash")
  ]

{-------------------------------------------------------------------------------
  Computing the code for a data type
-------------------------------------------------------------------------------}

-- | The code of a datatype. Every proper suffix of the code, and every
-- suffix of the list of field types of each constructor, is bound to a type
-- synonym. The code itself is returned unshared.
codeFor ::
  [TyVarBndrVis] -> String -> (Name -> Q Type) -> [TH.ConstructorInfo] -> ShareQ Type
codeFor bndrs tyBase f cs = do
    ts <- mapM conCode cs
    listBody (ListShape bndrs (suffixName (tyBase ++ "Code")) [t| [Kind.Type] |]) 0 (map return ts)
  where
    conCode :: TH.ConstructorInfo -> ShareQ Type
    conCode c = do
      (n, ts) <- liftQ (conInfo c)
      ts'     <- liftQ (mapM (>>= substType f) ts)
      sharedList
        (ListShape bndrs (suffixName (tyBase ++ identComponent n ++ "Code")) [t| Kind.Type |])
        0 (map return ts')

-- | Like 'codeFor', but bind the code itself to a type synonym as well, and
-- give that synonym an unqualified alias if the name is available.
sharedCodeFor ::
  [TyVarBndrVis] -> Name -> (Name -> Q Type) -> [TH.ConstructorInfo] -> ShareQ Type
sharedCodeFor bndrs name f cs = do
  body <- codeFor bndrs (typeBase name) f cs
  root <- shareAlways bndrs (typeBase name ++ "Code") [t| [[Kind.Type]] |] (return body)
  aliasFor bndrs (shortTypeBase name ++ "Code") root

{-------------------------------------------------------------------------------
  Computing the embedding/projection pair
-------------------------------------------------------------------------------}

embedding :: Name -> [TH.ConstructorInfo] -> Q Dec
embedding fromName = funD fromName . go' (\e -> [| Z $e |])
  where
    go' :: (Q Exp -> Q Exp) -> [TH.ConstructorInfo] -> [Q Clause]
    go' _ [] = (:[]) $ do
      x <- newName "x"
      clause [varP x] (normalB (caseE (varE x) [])) []
    go' br cs = go br cs

    go :: (Q Exp -> Q Exp) -> [TH.ConstructorInfo] -> [Q Clause]
    go _  []     = []
    go br (c:cs) = mkClause br c : go (\e -> [| S $(br e) |]) cs

    mkClause :: (Q Exp -> Q Exp) -> TH.ConstructorInfo -> Q Clause
    mkClause br c = do
      (n, ts) <- conInfo c
      vars    <- replicateM (length ts) (newName "x")
      clause [conP n (map varP vars)]
             (normalB [| SOP $(br . npE . map (appE (conE 'I) . varE) $ vars) |])
             []

projection :: Name -> [TH.ConstructorInfo] -> Q Dec
projection toName = funD toName . go'
  where
    go' :: [TH.ConstructorInfo] -> [Q Clause]
    go' [] = (:[]) $ do
      x <- newName "x"
      clause [varP x] (normalB (caseE (varE x) [])) []
    go' cs = go id cs

    go :: (Q Pat -> Q Pat) -> [TH.ConstructorInfo] -> [Q Clause]
    go br [] = [mkUnreachableClause br]
    go br (c:cs) = mkClause br c : go (\p -> conP 'S [br p]) cs

    -- Generates a final clause of the form:
    --
    --   to (S (... (S x))) = x `seq` error "inaccessible"
    --
    -- An equivalent way of achieving this would be:
    --
    --   to (S (... (S x))) = case x of {}
    --
    -- This, however, would require clients to enable the EmptyCase extension
    -- in their own code, which is something which we have not previously
    -- required. Therefore, we do not generate this code at the moment.
    mkUnreachableClause :: (Q Pat -> Q Pat) -> Q Clause
    mkUnreachableClause br = do
      var <- newName "x"
      clause [conP 'SOP [br (varP var)]]
             (normalB [| $(varE var) `seq` error "inaccessible" |])
             []

    mkClause :: (Q Pat -> Q Pat) -> TH.ConstructorInfo -> Q Clause
    mkClause br c = do
      (n, ts) <- conInfo c
      vars    <- replicateM (length ts) (newName "x")
      clause [conP 'SOP [br . conP 'Z . (:[]) . npP . map (\v -> conP 'I [varP v]) $ vars]]
             (normalB . appsE $ conE n : map varE vars)
             []

{-------------------------------------------------------------------------------
  Compute metadata
-------------------------------------------------------------------------------}

-- | Derive term-level metadata.
metadata' :: DatatypeVariant -> Name -> [TH.ConstructorInfo] -> Q Exp
metadata' dataVariant typeName cs = md
  where
    md :: Q Exp
    md | isNewtypeVariant dataVariant
       = [| SOP.Newtype $(stringE (nameModule' typeName))
                        $(stringE (nameBase typeName))
                        $(mdCon (head cs))
          |]

       | otherwise
       = [| SOP.ADT     $(stringE (nameModule' typeName))
                        $(stringE (nameBase typeName))
                        $(npE $ map mdCon cs)
                        $(popE $ map mdStrictness cs)
          |]

    mdStrictness :: TH.ConstructorInfo -> Q [Q Exp]
    mdStrictness ci@(ConstructorInfo { constructorName       = n
                                     , constructorStrictness = bs }) =
      checkForGADTs ci $ mdConStrictness n bs

    mdConStrictness :: Name -> [FieldStrictness] -> Q [Q Exp]
    mdConStrictness n bs = do
      dss <- reifyConStrictness n
      return (zipWith (\ (FieldStrictness su ss) ds ->
        [| SOP.StrictnessInfo
          $(mdTHUnpackedness     su)
          $(mdTHStrictness       ss)
          $(mdDecidedStrictness  ds)
        |]) bs dss)

    mdCon :: TH.ConstructorInfo -> Q Exp
    mdCon ci@(ConstructorInfo { constructorName    = n
                              , constructorVariant = conVariant }) =
      checkForGADTs ci $
      case conVariant of
        NormalConstructor    -> [| SOP.Constructor $(stringE (nameBase n)) |]
        RecordConstructor ts -> [| SOP.Record      $(stringE (nameBase n))
                                                   $(npE (map mdField ts))
                                 |]
        InfixConstructor     -> do
          fixity <- reifyFixity n
          case fromMaybe defaultFixity fixity of
            Fixity f a ->       [| SOP.Infix       $(stringE (nameBase n))
                                                   $(mdAssociativity a)
                                                   f
                                 |]


    mdField :: Name -> Q Exp
    mdField n = [| SOP.FieldInfo $(stringE (nameBase n)) |]

    mdTHUnpackedness :: TH.Unpackedness -> Q Exp
    mdTHUnpackedness UnspecifiedUnpackedness = [| SOP.NoSourceUnpackedness |]
    mdTHUnpackedness NoUnpack                = [| SOP.SourceNoUnpack       |]
    mdTHUnpackedness Unpack                  = [| SOP.SourceUnpack         |]

    mdTHStrictness :: TH.Strictness -> Q Exp
    mdTHStrictness UnspecifiedStrictness = [| SOP.NoSourceStrictness |]
    mdTHStrictness Lazy                  = [| SOP.SourceLazy         |]
    mdTHStrictness TH.Strict             = [| SOP.SourceStrict       |]

    mdDecidedStrictness :: DecidedStrictness -> Q Exp
    mdDecidedStrictness DecidedLazy   = [| SOP.DecidedLazy   |]
    mdDecidedStrictness DecidedStrict = [| SOP.DecidedStrict |]
    mdDecidedStrictness DecidedUnpack = [| SOP.DecidedUnpack |]

    mdAssociativity :: FixityDirection -> Q Exp
    mdAssociativity InfixL = [| SOP.LeftAssociative  |]
    mdAssociativity InfixR = [| SOP.RightAssociative |]
    mdAssociativity InfixN = [| SOP.NotAssociative   |]

-- | Derive type-level metadata. Everything that scales with the number of
-- constructors or the number of fields is bound to a type synonym; only the
-- 'SOP.T.ADT' (or 'SOP.T.Newtype') node itself is returned unshared.
metadataType' ::
  [TyVarBndrVis] -> String -> DatatypeVariant -> Name -> [TH.ConstructorInfo] -> ShareQ Type
metadataType' bndrs tyBase dataVariant typeName cs = md
  where
    md :: ShareQ Type
    md | isNewtypeVariant dataVariant
       = do
           c <- mdCon (head cs)
           liftQ [t| 'SOP.T.Newtype $(stringT (nameModule' typeName))
                                    $(stringT (nameBase typeName))
                                    $(return c)
                   |]

       | otherwise
       = do
           cons <- mapM mdCon cs
           conList <-
             sharedList
               (ListShape bndrs (suffixName (tyBase ++ "Constructors"))
                          [t| SOP.T.ConstructorInfo |])
               0 (map return cons)
           strs <- mapM mdStrictness cs
           strList <-
             sharedList
               (ListShape bndrs (suffixName (tyBase ++ "Strictnesses"))
                          [t| [SOP.T.StrictnessInfo] |])
               0 (map return strs)
           liftQ [t| 'SOP.T.ADT     $(stringT (nameModule' typeName))
                                    $(stringT (nameBase typeName))
                                    $(return conList)
                                    $(return strList)
                   |]

    mdStrictness :: TH.ConstructorInfo -> ShareQ Type
    mdStrictness ci@(ConstructorInfo { constructorName       = n
                                     , constructorStrictness = bs }) = do
      liftQ (checkForGADTs ci (return ()))
      dss <- liftQ (reifyConStrictness n)
      is  <- zipWithM (mdFieldStrictness (conBase n)) [0 ..] (zip bs dss)
      sharedList
        (ListShape bndrs (suffixName (conBase n ++ "Strictnesses"))
                   [t| SOP.T.StrictnessInfo |])
        0 (map return is)

    mdFieldStrictness :: String -> Int -> (FieldStrictness, DecidedStrictness) -> ShareQ Type
    mdFieldStrictness base i (FieldStrictness su ss, ds) =
      share bndrs (base ++ "Strictness" ++ show i) [t| SOP.T.StrictnessInfo |]
        [t| 'SOP.T.StrictnessInfo
          $(mdTHUnpackedness     su)
          $(mdTHStrictness       ss)
          $(mdDecidedStrictness  ds)
        |]

    mdCon :: TH.ConstructorInfo -> ShareQ Type
    mdCon ci@(ConstructorInfo { constructorName    = n
                              , constructorVariant = conVariant }) = do
      liftQ (checkForGADTs ci (return ()))
      t <- case conVariant of
        NormalConstructor    ->
          liftQ [t| 'SOP.T.Constructor $(stringT (nameBase n)) |]
        RecordConstructor ts -> do
          fs     <- mapM (mdField (conBase n)) ts
          fsList <-
            sharedList
              (ListShape bndrs (suffixName (conBase n ++ "Fields"))
                         [t| SOP.T.FieldInfo |])
              0 (map return fs)
          liftQ [t| 'SOP.T.Record      $(stringT (nameBase n))
                                       $(return fsList)
                  |]
        InfixConstructor     -> do
          fixity <- liftQ (reifyFixity n)
          case fromMaybe defaultFixity fixity of
            Fixity f a -> liftQ [t| 'SOP.T.Infix       $(stringT (nameBase n))
                                                       $(mdAssociativity a)
                                                       $(natT f)
                                  |]
      share bndrs (tyBase ++ "Constructor" ++ identComponent n)
        [t| SOP.T.ConstructorInfo |] (return t)

    mdField :: String -> Name -> ShareQ Type
    mdField base n =
      share bndrs (base ++ "Field" ++ identComponent n) [t| SOP.T.FieldInfo |]
        [t| 'SOP.T.FieldInfo $(stringT (nameBase n)) |]

    -- The prefix of the names of the synonyms belonging to one constructor.
    conBase :: Name -> String
    conBase n = tyBase ++ identComponent n

    mdTHUnpackedness :: TH.Unpackedness -> Q Type
    mdTHUnpackedness UnspecifiedUnpackedness = [t| 'SOP.NoSourceUnpackedness |]
    mdTHUnpackedness NoUnpack                = [t| 'SOP.SourceNoUnpack       |]
    mdTHUnpackedness Unpack                  = [t| 'SOP.SourceUnpack         |]

    mdTHStrictness :: TH.Strictness -> Q Type
    mdTHStrictness UnspecifiedStrictness = [t| 'SOP.NoSourceStrictness |]
    mdTHStrictness Lazy                  = [t| 'SOP.SourceLazy         |]
    mdTHStrictness TH.Strict             = [t| 'SOP.SourceStrict       |]

    mdDecidedStrictness :: DecidedStrictness -> Q Type
    mdDecidedStrictness DecidedLazy   = [t| 'SOP.DecidedLazy   |]
    mdDecidedStrictness DecidedStrict = [t| 'SOP.DecidedStrict |]
    mdDecidedStrictness DecidedUnpack = [t| 'SOP.DecidedUnpack |]

    mdAssociativity :: FixityDirection -> Q Type
    mdAssociativity InfixL = [t| 'SOP.T.LeftAssociative  |]
    mdAssociativity InfixR = [t| 'SOP.T.RightAssociative |]
    mdAssociativity InfixN = [t| 'SOP.T.NotAssociative   |]

nameModule' :: Name -> String
nameModule' = fromMaybe "" . nameModule

{-------------------------------------------------------------------------------
  Constructing n-ary pairs
-------------------------------------------------------------------------------}

-- Given
--
-- > [a, b, c]
--
-- Construct
--
-- > a :* b :* c :* Nil
npE :: [Q Exp] -> Q Exp
npE []     = [| Nil |]
npE (e:es) = [| $e :* $(npE es) |]

-- Construct a POP.
popE :: [Q [Q Exp]] -> Q Exp
popE ess =
  [| POP $(npE (map (join . fmap npE) ess)) |]

-- Like npE, but construct a pattern instead
npP :: [Q Pat] -> Q Pat
npP []     = conP 'Nil []
npP (p:ps) = conP '(:*) [p, npP ps]

{-------------------------------------------------------------------------------
  Some auxiliary definitions for working with TH
-------------------------------------------------------------------------------}

conInfo :: TH.ConstructorInfo -> Q (Name, [Q Type])
conInfo ci@(ConstructorInfo { constructorName    = n
                            , constructorFields  = ts }) =
  checkForGADTs ci $ return (n, map return ts)

stringT :: String -> Q Type
stringT = litT . strTyLit

natT :: Int -> Q Type
natT = litT . numTyLit . fromIntegral

appsT :: Name -> [Q Type] -> Q Type
appsT n = foldl' appT (conT n)

appTyVars :: (Name -> Q Type) -> Name -> [TyVarBndrVis] -> Q Type
appTyVars f n bndrs =
  appsT n (map (f . tvName) bndrs)

appTysSubst :: (Name -> Q Type) -> Name -> [Type] -> Q Type
appTysSubst f n args =
  appsT n (map (substType f . unSigType) args)

unSigType :: Type -> Type
unSigType (SigT t _) = t
unSigType t          = t

substType :: (Name -> Q Type) -> Type -> Q Type
substType f = go
  where
    go (VarT n)     = f n
    go (AppT t1 t2) = AppT <$> go t1 <*> go t2
    go ListT        = return ListT
    go (ConT n)     = return (ConT n)
    go ArrowT       = return ArrowT
    go (TupleT i)   = return (TupleT i)
    go t            = return t -- error (show t)
      -- TODO: This is incorrect, but we only need substitution to work
      -- in simple cases for now. The reason is that substitution is normally
      -- the identity, except if we use TH derivation for the tagged datatypes
      -- in the benchmarking suite. So we can fall back on identity in all
      -- but the cases we need for the benchmarking suite.

-- Process a DatatypeInfo using continuation-passing style.
withDataDec :: TH.DatatypeInfo
            -> (DatatypeVariant
                   -- The variety of data type
                   -- (@data@, @newtype@, @data instance@, or @newtype instance@)
                -> Cxt
                   -- The datatype context
                -> Name
                   -- The data type's name
                -> [TyVarBndrVis]
                   -- The datatype's type variable binders, both implicit and explicit.
                   -- Examples:
                   --
                   -- - For `data Maybe a = Nothing | Just a`, the binders are
                   --   [PlainTV a]
                   -- - For `data Proxy (a :: k) = Proxy`, the binders are
                   --   [PlainTV k, KindedTV a (VarT k)]
                   -- - For `data instance DF Int (Maybe b) = DF b`, the binders are
                   --   [PlainTV b]
                -> [Type]
                   -- For vanilla data types, these are the explicitly bound
                   -- type variable binders, but in Type form.
                   -- For data family instances, these are the type arguments.
                   -- Examples:
                   --
                   -- - For `data Maybe a = Nothing | Just a`, the types are
                   --   [VarT a]
                   -- - For `data Proxy (a :: k) = Proxy`, the types are
                   --   [SigT (VarT a) (VarT k)]
                   -- - For `data instance DF Int (Maybe b) = DF b`, the binders are
                   --   [ConT ''Int, ConT ''Maybe `AppT` VarT b]
                -> [TH.ConstructorInfo]
                   -- The data type's constructors
                -> Q a)
            -> Q a
withDataDec (TH.DatatypeInfo { datatypeContext   = ctxt
                             , datatypeName      = name
                             , datatypeVars      = bndrs
                             , datatypeInstTypes = instTypes
                             , datatypeVariant   = variant
                             , datatypeCons      = cons }) f =
  checkForTypeData variant $
  f variant ctxt name (changeTVFlags bndrReq bndrs) instTypes cons

checkForTypeData :: DatatypeVariant -> Q a -> Q a
checkForTypeData variant q = do
  case variant of
#if MIN_VERSION_th_abstraction(0,5,0)
    TH.TypeData -> fail $ "`type data` declarations not supported"
#endif
    _ -> return ()
  q

checkForGADTs :: TH.ConstructorInfo -> Q a -> Q a
checkForGADTs (ConstructorInfo { constructorVars    = exVars
                               , constructorContext = exCxt }) q = do
  unless (null exVars) $ fail "Existentials not supported"
  unless (null exCxt)  $ fail "GADTs not supported"
  q

isNewtypeVariant :: DatatypeVariant -> Bool
isNewtypeVariant Datatype        = False
isNewtypeVariant DataInstance    = False
isNewtypeVariant Newtype         = True
isNewtypeVariant NewtypeInstance = True
#if MIN_VERSION_th_abstraction(0,5,0)
isNewtypeVariant TH.TypeData     = False
#endif

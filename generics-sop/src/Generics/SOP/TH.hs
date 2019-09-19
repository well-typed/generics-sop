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

import Control.Monad (join, replicateM, unless)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Datatype as TH

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
-- Note that the generated code will require the @TypeFamilies@ and
-- @DataKinds@ extensions to be enabled for the module.
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
-- > instance Generic Tree where
-- >
-- >   type Code Tree = '[ '[Int], '[Tree, Tree] ]
-- >
-- >   from (Leaf x)   = SOP (   Z (I x :* Nil))
-- >   from (Node l r) = SOP (S (Z (I l :* I r :* Nil)))
-- >
-- >   to (SOP    (Z (I x :* Nil)))         = Leaf x
-- >   to (SOP (S (Z (I l :* I r :* Nil)))) = Node l r
-- >   to (SOP (S (S x)))                   = x `seq` error "inaccessible"
-- >
-- > instance HasDatatypeInfo Tree where
-- >   type DatatypeInfoOf Tree =
-- >     T.ADT "Main" "Tree"
-- >       '[ T.Constructor "Leaf", T.Constructor "Node" ]
-- >
-- >   datatypeInfo _ =
-- >     T.demoteDatatypeInfo (Proxy :: Proxy (DatatypeInfoOf Tree))
--
-- /Limitations:/ Generation does not work for GADTs, for
-- datatypes that involve existential quantification, for
-- datatypes with unboxed fields.
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
-- > type TreeCode = '[ '[Int], '[Tree, Tree] ]
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
    let codeType = codeFor varT cons                     -- '[ '[Int], '[Tree, Tree] ]
    let origType = appTysSubst varT name instTys         -- Tree
    let repType  = [t| SOP I $(appTyVars varT codeName' bndrs) |] -- SOP I TreeCode
    sequence
      [ tySynD codeName' bndrs codeType                 -- type TreeCode = '[ '[Int], '[Tree, Tree] ]
      , sigD fromName' [t| $origType -> $repType |]     -- fromTree :: Tree -> SOP I TreeCode
      , embedding fromName' cons                        -- fromTree ... =
      , sigD toName' [t| $repType -> $origType |]       -- toTree :: SOP I TreeCode -> Tree
      , projection toName' cons                         -- toTree ... =
      ]

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
  withDataDec dec $ \ variant _ctx name _bndrs _instTys cons ->
    sequence
      [ tySynD datatypeInfoName' [] (metadataType' variant name cons) ]

deriveGenericForDataDec ::
  (Name -> Q Type) -> DatatypeVariant -> Cxt -> Name -> [TyVarBndr] -> [Type] -> [TH.ConstructorInfo] -> Q [Dec]
deriveGenericForDataDec f _variant _cxt name _bndrs instTys cons = do
  let typ = appTysSubst f name instTys
  deriveGenericForDataType f typ cons

deriveGenericForDataType :: (Name -> Q Type) -> Q Type -> [TH.ConstructorInfo] -> Q [Dec]
deriveGenericForDataType f typ cons = do
  let codeSyn = tySynInstDCompat ''Code Nothing [typ] (codeFor f cons)
  inst <- instanceD
            (cxt [])
            [t| Generic $typ |]
            [codeSyn, embedding 'from cons, projection 'to cons]
  return [inst]

deriveMetadataForDataDec ::
  (Name -> Q Type) -> DatatypeVariant -> Cxt -> Name -> [TyVarBndr] -> [Type] -> [TH.ConstructorInfo] -> Q [Dec]
deriveMetadataForDataDec f variant _cxt name _bndrs instTys cons = do
  let typ = appTysSubst f name instTys
  deriveMetadataForDataType variant name typ cons

deriveMetadataForDataType :: DatatypeVariant -> Name -> Q Type -> [TH.ConstructorInfo] -> Q [Dec]
deriveMetadataForDataType variant name typ cons = do
  md   <- instanceD (cxt [])
            [t| HasDatatypeInfo $typ |]
            [ metadataType typ variant name cons
            , funD 'datatypeInfo
                [ clause [wildP]
                  (normalB [| SOP.T.demoteDatatypeInfo (Proxy :: Proxy (DatatypeInfoOf $typ)) |])
                  []
                ]
            ]
            -- [metadata variant name cons]
  return [md]

{-------------------------------------------------------------------------------
  Computing the code for a data type
-------------------------------------------------------------------------------}

codeFor :: (Name -> Q Type) -> [TH.ConstructorInfo] -> Q Type
codeFor f = promotedTypeList . map go
  where
    go :: TH.ConstructorInfo -> Q Type
    go c = do (_, ts) <- conInfo c
              promotedTypeListSubst f ts

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

metadataType :: Q Type -> DatatypeVariant -> Name -> [TH.ConstructorInfo] -> Q Dec
metadataType typ variant typeName cs =
  tySynInstDCompat ''DatatypeInfoOf Nothing [typ] (metadataType' variant typeName cs)

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

-- | Derive type-level metadata.
metadataType' :: DatatypeVariant -> Name -> [TH.ConstructorInfo] -> Q Type
metadataType' dataVariant typeName cs = md
  where
    md :: Q Type
    md | isNewtypeVariant dataVariant
       = [t| 'SOP.T.Newtype $(stringT (nameModule' typeName))
                            $(stringT (nameBase typeName))
                            $(mdCon (head cs))
           |]

       | otherwise
       = [t| 'SOP.T.ADT     $(stringT (nameModule' typeName))
                            $(stringT (nameBase typeName))
                            $(promotedTypeList $ map mdCon cs)
                            $(promotedTypeListOfList $ map mdStrictness cs)
           |]

    mdStrictness :: TH.ConstructorInfo -> Q [Q Type]
    mdStrictness ci@(ConstructorInfo { constructorName       = n
                                     , constructorStrictness = bs }) =
      checkForGADTs ci $ mdConStrictness n bs

    mdConStrictness :: Name -> [FieldStrictness] -> Q [Q Type]
    mdConStrictness n bs = do
      dss <- reifyConStrictness n
      return (zipWith (\ (FieldStrictness su ss) ds ->
        [t| 'SOP.T.StrictnessInfo
          $(mdTHUnpackedness     su)
          $(mdTHStrictness       ss)
          $(mdDecidedStrictness  ds)
        |]) bs dss)

    mdCon :: TH.ConstructorInfo -> Q Type
    mdCon ci@(ConstructorInfo { constructorName    = n
                              , constructorVariant = conVariant }) =
      checkForGADTs ci $
      case conVariant of
        NormalConstructor    -> [t| 'SOP.T.Constructor $(stringT (nameBase n)) |]
        RecordConstructor ts -> [t| 'SOP.T.Record      $(stringT (nameBase n))
                                                       $(promotedTypeList (map mdField ts))
                                  |]
        InfixConstructor     -> do
          fixity <- reifyFixity n
          case fromMaybe defaultFixity fixity of
            Fixity f a ->       [t| 'SOP.T.Infix       $(stringT (nameBase n))
                                                       $(mdAssociativity a)
                                                       $(natT f)
                                  |]

    mdField :: Name -> Q Type
    mdField n = [t| 'SOP.T.FieldInfo $(stringT (nameBase n)) |]

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

promotedTypeList :: [Q Type] -> Q Type
promotedTypeList []     = promotedNilT
promotedTypeList (t:ts) = [t| $promotedConsT $t $(promotedTypeList ts) |]

promotedTypeListOfList :: [Q [Q Type]] -> Q Type
promotedTypeListOfList =
  promotedTypeList . map (join . fmap promotedTypeList)

promotedTypeListSubst :: (Name -> Q Type) -> [Q Type] -> Q Type
promotedTypeListSubst _ []     = promotedNilT
promotedTypeListSubst f (t:ts) = [t| $promotedConsT $(t >>= substType f) $(promotedTypeListSubst f ts) |]

appsT :: Name -> [Q Type] -> Q Type
appsT n = foldl' appT (conT n)

appTyVars :: (Name -> Q Type) -> Name -> [TyVarBndr] -> Q Type
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
                -> [TyVarBndr]
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
  f variant ctxt name bndrs instTypes cons

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

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

import Control.Monad (join, replicateM)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

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
  dec <- reifyDec n
  ds1 <- withDataDec dec (deriveGenericForDataDec  f)
  ds2 <- withDataDec dec (deriveMetadataForDataDec f)
  return (ds1 ++ ds2)

-- | Variant of 'deriveGenericOnly' that allows to restrict the type parameters.
--
-- Experimental function, exposed primarily for benchmarking.
--
deriveGenericOnlySubst :: Name -> (Name -> Q Type) -> Q [Dec]
deriveGenericOnlySubst n f = do
  dec <- reifyDec n
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
  dec <- reifyDec n
  withDataDec dec $ \_isNewtype _cxt name bndrs cons _derivs -> do
    let codeType = codeFor varT cons                     -- '[ '[Int], '[Tree, Tree] ]
    let origType = appTyVars varT name bndrs             -- Tree
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
  dec <- reifyDec n
  withDataDec dec $ \isNewtype _cxt name _bndrs cons _derivs -> do
    sequence [ sigD datatypeInfoName' [t| SOP.DatatypeInfo $(conT codeName') |]                -- treeDatatypeInfo :: DatatypeInfo TreeCode
             , funD datatypeInfoName' [clause [] (normalB $ metadata' isNewtype name cons) []] -- treeDatatypeInfo = ...
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
  dec <- reifyDec n
  withDataDec dec $ \ isNewtype _ctx name _bndrs cons _derivs ->
    sequence
      [ tySynD datatypeInfoName' [] (metadataType' isNewtype name cons) ]

deriveGenericForDataDec ::
  (Name -> Q Type) -> Bool -> Cxt -> Name -> [TyVarBndr] -> [Con] -> Derivings -> Q [Dec]
deriveGenericForDataDec f _isNewtype _cxt name bndrs cons _derivs = do
  let typ = appTyVars f name bndrs
  deriveGenericForDataType f typ cons

deriveGenericForDataType :: (Name -> Q Type) -> Q Type -> [Con] -> Q [Dec]
deriveGenericForDataType f typ cons = do
  let codeSyn = tySynInstD ''Code $ tySynEqn [typ] (codeFor f cons)
  inst <- instanceD
            (cxt [])
            [t| Generic $typ |]
            [codeSyn, embedding 'from cons, projection 'to cons]
  return [inst]

deriveMetadataForDataDec ::
  (Name -> Q Type) -> Bool -> Cxt -> Name -> [TyVarBndr] -> [Con] -> Derivings -> Q [Dec]
deriveMetadataForDataDec f isNewtype _cxt name bndrs cons _derivs = do
  let typ = appTyVars f name bndrs
  deriveMetadataForDataType isNewtype name typ cons

deriveMetadataForDataType :: Bool -> Name -> Q Type -> [Con] -> Q [Dec]
deriveMetadataForDataType isNewtype name typ cons = do
  md   <- instanceD (cxt [])
            [t| HasDatatypeInfo $typ |]
            [ metadataType typ isNewtype name cons
            , funD 'datatypeInfo
                [ clause [wildP]
                  (normalB [| SOP.T.demoteDatatypeInfo (Proxy :: Proxy (DatatypeInfoOf $typ)) |])
                  []
                ]
            ]
            -- [metadata isNewtype name cons]
  return [md]

{-------------------------------------------------------------------------------
  Computing the code for a data type
-------------------------------------------------------------------------------}

codeFor :: (Name -> Q Type) -> [Con] -> Q Type
codeFor f = promotedTypeList . map go
  where
    go :: Con -> Q Type
    go c = do (_, ts) <- conInfo c
              promotedTypeListSubst f ts

{-------------------------------------------------------------------------------
  Computing the embedding/projection pair
-------------------------------------------------------------------------------}

embedding :: Name -> [Con] -> Q Dec
embedding fromName = funD fromName . go' (\e -> [| Z $e |])
  where
    go' :: (Q Exp -> Q Exp) -> [Con] -> [Q Clause]
    go' _ [] = (:[]) $ do
      x <- newName "x"
      clause [varP x] (normalB (caseE (varE x) [])) []
    go' br cs = go br cs

    go :: (Q Exp -> Q Exp) -> [Con] -> [Q Clause]
    go _  []     = []
    go br (c:cs) = mkClause br c : go (\e -> [| S $(br e) |]) cs

    mkClause :: (Q Exp -> Q Exp) -> Con -> Q Clause
    mkClause br c = do
      (n, ts) <- conInfo c
      vars    <- replicateM (length ts) (newName "x")
      clause [conP n (map varP vars)]
             (normalB [| SOP $(br . npE . map (appE (conE 'I) . varE) $ vars) |])
             []

projection :: Name -> [Con] -> Q Dec
projection toName = funD toName . go'
  where
    go' :: [Con] -> [Q Clause]
    go' [] = (:[]) $ do
      x <- newName "x"
      clause [varP x] (normalB (caseE (varE x) [])) []
    go' cs = go id cs

    go :: (Q Pat -> Q Pat) -> [Con] -> [Q Clause]
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

    mkClause :: (Q Pat -> Q Pat) -> Con -> Q Clause
    mkClause br c = do
      (n, ts) <- conInfo c
      vars    <- replicateM (length ts) (newName "x")
      clause [conP 'SOP [br . conP 'Z . (:[]) . npP . map (\v -> conP 'I [varP v]) $ vars]]
             (normalB . appsE $ conE n : map varE vars)
             []

{-------------------------------------------------------------------------------
  Compute metadata
-------------------------------------------------------------------------------}

metadataType :: Q Type -> Bool -> Name -> [Con] -> Q Dec
metadataType typ isNewtype typeName cs =
  tySynInstD ''DatatypeInfoOf (tySynEqn [typ] (metadataType' isNewtype typeName cs))

-- | Derive term-level metadata.
metadata' :: Bool -> Name -> [Con] -> Q Exp
metadata' isNewtype typeName cs = md
  where
    md :: Q Exp
    md | isNewtype = [| SOP.Newtype $(stringE (nameModule' typeName))
                                    $(stringE (nameBase typeName))
                                    $(mdCon (head cs))
                      |]
       | otherwise = [| SOP.ADT     $(stringE (nameModule' typeName))
                                    $(stringE (nameBase typeName))
                                    $(npE $ map mdCon cs)
                                    $(popE $ map mdStrictness cs)
                      |]

    mdStrictness :: Con -> Q [Q Exp]
    mdStrictness (NormalC n bts)            = mdConStrictness n (map fst bts)
    mdStrictness (RecC n vbts)              = mdConStrictness n (map (\ (_, b, _) -> b) vbts)
    mdStrictness (InfixC (b1, _) n (b2, _)) = mdConStrictness n [b1, b2]
    mdStrictness (ForallC _ _ _)            = fail "Existentials not supported"
    mdStrictness (GadtC _ _ _)              = fail "GADTs not supported"
    mdStrictness (RecGadtC _ _ _)           = fail "GADTs not supported"

    mdConStrictness :: Name -> [Bang] -> Q [Q Exp]
    mdConStrictness n bs = do
      dss <- reifyConStrictness n
      return (zipWith (\ (Bang su ss) ds ->
        [| SOP.StrictnessInfo
          $(mdSourceUnpackedness su)
          $(mdSourceStrictness   ss)
          $(mdDecidedStrictness  ds)
        |]) bs dss)

    mdCon :: Con -> Q Exp
    mdCon (NormalC n _)   = [| SOP.Constructor $(stringE (nameBase n)) |]
    mdCon (RecC n ts)     = [| SOP.Record      $(stringE (nameBase n))
                                               $(npE (map mdField ts))
                             |]
    mdCon (InfixC _ n _)  = do
      fixity <- reifyFixity n
      case fromMaybe defaultFixity fixity of
        Fixity f a ->
                            [| SOP.Infix       $(stringE (nameBase n)) $(mdAssociativity a) f |]
    mdCon (ForallC _ _ _) = fail "Existentials not supported"
    mdCon (GadtC _ _ _)    = fail "GADTs not supported"
    mdCon (RecGadtC _ _ _) = fail "GADTs not supported"

    mdField :: VarStrictType -> Q Exp
    mdField (n, _, _) = [| SOP.FieldInfo $(stringE (nameBase n)) |]

    mdSourceUnpackedness :: SourceUnpackedness -> Q Exp
    mdSourceUnpackedness NoSourceUnpackedness = [| SOP.NoSourceUnpackedness |]
    mdSourceUnpackedness SourceNoUnpack       = [| SOP.SourceNoUnpack       |]
    mdSourceUnpackedness SourceUnpack         = [| SOP.SourceUnpack         |]

    mdSourceStrictness :: SourceStrictness -> Q Exp
    mdSourceStrictness NoSourceStrictness = [| SOP.NoSourceStrictness |]
    mdSourceStrictness SourceLazy         = [| SOP.SourceLazy         |]
    mdSourceStrictness SourceStrict       = [| SOP.SourceStrict       |]

    mdDecidedStrictness :: DecidedStrictness -> Q Exp
    mdDecidedStrictness DecidedLazy   = [| SOP.DecidedLazy   |]
    mdDecidedStrictness DecidedStrict = [| SOP.DecidedStrict |]
    mdDecidedStrictness DecidedUnpack = [| SOP.DecidedUnpack |]

    mdAssociativity :: FixityDirection -> Q Exp
    mdAssociativity InfixL = [| SOP.LeftAssociative  |]
    mdAssociativity InfixR = [| SOP.RightAssociative |]
    mdAssociativity InfixN = [| SOP.NotAssociative   |]

-- | Derive type-level metadata.
metadataType' :: Bool -> Name -> [Con] -> Q Type
metadataType' isNewtype typeName cs = md
  where
    md :: Q Type
    md | isNewtype = [t| 'SOP.T.Newtype $(stringT (nameModule' typeName))
                                        $(stringT (nameBase typeName))
                                        $(mdCon (head cs))
                       |]
       | otherwise = [t| 'SOP.T.ADT     $(stringT (nameModule' typeName))
                                        $(stringT (nameBase typeName))
                                        $(promotedTypeList $ map mdCon cs)
                                        $(promotedTypeListOfList $ map mdStrictness cs)
                       |]

    mdStrictness :: Con -> Q [Q Type]
    mdStrictness (NormalC n bts)            = mdConStrictness n (map fst bts)
    mdStrictness (RecC n vbts)              = mdConStrictness n (map (\ (_, b, _) -> b) vbts)
    mdStrictness (InfixC (b1, _) n (b2, _)) = mdConStrictness n [b1, b2]
    mdStrictness (ForallC _ _ _)            = fail "Existentials not supported"
    mdStrictness (GadtC _ _ _)              = fail "GADTs not supported"
    mdStrictness (RecGadtC _ _ _)           = fail "GADTs not supported"

    mdConStrictness :: Name -> [Bang] -> Q [Q Type]
    mdConStrictness n bs = do
      dss <- reifyConStrictness n
      return (zipWith (\ (Bang su ss) ds ->
        [t| 'SOP.T.StrictnessInfo
          $(mdSourceUnpackedness su)
          $(mdSourceStrictness   ss)
          $(mdDecidedStrictness  ds)
        |]) bs dss)

    mdCon :: Con -> Q Type
    mdCon (NormalC n _)   = [t| 'SOP.T.Constructor $(stringT (nameBase n)) |]
    mdCon (RecC n ts)     = [t| 'SOP.T.Record      $(stringT (nameBase n))
                                                   $(promotedTypeList (map mdField ts))
                              |]
    mdCon (InfixC _ n _)  = do
      fixity <- reifyFixity n
      case fromMaybe defaultFixity fixity of
        Fixity f a ->
                            [t| 'SOP.T.Infix       $(stringT (nameBase n)) $(mdAssociativity a) $(natT f) |]
    mdCon (ForallC _ _ _) = fail "Existentials not supported"
    mdCon (GadtC _ _ _)    = fail "GADTs not supported"
    mdCon (RecGadtC _ _ _) = fail "GADTs not supported"

    mdField :: VarStrictType -> Q Type
    mdField (n, _, _) = [t| 'SOP.T.FieldInfo $(stringT (nameBase n)) |]

    mdSourceUnpackedness :: SourceUnpackedness -> Q Type
    mdSourceUnpackedness NoSourceUnpackedness = [t| 'SOP.NoSourceUnpackedness |]
    mdSourceUnpackedness SourceNoUnpack       = [t| 'SOP.SourceNoUnpack       |]
    mdSourceUnpackedness SourceUnpack         = [t| 'SOP.SourceUnpack         |]

    mdSourceStrictness :: SourceStrictness -> Q Type
    mdSourceStrictness NoSourceStrictness = [t| 'SOP.NoSourceStrictness |]
    mdSourceStrictness SourceLazy         = [t| 'SOP.SourceLazy         |]
    mdSourceStrictness SourceStrict       = [t| 'SOP.SourceStrict       |]

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

conInfo :: Con -> Q (Name, [Q Type])
conInfo (NormalC n ts) = return (n, map (return . (\(_, t)    -> t)) ts)
conInfo (RecC    n ts) = return (n, map (return . (\(_, _, t) -> t)) ts)
conInfo (InfixC (_, t) n (_, t')) = return (n, map return [t, t'])
conInfo (ForallC _ _ _) = fail "Existentials not supported"
conInfo (GadtC _ _ _)    = fail "GADTs not supported"
conInfo (RecGadtC _ _ _) = fail "GADTs not supported"

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

bndrToName :: TyVarBndr -> Name
bndrToName (PlainTV  v  ) = v
bndrToName (KindedTV v _) = v

appTyVars :: (Name -> Q Type) -> Name -> [TyVarBndr] -> Q Type
appTyVars f n bndrs =
  appsT n (map (f . bndrToName) bndrs)

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

reifyDec :: Name -> Q Dec
reifyDec name =
  do info <- reify name
     case info of TyConI dec -> return dec
                  _          -> fail "Info must be type declaration type."

withDataDec :: Dec -> (Bool -> Cxt -> Name -> [TyVarBndr] -> [Con] -> Derivings -> Q a) -> Q a
withDataDec (DataD    ctxt name bndrs _ cons derivs) f = f False ctxt name bndrs cons  derivs
withDataDec (NewtypeD ctxt name bndrs _ con  derivs) f = f True  ctxt name bndrs [con] derivs
withDataDec _ _ = fail "Can only derive labels for datatypes and newtypes."

-- | Utility type synonym to cover changes in the TH code
#if MIN_VERSION_template_haskell(2,12,0)
type Derivings = [DerivClause]
#else
type Derivings = Cxt
#endif

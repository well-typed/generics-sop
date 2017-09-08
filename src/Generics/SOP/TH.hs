{-# LANGUAGE TemplateHaskell #-}
-- | Generate @generics-sop@ boilerplate instances using Template Haskell.
module Generics.SOP.TH
  ( deriveGeneric
  , deriveGenericOnly
  , deriveGenericFunctions
  , deriveMetadataValue
  , deriveMetadataType
  ) where

import Control.Monad (replicateM)
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
-- >   to _ = error "unreachable" -- to avoid GHC warnings
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
deriveGeneric n = do
  dec <- reifyDec n
  ds1 <- withDataDec dec deriveGenericForDataDec
  ds2 <- withDataDec dec deriveMetadataForDataDec
  return (ds1 ++ ds2)

-- | Like 'deriveGeneric', but omit the 'HasDatatypeInfo' instance.
deriveGenericOnly :: Name -> Q [Dec]
deriveGenericOnly n = do
  dec <- reifyDec n
  withDataDec dec deriveGenericForDataDec

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
-- > toTree _ = error "unreachable" -- to avoid GHC warnings
--
-- @since 0.2
--
deriveGenericFunctions :: Name -> String -> String -> String -> Q [Dec]
deriveGenericFunctions n codeName fromName toName = do
  let codeName'  = mkName codeName
  let fromName' = mkName fromName
  let toName'   = mkName toName
  dec <- reifyDec n
  withDataDec dec $ \_isNewtype _cxt name _bndrs cons _derivs -> do
    let codeType = codeFor cons                        -- '[ '[Int], '[Tree, Tree] ]
    let repType = [t| SOP I $(conT codeName') |]       -- SOP I TreeCode
    sequence
      [ tySynD codeName' [] codeType                    -- type TreeCode = '[ '[Int], '[Tree, Tree] ]
      , sigD fromName' [t| $(conT name) -> $repType |]  -- fromTree :: Tree -> SOP I TreeCode
      , embedding fromName' cons                        -- fromTree ... =
      , sigD toName' [t| $repType -> $(conT name) |]    -- toTree :: SOP I TreeCode -> Tree
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
-- /Note:/ CodeType need to be derived with 'deriveGenericFunctions'.
--
-- @since 0.2
--
deriveMetadataValue :: Name -> String -> String -> Q [Dec]
deriveMetadataValue n codeName datatypeInfoName = do
  let codeName'  = mkName codeName
  let datatypeInfoName' = mkName datatypeInfoName
  dec <- reifyDec n
  withDataDec dec $ \isNewtype _cxt name _bndrs cons _derivs -> do
    sequence [ sigD datatypeInfoName' [t| SOP.DatatypeInfo $(conT codeName') |]                    -- treeDatatypeInfo :: DatatypeInfo TreeCode
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

deriveGenericForDataDec :: Bool -> Cxt -> Name -> [TyVarBndr] -> [Con] -> Derivings -> Q [Dec]
deriveGenericForDataDec _isNewtype _cxt name bndrs cons _derivs = do
  let typ = appTyVars name bndrs
#if MIN_VERSION_template_haskell(2,9,0)
  let codeSyn = tySynInstD ''Code $ tySynEqn [typ] (codeFor cons)
#else
  let codeSyn = tySynInstD ''Code [typ] (codeFor cons)
#endif
  inst <- instanceD
            (cxt [])
            [t| Generic $typ |]
            [codeSyn, embedding 'from cons, projection 'to cons]
  return [inst]

deriveMetadataForDataDec :: Bool -> Cxt -> Name -> [TyVarBndr] -> [Con] -> Derivings -> Q [Dec]
deriveMetadataForDataDec isNewtype _cxt name bndrs cons _derivs = do
  let typ = appTyVars name bndrs
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

codeFor :: [Con] -> Q Type
codeFor = promotedTypeList . map go
  where
    go :: Con -> Q Type
    go c = do (_, ts) <- conInfo c
              promotedTypeList ts

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
projection toName = funD toName . go' (\p -> conP 'Z [p])
  where
    go' :: (Q Pat -> Q Pat) -> [Con] -> [Q Clause]
    go' _ [] = (:[]) $ do
      x <- newName "x"
      clause [varP x] (normalB (caseE (varE x) [])) []
    go' br cs = go br cs

    go :: (Q Pat -> Q Pat) -> [Con] -> [Q Clause]
    go _ [] = [unreachable]
    go br (c:cs) = mkClause br c : go (\p -> conP 'S [br p]) cs

    mkClause :: (Q Pat -> Q Pat) -> Con -> Q Clause
    mkClause br c = do
      (n, ts) <- conInfo c
      vars    <- replicateM (length ts) (newName "x")
      clause [conP 'SOP [br . npP . map (\v -> conP 'I [varP v]) $ vars]]
             (normalB . appsE $ conE n : map varE vars)
             []

unreachable :: Q Clause
unreachable = clause [wildP]
                     (normalB [| error "unreachable" |])
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
                      |]


    mdCon :: Con -> Q Exp
    mdCon (NormalC n _)   = [| SOP.Constructor $(stringE (nameBase n)) |]
    mdCon (RecC n ts)     = [| SOP.Record      $(stringE (nameBase n))
                                               $(npE (map mdField ts))
                             |]
    mdCon (InfixC _ n _)  = do
#if MIN_VERSION_template_haskell(2,11,0)
      fixity <- reifyFixity n
      case fromMaybe defaultFixity fixity of
        Fixity f a ->
#else
      i <- reify n
      case i of
        DataConI _ _ _ (Fixity f a) ->
#endif
                            [| SOP.Infix       $(stringE (nameBase n)) $(mdAssociativity a) f |]
#if !MIN_VERSION_template_haskell(2,11,0)
        _                -> fail "Strange infix operator"
#endif
    mdCon (ForallC _ _ _) = fail "Existentials not supported"
#if MIN_VERSION_template_haskell(2,11,0)
    mdCon (GadtC _ _ _)    = fail "GADTs not supported"
    mdCon (RecGadtC _ _ _) = fail "GADTs not supported"
#endif

    mdField :: VarStrictType -> Q Exp
    mdField (n, _, _) = [| SOP.FieldInfo $(stringE (nameBase n)) |]

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
                       |]


    mdCon :: Con -> Q Type
    mdCon (NormalC n _)   = [t| 'SOP.T.Constructor $(stringT (nameBase n)) |]
    mdCon (RecC n ts)     = [t| 'SOP.T.Record      $(stringT (nameBase n))
                                                   $(promotedTypeList (map mdField ts))
                              |]
    mdCon (InfixC _ n _)  = do
#if MIN_VERSION_template_haskell(2,11,0)
      fixity <- reifyFixity n
      case fromMaybe defaultFixity fixity of
        Fixity f a ->
#else
      i <- reify n
      case i of
        DataConI _ _ _ (Fixity f a) ->
#endif
                            [t| 'SOP.T.Infix       $(stringT (nameBase n)) $(mdAssociativity a) $(natT f) |]
#if !MIN_VERSION_template_haskell(2,11,0)
        _                -> fail "Strange infix operator"
#endif
    mdCon (ForallC _ _ _) = fail "Existentials not supported"
#if MIN_VERSION_template_haskell(2,11,0)
    mdCon (GadtC _ _ _)    = fail "GADTs not supported"
    mdCon (RecGadtC _ _ _) = fail "GADTs not supported"
#endif

    mdField :: VarStrictType -> Q Type
    mdField (n, _, _) = [t| 'SOP.T.FieldInfo $(stringT (nameBase n)) |]

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
#if MIN_VERSION_template_haskell(2,11,0)
conInfo (GadtC _ _ _)    = fail "GADTs not supported"
conInfo (RecGadtC _ _ _) = fail "GADTs not supported"
#endif

stringT :: String -> Q Type
stringT = litT . strTyLit

natT :: Int -> Q Type
natT = litT . numTyLit . fromIntegral

promotedTypeList :: [Q Type] -> Q Type
promotedTypeList []     = promotedNilT
promotedTypeList (t:ts) = [t| $promotedConsT $t $(promotedTypeList ts) |]

appTyVars :: Name -> [TyVarBndr] -> Q Type
appTyVars n = go (conT n)
  where
    go :: Q Type -> [TyVarBndr] -> Q Type
    go t []                  = t
    go t (PlainTV  v   : vs) = go [t| $t $(varT v) |] vs
    go t (KindedTV v _ : vs) = go [t| $t $(varT v) |] vs

reifyDec :: Name -> Q Dec
reifyDec name =
  do info <- reify name
     case info of TyConI dec -> return dec
                  _          -> fail "Info must be type declaration type."

withDataDec :: Dec -> (Bool -> Cxt -> Name -> [TyVarBndr] -> [Con] -> Derivings -> Q a) -> Q a
#if MIN_VERSION_template_haskell(2,11,0)
withDataDec (DataD    ctxt name bndrs _ cons derivs) f = f False ctxt name bndrs cons  derivs
withDataDec (NewtypeD ctxt name bndrs _ con  derivs) f = f True  ctxt name bndrs [con] derivs
#else
withDataDec (DataD    ctxt name bndrs cons derivs) f = f False ctxt name bndrs cons  derivs
withDataDec (NewtypeD ctxt name bndrs con  derivs) f = f True  ctxt name bndrs [con] derivs
#endif
withDataDec _ _ = fail "Can only derive labels for datatypes and newtypes."

-- | Utility type synonym to cover changes in the TH code
#if MIN_VERSION_template_haskell(2,12,0)
type Derivings = [DerivClause]
#elif MIN_VERSION_template_haskell(2,11,0)
type Derivings = Cxt
#else
type Derivings = [Name]
#endif

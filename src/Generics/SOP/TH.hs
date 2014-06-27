{-# LANGUAGE TemplateHaskell, CPP #-}
module Generics.SOP.TH
  ( deriveGeneric
  ) where

-- TODO: Handle infix constructors.
-- TODO: Allow to not generate metadata.
-- TODO: Automatically invoke this function for base datatypes
--   (not here, but in a dedicated module / library).

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Generics.SOP

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
-- /Example:/ if you have the datatype
--
-- > data Tree = Leaf Int | Node Tree Tree
--
-- and say
--
-- > deriveGeneric ''Tree
--
-- then you get code that is equivalent to:
--
-- > type instance Code Tree = '[ '[Int], '[Tree, Tree] ]
-- >
-- > instance Generic Tree where
-- >   from (Leaf x)   = SOP (   Z (I x :* Nil))
-- >   from (Node l r) = SOP (S (Z (I l :* I r :* Nil)))
-- >
-- >   to (SOP    (Z (I x :* Nil)))         = Leaf x
-- >   to (SOP (S (Z (I l :* I r :* Nil)))) = Node l r
-- >   to _ = error "unreachable" -- to avoid GHC warnings
-- >
-- > instance HasDatatypeInfo Tree where
-- >   datatypeInfo _ = ADT "Main" "Tree"
-- >     (Constructor "Leaf" :* Constructor "Node" :* Nil)
--
-- /Limitations:/ Generation does not work for GADTs, for
-- datatypes that involve existential quantification, for
-- datatypes with unboxed fields.
--
-- It currently does not support datatypes with infix
-- constructors.
--
deriveGeneric :: Name -> Q [Dec]
deriveGeneric n = do
  dec <- reifyDec n
  withDataDec dec deriveGenericForDataDec

deriveGenericForDataDec :: Bool -> Cxt -> Name -> [TyVarBndr] -> [Con] -> [Name] -> Q [Dec]
deriveGenericForDataDec isNewtype _cxt name bndrs cons _derivs = do
  let typ = appTyVars name bndrs
#if MIN_VERSION_template_haskell(2,9,0)
  codeSyn <- tySynInstD ''Code $ tySynEqn [typ] (codeFor cons)
#else
  codeSyn <- tySynInstD ''Code [typ] (codeFor cons)
#endif
  inst <- instanceD
            (cxt [])
            [t| Generic $typ |]
            [embedding cons, projection cons]
  md   <- instanceD (cxt [])
            [t| HasDatatypeInfo $typ |]
            [metadata isNewtype name cons]
  return [codeSyn, inst, md]

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

embedding :: [Con] -> Q Dec
embedding = funD 'from . go (\e -> [| Z $e |])
  where
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

projection :: [Con] -> Q Dec
projection = funD 'to . go (\p -> conP 'Z [p])
  where
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

metadata :: Bool -> Name -> [Con] -> Q Dec
metadata isNewtype typeName cs =
    funD 'datatypeInfo [clause [wildP] (normalB md) []]
  where
    md :: Q Exp
    md | isNewtype = [| Newtype $(stringE (nameModule' typeName))
                                $(stringE (nameBase typeName))
                                $(mdCon (head cs))
                      |]
       | otherwise = [| ADT     $(stringE (nameModule' typeName))
                                $(stringE (nameBase typeName))
                                $(npE $ map mdCon cs)
                      |]


    mdCon :: Con -> Q Exp
    mdCon (NormalC n _)   = [| Constructor $(stringE (nameBase n)) |]
    mdCon (RecC n ts)     = [| Record      $(stringE (nameBase n))
                                           $(npE (map mdField ts))
                             |]
    mdCon (InfixC _ _ _)  = fail "Infix constructors not supported" -- TODO
    mdCon (ForallC _ _ _) = fail "Existentials not supported"

    mdField :: VarStrictType -> Q Exp
    mdField (n, _, _) = [| FieldInfo $(stringE (nameBase n)) |]

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

withDataDec :: Dec -> (Bool -> Cxt -> Name -> [TyVarBndr] -> [Con] -> [Name] -> Q a) -> Q a
withDataDec (DataD    ctxt name bndrs cons derivs) f = f False ctxt name bndrs cons  derivs
withDataDec (NewtypeD ctxt name bndrs con  derivs) f = f True  ctxt name bndrs [con] derivs
withDataDec _ _ = fail "Can only derive labels for datatypes and newtypes."

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Generics.SOP.Staged
  ( module X
  , module Generics.SOP.Staged
  ) where

import Data.Kind
import Data.Proxy as X
import Data.SOP.BasicFunctors as X
import Data.SOP.Classes
import Data.SOP.Constraint (And, All, Top, SListI, ccase_SList)
-- import Data.SOP.Constraint hiding (SListI(..))
import Data.SOP.Dict as X (Dict(..), withDict, hdicts)
import Data.SOP.NP as X (NP(..), POP(..), unPOP, map_NP, hd)
import Data.SOP.NS as X (NS(..), SOP(..), unSOP, map_NS, cmap_NS, collapse_NS, injections)
-- import Generics.SOP hiding (CodeQ)
import Language.Haskell.TH (CodeQ, TExp)

newtype C a = C { unC :: CodeQ a }

mapCCC :: CodeQ (a -> b -> c) -> C a -> C b -> C c
mapCCC op (C a) (C b) = C [|| $$op $$a $$b ||]

mapCCK :: CodeQ (a -> b -> c) -> C a -> C b -> K (CodeQ c) x
mapCCK op (C a) (C b) = K [|| $$op $$a $$b ||]

mapCK :: CodeQ (a -> b) -> C a -> K (CodeQ b) x
mapCK op (C a) = K [|| $$op $$a ||]

-- | Still undecided about the name, but going to use an s-prefix for now to
-- clearly disambiguate.
--
-- NOTE: The decision to flatten the constraints in Constraints and ConstraintsD
-- is necessary to ensure the limited overhead we will introduce into the generated
-- code will be optimised away completely.
--
class ( SListI (SDescription a), All SListI (SDescription a)
      -- , All (All LiftT `And` AllTails (LiftTCurry a)) (SDescription a) -- needed for stoA, hiding it here for now
      ) => SGeneric a where
  type SDescription a :: [[Type]] -- there's an argument to call this 'Description' in any case, because it's unchanged
  type Constraints (c :: Type -> Constraint) a :: Constraint -- flattened list of constraints
  data ConstraintsD (c :: Type -> Constraint) a -- flattended product of dictionaries
  -- newtype Constraints c a = Constraints (POP (C :.: Dict c) (SDescription a)) -- morally correct implementation, but we choose a simpler one

  sfrom :: CodeQ a -> (SRep a -> CodeQ r) -> CodeQ r
  sto :: SRep a -> CodeQ a

  constraints :: Constraints c a => ConstraintsD c a -- outside top-level splice
  allC :: CodeQ (ConstraintsD c a) -> POP (C :.: Dict c) (SDescription a) -- inside top-level splice

pallC :: SIsProductType a xs => CodeQ (ConstraintsD c a) -> NP (C :.: Dict c) xs
pallC d = hd $ unPOP $ allC d

type SRep a = SOP C (SDescription a)

type SIsProductType a xs = (SGeneric a, SDescription a ~ '[ xs ])
type SIsEnumType a = (SGeneric a, All ((~) '[]) (SDescription a))

sproductTypeFrom :: (SIsProductType a xs) => CodeQ a -> (NP C xs -> CodeQ r) -> CodeQ r
sproductTypeFrom c k =
  sfrom c $ \ (SOP (Z xs)) -> k xs

sproductTypeTo :: SIsProductType a xs => NP C xs -> CodeQ a
sproductTypeTo xs =
  sto (SOP (Z xs))

senumTypeFrom :: (SIsEnumType a) => CodeQ a -> (NS (K ()) (SDescription a) -> CodeQ r) -> CodeQ r
senumTypeFrom c k =
  sfrom c $ \ (SOP ns) -> k (map_NS (const (K ())) ns)

senumTypeTo :: SIsEnumType a => NS (K ()) (SDescription a) -> CodeQ a
senumTypeTo ns =
  sto (SOP (cmap_NS (Proxy @((~) '[])) (const Nil) ns))

data SList :: [k] -> Type where
  SNil  :: SList '[]
  SCons :: SListI xs => SList (x ': xs)

sList :: forall k (xs :: [k]) . SListI xs => SList xs
sList = ccase_SList (Proxy @Top) SNil SCons

type family AllTailsF (c :: [k] -> Constraint) (xs :: [k]) :: Constraint where
  AllTailsF c xs = (c xs, AllTailsF' c xs)

type family AllTailsF' (c :: [k] -> Constraint) (xs :: [k]) :: Constraint where
  AllTailsF' c '[] = ()
  AllTailsF' c (x : xs) = AllTailsF c xs

class AllTailsF c xs => AllTails (c :: [k] -> Constraint) (xs :: [k])
instance AllTailsF c xs => AllTails c xs

type family Curry r xs where
  Curry r '[]      = r
  Curry r (x : xs) = x -> Curry r xs

scurry_NP ::
  forall r xs . SListI xs => (NP C xs -> CodeQ r) -> CodeQ (Curry r xs)
scurry_NP =
  case sList :: SList xs of
    SNil  -> \ f -> f Nil
    SCons -> \ f -> [|| \ x -> $$(scurry_NP (\ xs -> f (C [|| x ||] :* xs))) ||]

stoA :: forall a f . SGeneric a => CodeQ (Dict Applicative f) -> SOP (C :.: f) (SDescription a) -> CodeQ (f a)
stoA d (SOP sop) =
  let
    go :: forall xs . CodeQ (f (Curry a xs)) -> NP (C :.: f) xs -> CodeQ (f a)
    go acc Nil                = acc
    go acc (Comp (C x) :* xs) = go [|| withDict $$d ($$acc <*> $$x) ||] xs
  in
    collapse_NS $
      cselectWith_NS (Proxy @SListI)
        (\ (Fn inj) -> K . go [|| withDict $$d (pure $$(scurry_NP @a $ sto . SOP . unK . inj)) ||])
        (injections @(SDescription a) @(NP C))
        sop

sproductTypeToA ::
  forall a f xs . SIsProductType a xs => CodeQ (Dict Applicative f) -> NP (C :.: f) xs -> CodeQ (f a)
sproductTypeToA d =
  go [|| withDict $$d (pure $$(scurry_NP (sproductTypeTo @a))) ||]
  where
    go :: forall ys . CodeQ (f (Curry a ys)) -> NP (C :.: f) ys -> CodeQ (f a)
    go acc Nil                  = acc
    go acc (Comp (C fx) :* fxs) = go [|| withDict $$d ($$acc <*> $$fx) ||] fxs

senumTypeToA ::
  forall a f xs . SIsEnumType a => CodeQ (Dict Applicative f) -> NS (K ()) (SDescription a) -> CodeQ (f a)
senumTypeToA d ns =
  stoA d (SOP (cmap_NS (Proxy @((~) '[])) (const Nil) ns))

-- Stuff we should actually define in sop-core
--

cselectWith_SOP :: forall c f g h xs . All (All c) xs => Proxy c -> (forall x . c x => f x -> g x -> h x) -> POP f xs -> SOP g xs -> SOP h xs
cselectWith_SOP p op = hczipWith p op

selectWith_SOP :: forall f g h xs . All SListI xs => (forall x . f x -> g x -> h x) -> POP f xs -> SOP g xs -> SOP h xs
selectWith_SOP p op = hzipWith p op

cselectWith_NS :: forall c f g h xs . All c xs => Proxy c -> (forall x . c x => f x -> g x -> h x) -> NP f xs -> NS g xs -> NS h xs
cselectWith_NS p op = hczipWith p op

selectWith_NS :: forall f g h xs . SListI xs => (forall x . f x -> g x -> h x) -> NP f xs -> NS g xs -> NS h xs
selectWith_NS p op = hzipWith p op


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
  , LiftT
  , CodeC
  ) where

import Data.Kind
import Data.Proxy as X
import Data.SOP.BasicFunctors as X
import Data.SOP.Classes
import Data.SOP.Constraint as X (And)
-- import Data.SOP.Constraint hiding (SListI(..))
import Data.SOP.Dict as X (Dict(..), withDict)
import Data.SOP.NP as X (NP(..), POP(..), unPOP)
import Data.SOP.NS as X (NS(..), SOP(..), unSOP)
import qualified Generics.SOP as SOP
import Language.Haskell.TH (Q, TExp)
import Language.Haskell.TH.Lib (LiftT)
import Language.Haskell.TH.Lib.Internal (CodeC)
import Language.Haskell.TH.Lift as X

type Code a = Q (TExp a)
newtype C a = C { unC :: Code a }

mapCCC :: (LiftT a, LiftT b, LiftT c) => Code (a -> b -> c) -> C a -> C b -> C c
mapCCC op (C a) (C b) = C [|| $$op $$a $$b ||]

mapCCK :: (LiftT a, LiftT b, LiftT c) => Code (a -> b -> c) -> C a -> C b -> K (Code c) x
mapCCK op (C a) (C b) = K [|| $$op $$a $$b ||]

mapCK :: (LiftT a, LiftT b) => Code (a -> b) -> C a -> K (Code b) x
mapCK op (C a) = K [|| $$op $$a ||]

-- | Still undecided about the name, but going to use an s-prefix for now to
-- clearly disambiguate.
class ( LiftT a, SListI (Description a), All SListI (Description a)
      -- , SOP.Code a ~ Description a
      -- , All (All LiftT `And` AllTails (LiftTCurry a)) (Description a) -- needed for toA, hiding it here for now
      ) => Generic a where
  type Description a :: [[Type]] -- there's an argument to call this 'Description' in any case, because it's unchanged
  from :: LiftT r => Code a -> (Rep a -> Code r) -> Code r
  to :: Rep a -> Code a

type SListI = All LiftT
type Rep a = SOP C (Description a)

type IsProductType a xs = (Generic a, Description a ~ '[ xs ])
type IsEnumType a = (Generic a, All ((~) '[]) (Description a))

class (CodeC (c a), LiftT a) => Quoted (c :: k -> Constraint) (a :: k)
instance (CodeC (c a), LiftT a) => Quoted c a

productTypeFrom :: (IsProductType a xs, LiftT r) => Code a -> (NP C xs -> Code r) -> Code r
productTypeFrom c k =
  from c $ \ (SOP (Z xs)) -> k xs

productTypeTo :: IsProductType a xs => NP C xs -> Code a
productTypeTo xs =
  to (SOP (Z xs))

enumTypeFrom :: (IsEnumType a, LiftT r) => Code a -> (NS (K ()) (Description a) -> Code r) -> Code r
enumTypeFrom c k =
  from c $ \ (SOP ns) -> k (cmap_NS (Proxy @LiftT) (const (K ())) ns)

enumTypeTo :: IsEnumType a => NS (K ()) (Description a) -> Code a
enumTypeTo ns =
  to (SOP (cmap_NS (Proxy @((~) '[])) (const Nil) ns))

data SList :: [k] -> Type where
  SNil  :: SList '[]
  SCons :: SListI xs => SList (x ': xs)

sList :: forall k (xs :: [k]) . SListI xs => SList xs
sList = ccase_SList (Proxy @LiftT) SNil SCons

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

class (LiftT (Curry r xs)) => LiftTCurry r xs
instance (LiftT (Curry r xs)) => LiftTCurry r xs

scurry_NP ::
  forall r xs . (All LiftT xs, AllTails (LiftTCurry r) xs) =>
  (NP C xs -> Code r) -> Code (Curry r xs)
scurry_NP =
  case sList :: SList xs of
    SNil  -> \ f -> f Nil
    SCons -> \ f -> [|| \ x -> $$(scurry_NP (\ xs -> f (C [|| x ||] :* xs))) ||]

type Injection (f :: k -> Type) (xs :: [k]) = f -.-> K (NS f xs)

injections :: forall xs f. SListI xs => NP (Injection f xs) xs
injections = case sList :: SList xs of
  SNil   -> Nil
  SCons  -> fn (K . Z) :* map_NP shiftInjection injections

shiftInjection :: Injection f xs a -> Injection f (x ': xs) a
shiftInjection (Fn f) = Fn $ K . S . unK . f

toA ::
  forall a f . (Generic a, Quoted Applicative f, All (All LiftT `And` AllTails (LiftTCurry a)) (Description a)) =>
  SOP (C :.: f) (Description a) -> Code (f a)
toA (SOP sop) =
  let
    go :: forall xs . (All LiftT xs, AllTails (LiftTCurry a) xs) => Code (f (Curry a xs)) -> NP (C :.: f) xs -> Code (f a)
    go acc Nil                = acc
    go acc (Comp (C x) :* xs) = go [|| $$acc <*> $$x ||] xs
  in
    collapse_NS $
      cselectWith_NS (Proxy @(All LiftT `And` AllTails (LiftTCurry a)))
        (\ (Fn inj) -> K . go [|| pure $$(scurry_NP @a $ to . SOP . unK . inj) ||])
        (injections @(Description a) @(NP C))
        sop

productTypeToA ::
  forall a f xs . (IsProductType a xs, Quoted Applicative f, AllTails (LiftTCurry a) xs) =>
  NP (C :.: f) xs -> Code (f a)
productTypeToA =
  go [|| pure $$(scurry_NP (productTypeTo @a)) ||]
  where
    go :: forall ys . (All LiftT ys, AllTails (LiftTCurry a) ys) => Code (f (Curry a ys)) -> NP (C :.: f) ys -> Code (f a)
    go acc Nil                  = acc
    go acc (Comp (C fx) :* fxs) = go [|| $$acc <*> $$fx ||] fxs

enumTypeToA ::
  forall a f xs . (IsEnumType a, Quoted Applicative f, All (All LiftT `And` AllTails (LiftTCurry a)) (Description a)) =>
  NS (K ()) (Description a) -> Code (f a)
enumTypeToA ns =
  toA (SOP (cmap_NS (Proxy @((~) '[])) (const Nil) ns))

dictImplies :: (SListI xs, forall x . c x => d x) => Dict (All c) xs -> Dict (All d) xs
dictImplies =
  dictImplies' (\ Dict -> Dict)

dictImplies' :: SListI xs => (forall x . Dict c x -> Dict d x) -> Dict (All c) xs -> Dict (All d) xs
dictImplies' f dict =
  all_NP (map_NP f (unAll_NP dict))

all_NP :: NP (Dict c) xs -> Dict (All c) xs
all_NP Nil          = Dict
all_NP (Dict :* ds) = withDict (all_NP ds) Dict

unAll_NP :: Dict (All c) xs -> NP (Dict c) xs
unAll_NP d = withDict d dicts_NP

-- Stuff we currently reproduce from sop-core

class AllF c xs => All (c :: k -> Constraint) xs where
  cpara_SList ::
       proxy c
    -> r '[]
    -> (forall y ys . (c y, All c ys) => r ys -> r (y ': ys))
    -> r xs
  dicts_NP :: NP (Dict c) xs
  cpure_NP' :: (forall x . c x => f x) -> NP f xs

instance All c '[] where
  cpara_SList _p nil _cons = nil
  dicts_NP = Nil
  cpure_NP' p = Nil

instance (c x, All c xs) => All c (x : xs) where
  cpara_SList p nil cons =
    cons (cpara_SList p nil cons)
  dicts_NP = Dict :* dicts_NP
  cpure_NP' p = p :* cpure_NP' @_ @c p

type family AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  AllF c '[] = ()
  AllF c (x : xs) = (c x, All c xs)

ccase_SList ::
     All c xs
  => proxy c
  -> r '[]
  -> (forall y ys . (c y, All c ys) => r (y ': ys))
  -> r xs
ccase_SList p nil cons =
  cpara_SList p nil (const cons)

cmap_NP :: forall c f g xs . All c xs => Proxy c -> (forall x . c x => f x -> g x) -> NP f xs -> NP g xs
cmap_NP _ f Nil       = Nil
cmap_NP p f (x :* xs) = f x :* cmap_NP p f xs

map_NP :: forall f g xs . SListI xs => (forall x . LiftT x => f x -> g x) -> NP f xs -> NP g xs
map_NP = cmap_NP (Proxy @LiftT)

cmap_NS :: forall c f g xs . All c xs => Proxy c -> (forall x . c x => f x -> g x) -> NS f xs -> NS g xs
cmap_NS _ f (Z x) = Z (f x)
cmap_NS p f (S y) = S (cmap_NS p f y)

map_NS :: forall f g xs . SListI xs => (forall x . LiftT x => f x -> g x) -> NS f xs -> NS g xs
map_NS = cmap_NS (Proxy @LiftT)

cmap_SOP :: forall c f g xss . All (All c) xss => Proxy c -> (forall x . c x => f x -> g x) -> SOP f xss -> SOP g xss
cmap_SOP p f (SOP sop) = SOP (cmap_NS (Proxy @(All c)) (cmap_NP (Proxy @c) f) sop)

czipWith_NP :: forall c f g h xs . All c xs => Proxy c -> (forall x . c x => f x -> g x -> h x) -> NP f xs -> NP g xs -> NP h xs
czipWith_NP p f xs ys = cpure_NP p (Fn $ \x -> Fn $ \ y -> f x y) `ap_NP` xs `ap_NP` ys

zipWith_NP :: forall f g h xs . SListI xs => (forall x . LiftT x => f x -> g x -> h x) -> NP f xs -> NP g xs -> NP h xs
zipWith_NP = czipWith_NP (Proxy @LiftT)

cselectWith_NS :: forall c f g h xs . All c xs => Proxy c -> (forall x . c x => f x -> g x -> h x) -> NP f xs -> NS g xs -> NS h xs
cselectWith_NS _ f (x :* _)  (Z y) = Z (f x y)
cselectWith_NS p f (_ :* xs) (S i) = S (cselectWith_NS p f xs i)

cselectWith'_NS :: forall c f g r xs . All c xs => Proxy c -> (forall x . c x => f x -> g x -> r) -> NP f xs -> NS g xs -> r
cselectWith'_NS p f np ns = collapse_NS (cselectWith_NS p (\ fx gx -> K (f fx gx)) np ns)

selectWith_NS :: forall f g h xs . SListI xs => (forall x . LiftT x => f x -> g x -> h x) -> NP f xs -> NS g xs -> NS h xs
selectWith_NS = cselectWith_NS (Proxy @LiftT)

selectWith'_NS :: forall f g r xs . SListI xs => (forall x . LiftT x => f x -> g x -> r) -> NP f xs -> NS g xs -> r
selectWith'_NS = cselectWith'_NS (Proxy @LiftT)

cpure_POP :: forall c f xss . All (All c) xss => Proxy c -> (forall x . c x => f x) -> POP f xss
cpure_POP _ f = POP (cpure_NP (Proxy @(All c)) (cpure_NP (Proxy @c) f))

cpure_NP :: forall c f xs . All c xs => Proxy c -> (forall x . c x => f x) -> NP f xs
cpure_NP _ f = cpure_NP' @_ @c f

pure_NP :: forall f xs . SListI xs => (forall x . LiftT x => f x) -> NP f xs
pure_NP p = cpure_NP (Proxy @LiftT) p

ap_NP :: NP (f -.-> g) xs -> NP f xs -> NP g xs
ap_NP Nil       Nil       = Nil
ap_NP (f :* fs) (x :* xs) = apFn f x :* ap_NP fs xs

collapse_NP :: NP (K a) xs -> [a]
collapse_NP Nil         = []
collapse_NP (K a :* xs) = a : collapse_NP xs

collapse_NS :: NS (K a) xs -> a
collapse_NS (Z (K x)) = x
collapse_NS (S i)     = collapse_NS i

collapse_SOP :: SOP (K a) xs -> [a]
collapse_SOP (SOP (Z xs)) = collapse_NP xs
collapse_SOP (SOP (S i))  = collapse_SOP (SOP i)

ccompare_NS ::
     forall c proxy r f g xs .
     (All c xs)
  => proxy c
  -> r                                    -- ^ what to do if first is smaller
  -> (forall x . c x => f x -> g x -> r)  -- ^ what to do if both are equal
  -> r                                    -- ^ what to do if first is larger
  -> NS f xs -> NS g xs
  -> r
ccompare_NS _ lt eq gt = go
  where
    go :: forall ys . (All c ys) => NS f ys -> NS g ys -> r
    go (Z x)  (Z y)  = eq x y
    go (Z _)  (S _)  = lt
    go (S _)  (Z _)  = gt
    go (S xs) (S ys) = go xs ys

ccompare_SOP ::
     forall c proxy r f g xss .
     (All (All c) xss)
  => proxy c
  -> r                                                  -- ^ what to do if first is smaller
  -> (forall xs . All c xs => NP f xs -> NP g xs -> r)  -- ^ what to do if both are equal
  -> r                                                  -- ^ what to do if first is larger
  -> SOP f xss -> SOP g xss
  -> r
ccompare_SOP _ lt eq gt (SOP xs) (SOP ys) =
  ccompare_NS (Proxy @(All c)) lt eq gt xs ys

apInjs'_NP :: SListI xs => NP f xs -> NP (K (NS f xs)) xs
apInjs'_NP = ap_NP injections

apInjs_NP :: SListI xs => NP f xs -> [NS f xs]
apInjs_NP = collapse_NP . apInjs'_NP

apInjs_POP :: forall k (f :: k -> *) (xss :: [[k]]) . SListI xss => POP f xss -> [SOP f xss]
apInjs_POP = map SOP . apInjs_NP . unPOP

apInjs'_POP :: forall k (f :: k -> *) (xss :: [[k]]) . SListI xss => POP f xss -> NP (K (SOP f xss)) xss
apInjs'_POP = map_NP (K . SOP . unK) . ap_NP injections . unPOP

ana_NP ::
     forall s f xs .
     SListI xs
  => (forall y ys . s (y ': ys) -> (f y, s ys))
  -> s xs
  -> NP f xs
ana_NP uncons = go sList
  where
    go :: forall ys . SList ys -> s ys -> NP f ys
    go SNil  _ = Nil
    go SCons s = case uncons s of
      (x, s') -> x :* go sList s'

{-
cana_NP ::
   forall c proxy s f xs . (All c xs)
=> proxy c
-> (forall y ys . c y => s (y ': ys) -> (f y, s ys))
-> s xs
-> NP f xs
cana_NP _ uncons = go sList
where
  go :: forall ys . (All c ys) => SList ys -> s ys -> NP f ys
  go SNil  _ = Nil
  go SCons s = case uncons s of
    (x, s') -> x :* go sList s'
-}

data Shape :: [k] -> Type where
  ShapeNil  :: Shape '[]
  ShapeCons :: SListI xs => Shape xs -> Shape (x ': xs)

shape :: forall k (xs :: [k]). SListI xs => Shape xs
shape = case sList :: SList xs of
          SNil  -> ShapeNil
          SCons -> ShapeCons shape

lengthSList :: forall k (xs :: [k]) proxy. SListI xs => proxy xs -> Int
lengthSList _ = lengthShape (shape :: Shape xs)
  where
    lengthShape :: forall xs'. Shape xs' -> Int
    lengthShape ShapeNil      = 0
    lengthShape (ShapeCons s) = 1 + lengthShape s

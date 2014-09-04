-- | Functions of one-liner, reimplemented using generics-sop.
module Generics.SOP.Derived where

import Control.Applicative
import Data.Monoid
import Generics.SOP

create :: forall c r. (Generic r, All2 c (Code r))
       => Proxy c -> (forall a. c a => a) -> [r]
create p f = map to (apInjs_POP (hcpure p (I f)))

createA :: forall c r f. (Generic r, All2 c (Code r), Applicative f)
        => Proxy c -> (forall a. c a => f a) -> [f r]
createA p f = map (fmap to . hsequence) (apInjs_POP (hcpure p f))

ctorIndex :: Generic r => r -> Int
ctorIndex = go . unSOP . from
  where
    go :: NS (NP I) xss -> Int
    go (Z _) = 0
    go (S i) = 1 + go i

gmap :: (Generic r, All2 c (Code r)) => Proxy c -> (forall a. c a => a -> a) -> r -> r
gmap p f = to . hcliftA p (\ (I x) -> I (f x)) . from

gfoldMap :: (Generic r, All2 c (Code r), Monoid m) => Proxy c -> (forall a. c a => a -> m) -> r -> m
gfoldMap p f = mconcat . hcollapse . hcliftA p (\ (I x) -> K (f x)) . from

gtraverse :: (Generic r, All2 c (Code r), Applicative f)
          => Proxy c -> (forall a. c a => a -> f a) -> r -> f r
gtraverse p f = fmap to . hsequence . hcliftA p (\ (I x) -> f x) . from

gzipWith :: forall c r. (Generic r, All2 c (Code r))
         => Proxy c -> (forall a. c a => a -> a -> a) -> r -> r -> Maybe r
gzipWith p f x y = to . SOP <$> go sing (unSOP (from x)) (unSOP (from y))
  where
    go :: All2 c z => Sing z -> NS (NP I) z -> NS (NP I) z -> Maybe (NS (NP I) z)
    go SCons (Z q) (Z r) = Just (Z (hcliftA2 p (\ (I a) (I b) -> I (f a b)) q r))
    go SCons (S i) (S j) = S <$> go sing i j
    go _     _     _     = Nothing

mzipWith :: forall c r m. (Generic r, All2 c (Code r), Monoid m)
         => Proxy c -> (forall a. c a => a -> a -> m) -> r -> r -> m
mzipWith p f x y = go sing (unSOP (from x)) (unSOP (from y))
  where
    go :: All2 c z => Sing z -> NS (NP I) z -> NS (NP I) z -> m
    go SCons (Z q) (Z r) = mconcat (hcollapse (hcliftA2 p (\ (I a) (I b) -> K (f a b)) q r))
    go SCons (S i) (S j) = go sing i j
    go _     _     _     = mempty

zipWithA :: forall c r f. (Generic r, All2 c (Code r), Applicative f)
         => Proxy c -> (forall a. c a => a -> a -> f a) -> r -> r -> Maybe (f r)
zipWithA p f x y = (to <$>) . hsequence . SOP <$> go sing (unSOP (from x)) (unSOP (from y))
  where
    go :: All2 c z => Sing z -> NS (NP I) z -> NS (NP I) z -> Maybe (NS (NP f) z)
    go SCons (Z q) (Z r) = Just (Z (hcliftA2 p (\ (I a) (I b) -> f a b) q r))
    go SCons (S i) (S j) = S <$> go sing i j
    go _     _     _     = Nothing

op0 :: (Generic r, All2 c (Code r), Code r ~ '[ x ], SingI x)
    => Proxy c -> (forall a. c a => a) -> r
op0 p f = to (SOP (Z (hcpure p (I f))))

op1 :: (Generic r, All2 c (Code r), Code r ~ '[ x ], SingI x)
    => Proxy c -> (forall a. c a => a -> a) -> r -> r
op1 = gmap

op2 :: (Generic r, All2 c (Code r), Code r ~ '[ x ], SingI x)
    => Proxy c -> (forall a. c a => a -> a -> a) -> r -> r -> r
op2 p f x y = to (go (from x) (from y))
  where
    go (SOP (Z q)) (SOP (Z r)) = SOP (Z (hcliftA2 p (\ (I a) (I b) -> I (f a b)) q r))
    go _           _           = error "inaccessible"

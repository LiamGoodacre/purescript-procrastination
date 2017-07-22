module Procrastination
  ( class Defer
  , class Semigroup
  , strictAppend
  , deferAppend
  , class Monoid
  , mempty
  , class Foldable
  , foldMap
  , StrictList(..)
  , DeferList(..)
  , findMap
  ) where

import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.List (List(..))
import Data.Newtype (unwrap)

-- | The Defer class should be used to mark arguments whose evaluation should
-- | be deferred until the result is needed.  Every time a deferred
-- | expression is used in a strict position it is re-evaluated or *forced*.
-- | Think about deferring in terms of implicit wrapping of `x :: t` to
-- | `(\_ -> x :: t) :: Defer => t` and the use in strict position as
-- | application of unit: `(x' :: Defer => t) unit :: t`.  In practice it is
-- | actually the type class dictionary being passed in.
class Defer
instance force :: Defer

-- | Two versions of append for comparison
class Semigroup m where
  strictAppend :: m -> m -> m
  deferAppend :: (Defer => m) -> (Defer => m) -> m

-- | Example semigroup for `First` which can ignore the rhs if the left is
-- | a `Just`.  In the `deferAppend` implementation `r` will never be forced
-- | if we only use `l`.
instance semigroupFirst :: Semigroup (First t) where
  strictAppend l r = case l of
    l'@(First (Just _)) -> l'
    _ -> r

  -- careful not to pattern match on `r` so it doesn't get forced too early
  deferAppend l@(First (Just _)) r = l
  deferAppend _ r = r

--  -- careful not to pattern match on `r` so it doesn't get forced too early
--  deferAppend l r = case l of
--    l'@(First (Just _)) -> l'
--    _ -> r

class Semigroup m <= Monoid m where
  mempty :: m

instance monoidFirst :: Monoid (First t) where
  mempty = First Nothing

class Foldable f where
  foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m

-- A version of List with a Foldable instance using strictAppend
newtype StrictList t = SL (List t)
instance foldableStrictList :: Foldable StrictList where
  foldMap f (SL Nil) = mempty
  foldMap f (SL (Cons hd tl)) = strictAppend (f hd) (foldMap f (SL tl))

-- A version of List with a Foldable instance using deferAppend
newtype DeferList t = DL (List t)
instance foldableDeferList :: Foldable DeferList where
  foldMap f (DL Nil) = mempty
  foldMap f (DL (Cons hd tl)) = deferAppend (f hd) (foldMap f (DL tl))

-- An example program using `First`
findMap :: forall f a b. Foldable f => (a -> Maybe b) -> f a -> Maybe b
findMap f xs = unwrap (foldMap (\x -> First (f x)) xs)


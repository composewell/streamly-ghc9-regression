{-# LANGUAGE ExistentialQuantification #-}

module Fold
    (
    -- * Types
      Step(..)
    , Fold (..)
    , sum
    )
where

import Data.Bifunctor (Bifunctor(..))
import Prelude hiding (sum)

------------------------------------------------------------------------------
-- Step of a fold
------------------------------------------------------------------------------

-- The Step functor around b allows expressing early termination like a right
-- fold. Traditional list right folds use function composition and laziness to
-- terminate early whereas we use data constructors. It allows stream fusion in
-- contrast to the foldr/build fusion when composing with functions.

-- | Represents the result of the @step@ of a 'Fold'.  'Partial' returns an
-- intermediate state of the fold, the fold step can be called again with the
-- state or the driver can use @extract@ on the state to get the result out.
-- 'Done' returns the final result and the fold cannot be driven further.
--
-- /Pre-release/
--
-- {-# ANN type Step Fuse #-}
data Step s b
    = Partial !s
    | Done !b

-- | 'first' maps over 'Partial' and 'second' maps over 'Done'.
--
instance Bifunctor Step where
    {-# INLINE bimap #-}
    bimap f _ (Partial a) = Partial (f a)
    bimap _ g (Done b) = Done (g b)

    {-# INLINE first #-}
    first f (Partial a) = Partial (f a)
    first _ (Done x) = Done x

    {-# INLINE second #-}
    second _ (Partial x) = Partial x
    second f (Done a) = Done (f a)

-- | 'fmap' maps over 'Done'.
--
-- @
-- fmap = 'second'
-- @
--
instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap = second

{-
-- | Map a monadic function over the result @b@ in @Step s b@.
--
-- /Internal/
{-# INLINE mapMStep #-}
mapMStep :: Applicative m => (a -> m b) -> Step s a -> m (Step s b)
mapMStep f res =
    case res of
        Partial s -> pure $ Partial s
        Done b -> Done <$> f b
-}

------------------------------------------------------------------------------
-- The Fold type
------------------------------------------------------------------------------

-- | The type @Fold m a b@ having constructor @Fold step initial extract@
-- represents a fold over an input stream of values of type @a@ to a final
-- value of type @b@ in 'Monad' @m@.
--
-- The fold uses an intermediate state @s@ as accumulator, the type @s@ is
-- internal to the specific fold definition. The initial value of the fold
-- state @s@ is returned by @initial@. The @step@ function consumes an input
-- and either returns the final result @b@ if the fold is done or the next
-- intermediate state (see 'Step'). At any point the fold driver can extract
-- the result from the intermediate state using the @extract@ function.
--
-- NOTE: The constructor is not yet exposed via exposed modules, smart
-- constructors are provided to create folds.  If you think you need the
-- constructor of this type please consider using the smart constructors in
-- "Streamly.Internal.Data.Fold' instead.
--
-- /since 0.8.0 (type changed)/
--
-- @since 0.7.0

data Fold m a b =
  -- | @Fold @ @ step @ @ initial @ @ extract@
  forall s. Fold (s -> a -> m (Step s b)) (m (Step s b)) (s -> m b)

-- | Make a fold from a left fold style pure step function and initial value of
-- the accumulator.
--
-- If your 'Fold' returns only 'Partial' (i.e. never returns a 'Done') then you
-- can use @foldl'*@ constructors.
--
-- A fold with an extract function can be expressed using fmap:
--
-- @
-- mkfoldlx :: Monad m => (s -> a -> s) -> s -> (s -> b) -> Fold m a b
-- mkfoldlx step initial extract = fmap extract (foldl' step initial)
-- @
--
-- See also: "Streamly.Prelude.foldl'"
--
-- @since 0.8.0
--
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> Fold m a b
foldl' step initial =
    Fold
        (\s a -> return $ Partial $ step s a)
        (return (Partial initial))
        return

-- | Determine the sum of all elements of a stream of numbers. Returns additive
-- identity (@0@) when the stream is empty. Note that this is not numerically
-- stable for floating point numbers.
--
-- > sum = fmap getSum $ Fold.foldMap Sum
--
-- @since 0.7.0
{-# INLINE sum #-}
sum :: (Monad m, Num a) => Fold m a a
sum =  foldl' (+) 0

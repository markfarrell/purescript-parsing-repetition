module Text.Parsing.Array.Repetition
  ( until
  , exact
  , least
  , most
  , range
  ) where

import Prelude


import Data.Array as A
import Data.List  as L

import Text.Parsing.Parser (ParserT, fail)

import Text.Parsing.Monoid.Repetition as R

-- Helper function: produces an array with a single element.
singleton :: forall a. a -> Array a
singleton x = [x] 

-- | Consumes the current parse input with a parser `p` until the result of a parser `q` is successful.
-- | Does not consume the remaining parse input with the successful result of `q`.
until :: forall m a b c. Monad m => ParserT a m b -> ParserT a m c -> ParserT a m (Array b)
until p = R.until (singleton <$> p)

-- | Consumes the current parse input with a parser `p`, with `n` repetitions of `p`.
-- | Does not check if the remaining parse input can be moreover parsed with `p`.
least :: forall a m b. Monad m => Int -> ParserT a m b -> ParserT a m (Array b)
least n p = R.least n (singleton <$> p)

-- | Consumes the current parse input with a parser `p`, with `n` repetitions of `p`.
-- | Fails if the remaining parse input can be moreover be parsed with `p`.
exact :: forall a m b. Monad m => Int -> ParserT a m b -> ParserT a m (Array b)
exact n p = R.exact n (singleton <$> p)

-- | Consumes the current parse input with a parser `p`, with `m` greedy repetitions of `p`.
-- | Fails if `m` is greater than the constraint `n` passed to the function. 
most :: forall a m b. Monad m => Int -> ParserT a m b -> ParserT a m (Array b)
most n p = case n > 0 of
 false -> exact 0 p
 true  -> do
   x <- A.fromFoldable <$> L.many p
   m <- pure $ A.length x
   case m > n of
     true  -> fail $ "Number of repetitions must be at most " <> show n <> "." 
     false -> pure x

-- | Consumes the current parse input with a parser `p`, with at *least* `min` and at *most* `max >= min` repetitions of `p`.
range :: forall a m b. Monad m => Int -> Int -> ParserT a m b -> ParserT a m (Array b)
range min max = \p -> do
  _ <- assert min max
  x <- least  min         $ p
  y <- most   (max - min) $ p
  pure (x <> y)
  where
    assert m n = case n >= m of
      false -> fail $ "Invalid range of repetitions."
      true  -> pure unit

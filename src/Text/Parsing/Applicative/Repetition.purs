module Text.Parsing.Applicative.Repetition
  ( until
  , exact
  , least
  , greedy
  , most
  , range
  ) where

import Prelude

import Data.Tuple (Tuple)
import Data.Tuple as T

import Text.Parsing.Parser (ParserT, fail)

import Text.Parsing.Monoid.Repetition as R

-- | Consumes the current parse input with a parser `p` until the result of a parser `q` is successful.
-- | Does not consume the remaining parse input with the successful result of `q`.
until :: forall m f a b c. Monad m => Applicative f => Monoid (f b) => ParserT a m b -> ParserT a m c -> ParserT a m (f b)
until p = R.until (pure <$> p)

-- | Consumes the current parse input with a parser `p`, with `n` repetitions of `p`.
-- | Does not check if the remaining parse input can be moreover parsed with `p`.
least :: forall m f a b. Monad m => Applicative f => Monoid (f b) => Int -> ParserT a m b -> ParserT a m (f b)
least n p = R.least n (pure <$> p)

-- | Consumes the current parse input with a parser `p`, with `n` repetitions of `p`.
-- | Fails if the remaining parse input can be moreover be parsed with `p`.
exact :: forall m a f b. Monad m => Applicative f => Monoid (f b) => Int -> ParserT a m b -> ParserT a m (f b)
exact n p = R.exact n (pure <$> p)

-- | Consumes the current input with a parser `p` as many times as successful.
-- | Produces a pair of the number of successful repetitions of `p`, and the accumulated result.
greedy :: forall m a f b. Monad m => Applicative f => Monoid (f b) => ParserT a m b -> ParserT a m (Tuple Int (f b))
greedy p = R.greedy (pure <$> p)

-- | Consumes the current parse input with a parser `p`, with `m` greedy repetitions of `p`.
-- | Fails if `m` is greater than the constraint `n` passed to the function. 
most :: forall m a f b. Monad m => Applicative f => Monoid (f b) => Int -> ParserT a m b -> ParserT a m (f b)
most n p = case n > 0 of
 false -> exact 0 p
 true  -> do
   w <- R.greedy (pure <$> p)
   m <- pure $ T.fst w
   x <- pure $ T.snd w
   case m > n of
     true  -> fail $ "Number of repetitions must be at most " <> show n <> "." 
     false -> pure x

-- | Consumes the current parse input with a parser `p`, with at *least* `min` and at *most* `max >= min` repetitions of `p`.
range :: forall m a f b. Monad m => Applicative f => Monoid (f b) => Int -> Int -> ParserT a m b -> ParserT a m (f b)
range min max = \p -> do
  _ <- assert min max
  x <- least  min p
  y <- most (max - min) p
  pure (x <> y)
  where
    assert m n = case n >= m of
      false -> fail $ "Invalid range of repetitions."
      true  -> pure unit

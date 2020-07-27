module Text.Parsing.Array.Repetition
  ( until
  , exact
  , least
  , greedy
  , many
  , many1
  , most
  , range
  ) where

import Prelude

import Data.Array as A
import Data.List  as L

import Data.Tuple (Tuple(..))

import Text.Parsing.Parser (ParserT)

import Text.Parsing.Applicative.Repetition as R

-- | Consumes the current parse input with a parser `p` until the result of a parser `q` is successful.
-- | Does not consume the remaining parse input with the successful result of `q`.
until :: forall m a b c. Monad m => ParserT a m b -> ParserT a m c -> ParserT a m (Array b)
until = R.until

-- | Consumes the current parse input with a parser `p`, with `n` repetitions of `p`.
-- | Does not check if the remaining parse input can be moreover parsed with `p`.
least :: forall a m b. Monad m => Int -> ParserT a m b -> ParserT a m (Array b)
least = R.least

-- | Consumes the current parse input with a parser `p`, with `n` repetitions of `p`.
-- | Fails if the remaining parse input can be moreover be parsed with `p`.
exact :: forall a m b. Monad m => Int -> ParserT a m b -> ParserT a m (Array b)
exact = R.exact

-- | Consumes the current input with a parser `p` as many times as successful.
-- | Produces a pair of the number of successful repetitions of `p`, and the accumulated result.
-- | Not guaranteed to be stack-safe for large input.
greedy :: forall m a b. Monad m => ParserT a m b -> ParserT a m (Tuple Int (Array b))
greedy p = do
  x <- many p
  pure $ Tuple (A.length x) x

-- | Consumes the current input with a parser `p` as many times as successful.
-- | Produces the accumulated result, without the guarantee of being stack-safe for large input.
many :: forall m a b. Monad m => ParserT a m b -> ParserT a m (Array b)
many p = A.fromFoldable <$> L.many p

-- | Consumes the current input with a parser `p` as many times as successful, with at least one occurrence of `p`.
-- | Produces the accumulated result, without the guarantee of being stack-safe for large input.
many1 :: forall m a b. Monad m => ParserT a m b -> ParserT a m (Array b)
many1 p = do
  x <- pure <$> p
  y <- many p
  pure $ x <> y

-- | Consumes the current parse input with a parser `p`, with `m` greedy repetitions of `p`.
-- | Fails if `m` is greater than the constraint `n` passed to the function.
most :: forall a m b. Monad m => Int -> ParserT a m b -> ParserT a m (Array b)
most = R.most

-- | Consumes the current parse input with a parser `p`, with at *least* `min` and at *most* `max >= min` repetitions of `p`.
range :: forall a m b. Monad m => Int -> Int -> ParserT a m b -> ParserT a m (Array b)
range = R.range

module Text.Parsing.String.Repetition
  ( until
  , least
  , exact
  , greedy
  , many
  , most
  , range
  ) where

import Prelude

import Data.Foldable (foldMap)

import Data.String.CodeUnits (singleton)

import Data.Tuple (Tuple(..))
import Data.Tuple as T

import Text.Parsing.Parser (ParserT)

import Text.Parsing.Array.Repetition as R

-- Helper function to convert an array of characters to a string.
fromArray :: Array Char -> String
fromArray = foldMap singleton

-- | Consumes the current parse input with a parser `p` until the result of a parser `q` is successful.
-- | Does not consume the remaining parse input with the successful result of `q`.
until :: forall m a b. Monad m => ParserT a m Char -> ParserT a m b -> ParserT a m String
until p q = fromArray <$> R.until p q

-- | Consumes the current parse input with a parser `p`, with `n` repetitions of `p`.
-- | Does not check if the remaining parse input can be moreover parsed with `p`.
least :: forall a m. Monad m => Int -> ParserT a m Char -> ParserT a m String
least n p = fromArray <$> R.least n p 

-- | Consumes the current parse input with a parser `p`, with `n` repetitions of `p`.
-- | Fails if the remaining parse input can be moreover be parsed with `p`.
exact :: forall a m. Monad m => Int -> ParserT a m Char -> ParserT a m String
exact n p = fromArray <$> R.exact n p

-- | Consumes the current input with a parser `p` as many times as successful.
-- | Produces a pair of the number of successful repetitions of `p`, and the accumulated result.
-- | Not guaranteed to be stack-safe for large input.
greedy :: forall a m. Monad m => ParserT a m Char -> ParserT a m (Tuple Int String)
greedy p = do
 x <- R.greedy p
 y <- pure $ T.fst x
 z <- pure $ fromArray $ T.snd x
 pure $ Tuple y z

-- | Consumes the current input with a parser `p` as many times as successful.
-- | Produces the accumulated result, without the guarantee of being stack-safe for large input.
many :: forall a m. Monad m => ParserT a m Char -> ParserT a m String
many p = fromArray <$> R.many p

-- | Consumes the current parse input with a parser `p`, with `m` greedy repetitions of `p`.
-- | Fails if `m` is greater than the constraint `n` passed to the function. 
most :: forall a m. Monad m => Int -> ParserT a m Char -> ParserT a m String
most n p = fromArray <$> R.most n p

-- | Consumes the current parse input with a parser `p`, with at *least* `min` and at *most* `max >= min` repetitions of `p`.
range :: forall a m. Monad m => Int -> Int -> ParserT a m Char -> ParserT a m String
range min max p = fromArray <$> R.range min max p

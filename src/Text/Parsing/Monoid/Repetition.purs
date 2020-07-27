module Text.Parsing.Monoid.Repetition
  ( until
  , exact
  , least
  , greedy
  , many
  , many1
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Maybe as M

import Data.Tuple (Tuple(..))
import Data.Tuple as T

import Text.Parsing.Parser (ParserT, fail)

import Text.Parsing.Parser.Combinators as C

-- | Consumes the current parse input with a parser `p` until the result of a parser `q` is successful.
-- | Does not consume the remaining parse input with the successful result of `q`.
until :: forall m a b c. Monad m => Monoid b => ParserT a m b -> ParserT a m c -> ParserT a m b
until = until' mempty
  where
    until' acc p q = do
      x <- p
      y <- C.optionMaybe $ C.lookAhead q
      case M.isJust y of
        true  -> pure acc
        false -> until' (acc <> x) p q

-- | Consumes the current parse input with a parser `p`, with `n` repetitions of `p`.
-- | Does not check if the remaining parse input can be moreover parsed with `p`.
least :: forall a m b. Monad m => Monoid b => Int -> ParserT a m b -> ParserT a m b
least n = \p -> case n > 0 of
  false  -> mempty
  true   -> least' mempty 0 p
  where
    least' acc m p = case m == n of
      true  -> pure acc
      false -> do
        x <- p
        y <- least' (acc <> x) (m + 1) p
        pure y

-- | Consumes the current parse input with a parser `p`, with `n` repetitions of `p`.
-- | Fails if the remaining parse input can be moreover be parsed with `p`.
exact :: forall a m b. Monad m => Monoid b => Int -> ParserT a m b -> ParserT a m b
exact n = \p -> do
  x <- least n $ p
  y <- C.optionMaybe p
  case M.isJust y of
    true  -> fail $ "Number of repetitions must be " <> show n <> "."
    false -> pure x

-- | Consumes the current input with a parser `p` as many times as successful.
-- | Produces a pair of the number of successful repetitions of `p`, and the accumulated result.
-- | Not guaranteed to be stack-safe for large input.
greedy :: forall a m b. Monad m => Monoid b => ParserT a m b -> ParserT a m (Tuple Int b)
greedy = \p -> many' 0 mempty p
  where
    many' n acc  = \p -> do
      x <- C.optionMaybe p
      case x of
        (Nothing) -> pure $ Tuple n acc
        (Just y)  -> many' (n + 1) (acc <> y) p

-- | Consumes the current input with a parser `p` as many times as successful.
-- | Produces the accumulated result, without the guarantee of being stack-safe for large input.
many :: forall a m b. Monad m => Monoid b => ParserT a m b -> ParserT a m b
many p = T.snd <$> greedy p

-- | Consumes the current input with a parser `p` as many times as successful, with at least one occurrence of `p`.
-- | Produces the accumulated result, without the guarantee of being stack-safe for large input.
many1 :: forall a m b. Monad m => Monoid b => ParserT a m b -> ParserT a m b
many1 p = do
  x <- p
  y <- many p
  pure $ x <> y

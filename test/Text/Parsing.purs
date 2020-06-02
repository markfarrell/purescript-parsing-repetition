module Test.Text.Parsing
  ( success
  , failure
  ) where

import Prelude

import Data.Either as E

import Effect (Effect)
import Effect.Exception (throw) as Exception

import Text.Parsing.Parser (Parser, runParser)

successful :: forall a b. Boolean -> a -> Parser a b -> Effect Unit
successful expect check parser = do
  result <- pure $ runParser check parser
  _      <- successful' expect (E.isRight result)
  pure unit

successful' :: Boolean -> Boolean -> Effect Unit
successful' x y = case x == y of
  false -> Exception.throw ("Test.Text.Parsing.successful (unexpected result).")
  true  -> pure unit

-- Asserts that the result of running a parser `p` with input `x` was successful.
success :: forall a b. a -> Parser a b -> Effect Unit
success = successful true

-- Asserts that the result of running a parser `p` with input `x` was a failure.
failure :: forall a b. a -> Parser a b -> Effect Unit
failure = successful false

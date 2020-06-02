module Test.Text.Parsing.Array.Repetition
  ( main
  ) where

import Prelude

import Effect (Effect)

import Text.Parsing.Parser.String as S

import Text.Parsing.Array.Repetition as R

import Test.Text.Parsing as P

n :: Int
n = (-1)

least :: Effect Unit
least = do
  _ <- P.success "a"   (R.least n (S.char 'a'))
  _ <- P.success "a"   (R.least 0 (S.char 'a'))
  _ <- P.success "a"   (R.least 1 (S.char 'a'))
  _ <- P.failure "a"   (R.least 2 (S.char 'a'))
  _ <- P.success "aa"  (R.least 2 (S.char 'a'))
  _ <- P.failure "a "  (R.least 2 (S.char 'a'))
  _ <- P.success "aaa" (R.least 2 (S.char 'a'))
  pure unit

exact :: Effect Unit
exact = do
  _ <- P.failure "a"   (R.exact n (S.char 'a'))
  _ <- P.failure "a"   (R.exact 0 (S.char 'a'))
  _ <- P.success "a"   (R.exact 1 (S.char 'a'))
  _ <- P.failure "a"   (R.exact 2 (S.char 'a'))
  _ <- P.success "aa"  (R.exact 2 (S.char 'a'))
  _ <- P.failure "a "  (R.exact 2 (S.char 'a'))
  _ <- P.failure "aaa" (R.exact 2 (S.char 'a'))
  pure unit

most :: Effect Unit
most = do
  _ <- P.failure "a"   (R.most n (S.char 'a'))
  _ <- P.failure "a"   (R.most 0 (S.char 'a'))
  _ <- P.success "a"   (R.most 1 (S.char 'a'))
  _ <- P.success "a"   (R.most 2 (S.char 'a'))
  _ <- P.success "aa"  (R.most 2 (S.char 'a'))
  _ <- P.success "a "  (R.most 2 (S.char 'a'))
  _ <- P.failure "aaa" (R.most 2 (S.char 'a'))
  _ <- P.success "aaa" (R.most 3 (S.char 'a'))
  pure unit

range :: Effect Unit
range = do
  _ <- P.failure "a"  (R.range 0 n (S.char 'a'))
  _ <- P.failure "a"  (R.range 0 0 (S.char 'a'))
  _ <- P.failure "a"  (R.range 1 0 (S.char 'a'))
  _ <- P.success "a"  (R.range n 0 (S.char 'a'))
  _ <- P.success "a"  (R.range n 1 (S.char 'a'))
  _ <- P.success "a"  (R.range 0 1 (S.char 'a'))
  _ <- P.success "a"  (R.range 1 1 (S.char 'a'))
  _ <- P.success "a"  (R.range 1 2 (S.char 'a'))
  _ <- P.failure "aa" (R.range 0 1 (S.char 'a'))
  _ <- P.success "aa" (R.range 0 2 (S.char 'a'))
  _ <- P.success "aa" (R.range 1 2 (S.char 'a'))
  _ <- P.success "aa" (R.range 2 2 (S.char 'a'))
  pure unit

until :: Effect Unit
until = do
  _ <- P.failure "a"  (R.until (S.char 'a') (S.char ' '))
  _ <- P.success "a " (R.until (S.char 'a') (S.char ' '))
  _ <- P.success "a " (R.until (S.satisfy (not <<< eq ' ')) (S.char ' '))
  _ <- P.success "a " (R.until (S.char 'a') (S.char ' ') *> S.char (' '))
  pure unit

main :: Effect Unit
main = do
  _ <- least
  _ <- exact
  _ <- most
  _ <- range
  _ <- until
  pure unit

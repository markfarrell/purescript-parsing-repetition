{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "parsing-repetition"
, license = "MIT"
, dependencies =
  [ "arrays"
  , "effect"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "parsing"
  , "parsing-expect"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

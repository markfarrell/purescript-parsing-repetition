{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "parsing-repetition"
, dependencies =
  [ "console"
  , "effect"
  , "parsing"
  , "parsing-expect"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

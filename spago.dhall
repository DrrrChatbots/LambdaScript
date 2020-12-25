{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "assert"
  , "console"
  , "effect"
  , "foreign-object"
  , "generics-rep"
  , "lists"
  , "parsing"
  , "psci-support"
  , "tailrec"
  , "tuples"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

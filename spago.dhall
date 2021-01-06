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
  , "generics-rep"
  , "lists"
  , "parsing"
  , "psci-support"
  , "tailrec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "pspy"
}

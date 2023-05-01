{ name = "virtual-dom-react-basic"
, dependencies =
  [ "console"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "maybe"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "strings"
  , "tuples"
  , "unsafe-coerce"
  , "virtual-dom"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

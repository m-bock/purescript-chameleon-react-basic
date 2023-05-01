{ name = "virtual-dom-react-basic-test"
, dependencies =
  [ "console"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "maybe"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "strings"
  , "tuples"
  , "unsafe-coerce"
  , "virtual-dom"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

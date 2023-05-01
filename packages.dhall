let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230430/packages.dhall
        sha256:007f16aff737b37041e2f387f906ea711c85afc19b898e9f4986ec85cb96efc4

in  upstream

  with virtual-dom =
      { dependencies =
          [ "either"
          , "foreign"
          , "maybe"
          , "prelude"
          , "strings"
          , "transformers"
          , "tuples"
          , "variant"
          ]
      , repo =
          "https://github.com/thought2/purescript-virtual-dom.git"
      , version =
          "main"
      }
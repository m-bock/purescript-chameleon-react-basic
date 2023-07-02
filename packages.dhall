
let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230517/packages.dhall
    sha256:8b94a0cd7f86589a6bd06d48cb9a61d69b66a94b668657b2f10c8b14c16e028c

in upstream


with marked =
  { repo = "ssh://git@github.com/thought2/purescript-marked.git"
  , version = "e9505e2438fdf5dd975fcac96a759189b9574563"
  , dependencies = [ "console", "dts", "effect", "either", "integers", "labeled-data", "maybe", "newtype", "nullable", "prelude", "ts-bridge", "unsafe-coerce", "untagged-union", "variant", "variant-encodings" ]
  }


with data-mvc =
  { repo = "https://github.com/thought2/purescript-data-mvc.git"
  , version = "b7d9be8e404530c46f4f6ce7ba540f637220250f"
  , dependencies = [ "heterogeneous", "maybe", "newtype", "prelude", "record", "variant" ]
  }


with ts-bridge =
  { repo = "https://github.com/thought2/purescript-ts-bridge.git"
  , version = "3fe0e5dd6276822e1c43c1040356e3977559c26d"
  , dependencies = [ "aff", "aff-promise", "arrays", "console", "dts", "effect", "either", "foldable-traversable", "foreign-object", "literals", "maybe", "newtype", "node-buffer", "node-fs", "node-fs-aff", "node-path", "node-process", "nullable", "optparse", "ordered-collections", "ordered-set", "partial", "prelude", "record", "safe-coerce", "strings", "transformers", "tuples", "typelevel-prelude", "unsafe-coerce", "untagged-union", "variant", "variant-encodings" ]
  }


with virtual-dom-react-basic =
  { repo = "https://github.com/thought2/purescript-virtual-dom-react-basic.git"
  , version = "83f4c1f33fe4744e2a7b99dcd89df03aa6a8867d"
  , dependencies = [ "arrays", "console", "effect", "foreign", "foreign-object", "maybe", "prelude", "react-basic", "react-basic-dom", "react-basic-hooks", "strings", "tuples", "unsafe-coerce", "virtual-dom", "web-dom" ]
  }


with virtual-dom-halogen =
  { repo = "https://github.com/thought2/purescript-virtual-dom-halogen.git"
  , version = "8cb3728c8152374523ac750effc1a8f69a20e235"
  , dependencies = [ "aff", "bifunctors", "effect", "foreign", "halogen", "prelude", "safe-coerce", "strings", "tuples", "unsafe-coerce", "virtual-dom", "web-events", "web-html" ]
  }


with virtual-dom =
  { repo = "https://github.com/thought2/purescript-virtual-dom.git"
  , version = "f5ae781a03abb150aba17384b7bc20f92d341ce9"
  , dependencies = [ "arrays", "either", "foldable-traversable", "foreign", "maybe", "newtype", "prelude", "strings", "these", "transformers", "tuples", "unordered-collections", "variant" ]
  }


with labeled-data =
  { repo = "https://github.com/thought2/purescript-labeled-data.git"
  , version = "02647c4a175d73fad22d8ecba4e8c618744d0404"
  , dependencies = [ "aff", "effect", "either", "maybe", "prelude", "record", "tuples", "type-equality", "unsafe-coerce", "variant" ]
  }


with variant-encodings =
  { repo = "https://github.com/thought2/purescript-variant-encodings.git"
  , version = "ec064edfd885f4efd0eb924ae4e26752ccf975c2"
  , dependencies = [ "prelude", "unsafe-coerce", "variant" ]
  }


with dts =
  { repo = "https://github.com/thought2/purescript-dts.git"
  , version = "0f71dd4a8ea966835eee31684e7b004c683e4f72"
  , dependencies = [ "arrays", "console", "effect", "maybe", "newtype", "ordered-collections", "ordered-set", "prelude", "tuples" ]
  }


with interactive-data-core =
  { repo = "ssh://git@github.com/thought2/purescript-interactive-data-core.git"
  , version = "a95b563e7761e09f6fb311057f50e7cab0ccce7d"
  , dependencies = [ "data-mvc", "either", "heterogeneous", "identity", "maybe", "newtype", "prelude", "profunctor", "record", "record-extra", "typelevel-prelude", "unsafe-coerce", "variant" ]
  }


with interactive-data =
  { repo = "ssh://git@github.com/thought2/purescript-interactive-data.git"
  , version = "e4766341f0573a62a4d7f4add4b06641594f084a"
  , dependencies = [ "arraybuffer-types", "arrays", "console", "convertable-options", "data-mvc", "debug", "dodo-printer", "effect", "either", "foldable-traversable", "functors", "identity", "integers", "interactive-data-core", "labeled-data", "lazy", "lists", "maybe", "newtype", "numbers", "ordered-collections", "prelude", "profunctor", "record", "safe-coerce", "strings", "these", "tuples", "type-equality", "typelevel-prelude", "unordered-collections", "unsafe-coerce", "variant", "virtual-dom" ]
  }


with data-functions =
  { repo = "ssh://git@github.com/thought2/purescript-data-functions.git"
  , version = "8707ec9f38faf43e5fbed190338580980be00557"
  , dependencies = [ "heterogeneous", "prelude" ]
  }


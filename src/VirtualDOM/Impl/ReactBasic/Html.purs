module VirtualDOM.Impl.ReactBasic.Html
  ( Config
  , ConfigOpt
  , ReactHtml
  , defaultConfig
  , runReactHtml
  , uppercaseStyleHack
  ) where

import Prelude

import Data.Array as Arr
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), toUpper)
import Data.String as Str
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Obj
import React.Basic (JSX)
import React.Basic.DOM (text) as DOM
import Unsafe.Coerce (unsafeCoerce)
import VirtualDOM (class Html, ElemName(..), Key, Prop(..))

--------------------------------------------------------------------------------
--- ReactHtml
--------------------------------------------------------------------------------

type ConfigOpt =
  { cssStringToAttr :: String -> String /\ Foreign
  , key :: Maybe Key
  }

type Config a =
  { handler :: a -> Effect Unit
  }

defaultConfig :: ConfigOpt
defaultConfig =
  { cssStringToAttr: simpleSplit
  , key: Nothing
  }

newtype ReactHtml a = ReactHtml (Config a -> ConfigOpt -> JSX)

instance Functor ReactHtml where
  map f (ReactHtml mkJsx) = ReactHtml \env -> mkJsx env
    { handler = f >>> env.handler
    }

runReactHtml :: forall a. Config a -> ConfigOpt -> ReactHtml a -> JSX
runReactHtml cfg cfgOpt (ReactHtml f) =
  f cfg cfgOpt

instance Html ReactHtml where
  elem (ElemName name) props1 children1 = ReactHtml $ \cfg cfgOpt ->
    let
      props2 = Obj.fromFoldable $ mkProp cfg cfgOpt <$> props1
      children2 = (\jsx -> runReactHtml cfg cfgOpt { key = Nothing } jsx) <$> children1
    in
      if Arr.null children2 then
        createVoidElement name props2
      else
        createElement name props2 children2

  elemKeyed (ElemName name) props1 children1 = ReactHtml $ \cfg cfgOpt ->
    let
      props2 = Obj.fromFoldable $ mkProp cfg cfgOpt <$> props1
      children2 = (\(key /\ jsx) -> runReactHtml cfg cfgOpt { key = Just key } jsx) <$> children1
    in
      if Arr.null children2 then
        createVoidElement name props2
      else
        createElement name props2 children2

  text str = ReactHtml $ \_ _ -> DOM.text str

mkProp :: forall a. Config a -> ConfigOpt -> Prop a -> String /\ Foreign
mkProp { handler } { cssStringToAttr } = case _ of
  Attr "style" v -> cssStringToAttr v
  Attr k v -> k /\ toForeign v
  Event n f -> ("on" <> upperFirst n) /\ toForeign
    ( mkEffectFn1 \event -> case f event of
        Just action -> handler action
        Nothing -> pure unit
    )

-------------------------------------------------------------------------------
--- CSS Converter
-------------------------------------------------------------------------------

-- | This is a hack to provide styles as string to React
-- | It creates a warning in the console
uppercaseStyleHack :: String -> String /\ Foreign
uppercaseStyleHack str = "STYLE" /\ toForeign str

-- | This is a very simple parser for CSS strings
-- | It does not work for all cases, but it is enough for most of them
simpleSplit :: String -> String /\ Foreign
simpleSplit str =
  let
    decls :: Array String
    decls = Str.split (Pattern ";") str

    entries :: Array (String /\ String)
    entries = map mkEntry decls

    object :: Object String
    object = Obj.fromFoldable entries

    object' :: Object Foreign
    object' = map toForeign object
  in
    "style" /\ toForeign object'

  where
  mkEntry :: String -> String /\ String
  mkEntry str' =
    let
      parts :: Array String
      parts = Str.split (Pattern ":") str'
    in
      case Array.uncons parts of
        Just { head, tail } -> kebabToCamelCase head /\ Str.joinWith ":" tail
        _ -> "" /\ ""

--------------------------------------------------------------------------------
--- FFI
--------------------------------------------------------------------------------

foreign import createVoidElement :: String -> Object Foreign -> JSX

foreign import createElement :: String -> Object Foreign -> Array JSX -> JSX

--------------------------------------------------------------------------------
--- ToForeign
--------------------------------------------------------------------------------

class ToForeign a where
  toForeign :: a -> Foreign

instance ToForeign JSX where
  toForeign = unsafeCoerce

instance ToForeign a => ToForeign (Array a) where
  toForeign = unsafeCoerce <<< map toForeign

instance ToForeign String where
  toForeign = unsafeCoerce

instance ToForeign (Object Foreign) where
  toForeign = unsafeCoerce

instance ToForeign (EffectFn1 Foreign Unit) where
  toForeign = unsafeCoerce

--------------------------------------------------------------------------------
--- Utils
--------------------------------------------------------------------------------

upperFirst :: String -> String
upperFirst str =
  case str of
    "" -> ""
    _ -> toUpper (Str.take 1 str) <> Str.drop 1 str

kebabToCamelCase :: String -> String
kebabToCamelCase str =
  let
    parts :: Array String
    parts = Str.split (Pattern "-") str
  in
    Str.joinWith "" $ map upperFirst parts

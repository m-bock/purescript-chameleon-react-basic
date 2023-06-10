module VirtualDOM.Impl.ReactBasic.Html
  ( Config
  , ConfigOpt
  , ReactHtml
  , ReactHtmlCtx(..)
  , defaultConfig
  , runReactHtml
  , runReactHtmlCtx
  ) where

import Prelude

import Data.Array as Arr
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
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
import VirtualDOM (class Ctx, class CtxHtml, class Html, ElemName(..), Key, Prop(..))
import VirtualDOM as V

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
  { cssStringToAttr: \str -> "STYLE" /\ toForeign str
  -- ^ This is a hack to provide styles as string to React
  -- It creates a warning in the console, you can provide a custom parser to avoid it
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

foreign import createVoidElement :: String -> Object Foreign -> JSX

foreign import createElement :: String -> Object Foreign -> Array JSX -> JSX

upperFirst :: String -> String
upperFirst str =
  case str of
    "" -> ""
    _ -> toUpper (Str.take 1 str) <> Str.drop 1 str

instance ToForeign JSX where
  toForeign = unsafeCoerce

instance ToForeign a => ToForeign (Array a) where
  toForeign = unsafeCoerce <<< map toForeign

instance ToForeign String where
  toForeign = unsafeCoerce

instance ToForeign (EffectFn1 Foreign Unit) where
  toForeign = unsafeCoerce

class ToForeign a where
  toForeign :: a -> Foreign

--------------------------------------------------------------------------------

newtype ReactHtmlCtx ctx a = ReactHtmlCtx (ctx -> ReactHtml a)

derive instance Functor (ReactHtmlCtx ctx)

instance Html (ReactHtmlCtx ctx) where
  elem elemName props children = ReactHtmlCtx \ctx ->
    V.elem elemName props (runReactHtmlCtx ctx <$> children)

  elemKeyed elemName props children = ReactHtmlCtx \ctx ->
    V.elemKeyed elemName props ((\(key /\ html) -> key /\ runReactHtmlCtx ctx html) <$> children)

  text str = ReactHtmlCtx \_ -> V.text str

instance Ctx (ReactHtmlCtx ctx) ctx where
  withCtx mkHtml = ReactHtmlCtx \ctx -> runReactHtmlCtx ctx (mkHtml ctx)
  setCtx ctx html = ReactHtmlCtx \_ -> runReactHtmlCtx ctx html

instance CtxHtml (ReactHtmlCtx ctx) ctx

runReactHtmlCtx :: forall ctx a. ctx -> ReactHtmlCtx ctx a -> ReactHtml a
runReactHtmlCtx ctx (ReactHtmlCtx f) = f ctx

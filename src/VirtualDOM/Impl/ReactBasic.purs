module VirtualDOM.Impl.ReactBasic
  ( ReactHtml
  , ReactHtmlCtx(..)
  , runReactHtml
  , runReactHtmlCtx
  , runReactHtmlKeyed
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Data.String as Str
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Obj
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (text) as DOM
import React.Basic.DOM (unsafeCreateDOMComponent)
import Unsafe.Coerce (unsafeCoerce)
import VirtualDOM (class Ctx, class Html, ElemName(..), Key, Prop(..))
import VirtualDOM as V

--------------------------------------------------------------------------------
--- ReactHtml
--------------------------------------------------------------------------------

newtype ReactHtml a = ReactHtml ((a -> Effect Unit) -> Maybe Key -> JSX)

instance Functor ReactHtml where
  map f (ReactHtml mkJsx) = ReactHtml \handler -> mkJsx (f >>> handler)

runReactHtmlKeyed :: forall a. (a -> Effect Unit) -> Key /\ ReactHtml a -> JSX
runReactHtmlKeyed handler (key /\ ReactHtml f) = f handler (Just key)

runReactHtml :: forall a. (a -> Effect Unit) -> ReactHtml a -> JSX
runReactHtml handler (ReactHtml f) = f handler Nothing

instance Html ReactHtml where
  elem (ElemName name) props1 children1 = ReactHtml $ \handleAction _ ->
    let

      props2 = Obj.fromFoldable $ mkProp handleAction <$> props1
      props3 = Obj.insert "children" (toForeign children2) props2

      children2 = runReactHtml handleAction <$> children1

    in
      foreignFn (element $ mkComp name) props3

  elemKeyed (ElemName name) props1 children1 = ReactHtml $ \handleAction key ->
    let

      props2 = Obj.fromFoldable $ mkProp handleAction <$> props1
      props3 = Obj.insert "children" (toForeign children2) props2

      children2 = runReactHtmlKeyed handleAction <$> children1

    in
      foreignFn (element $ mkComp name) props3

  text str = ReactHtml $ \_ _ -> DOM.text str

mkProp :: forall a. (a -> Effect Unit) -> Prop a -> String /\ Foreign
mkProp handleAction = case _ of
  Attr "style" v -> "STYLE" /\ toForeign v
  Attr k v -> k /\ toForeign v
  Event n f -> ("on" <> upperFirst n) /\ toForeign
    ( mkEffectFn1 \event -> case f event of
        Just action -> handleAction action
        Nothing -> pure unit
    )

mkComp :: forall r. String -> ReactComponent (Record r)
mkComp name = unsafePerformEffect (unsafeCreateDOMComponent name)

foreignFn :: forall r. (Record r -> JSX) -> Object Foreign -> JSX
foreignFn = unsafeCoerce

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

runReactHtmlCtx :: forall ctx a. ctx -> ReactHtmlCtx ctx a -> ReactHtml a
runReactHtmlCtx ctx (ReactHtmlCtx f) = f ctx

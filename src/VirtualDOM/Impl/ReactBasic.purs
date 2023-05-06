module VirtualDOM.Impl.ReactBasic
  ( ReactHtml
  , ReactHtmlCtx(..)
  , runReactHtml
  , runReactHtml'
  , runReactHtmlCtx
  , runReactHtmlKeyed
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
import VirtualDOM (class Ctx, class Html, ElemName(..), Key, Prop(..))
import VirtualDOM as V

--------------------------------------------------------------------------------
--- ReactHtml
--------------------------------------------------------------------------------
type Env a =
  { handler :: a -> Effect Unit
  , key :: Maybe Key
  }

newtype ReactHtml a = ReactHtml (Env a -> JSX)

instance Functor ReactHtml where
  map f (ReactHtml mkJsx) = ReactHtml \env -> mkJsx env
    { handler = f >>> env.handler
    }

runReactHtmlKeyed :: forall a. (a -> Effect Unit) -> Key /\ ReactHtml a -> JSX
runReactHtmlKeyed handler (key /\ ReactHtml mkJsx) = mkJsx { handler, key: Just key }

runReactHtml' :: forall a r. (r -> ReactHtml a) -> Effect ({ props :: r, handler :: (a -> Effect Unit) } -> JSX)
runReactHtml' f = pure \r -> case f r.props of
  ReactHtml x -> x { handler: r.handler, key: Nothing }

runReactHtml :: forall a. (a -> Effect Unit) -> ReactHtml a -> JSX
runReactHtml handler (ReactHtml f) = f { handler, key: Nothing }

instance Html ReactHtml where
  elem (ElemName name) props1 children1 = ReactHtml $ \env ->
    let
      props2 = Obj.fromFoldable $ mkProp env.handler <$> props1
      children2 = runReactHtml env.handler <$> children1
    in
      if Arr.null children2 then
        createVoidElement name props2
      else
        createElement name props2 children2

  elemKeyed (ElemName name) props1 children1 = ReactHtml $ \env ->
    let
      props2 = Obj.fromFoldable $ mkProp env.handler <$> props1
      children2 = runReactHtmlKeyed env.handler <$> children1
    in
      if Arr.null children2 then
        createVoidElement name props2
      else
        createElement name props2 children2

  text str = ReactHtml $ \_ -> DOM.text str

mkProp :: forall a. (a -> Effect Unit) -> Prop a -> String /\ Foreign
mkProp handleAction = case _ of
  Attr "style" v -> "STYLE" /\ toForeign v
  -- ^ This is a hack to provide styles as string to React
  -- It creates a warning in the console, so it should be replaced with something better
  Attr k v -> k /\ toForeign v
  Event n f -> ("on" <> upperFirst n) /\ toForeign
    ( mkEffectFn1 \event -> case f event of
        Just action -> handleAction action
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

runReactHtmlCtx :: forall ctx a. ctx -> ReactHtmlCtx ctx a -> ReactHtml a
runReactHtmlCtx ctx (ReactHtmlCtx f) = f ctx

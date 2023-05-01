# purescript-virtual-dom-react-basic
React Basic implementation of the general `Html` class from the
[virtual-dom](https://github.com/thought2/purescript-virtual-dom) package. You can write your web views in a framework agnostic way and this package can convert them to react-basic components (and therefore to actual React components as well). 

## Example
```hs
module Test.SampleReadme where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import React.Basic.DOM.Client as ReactBasicDOM
import React.Basic.Hooks as React
import VirtualDOM (class Html, text)
import VirtualDOM.HTML.Attributes as TA
import VirtualDOM.HTML.Elements as T
import VirtualDOM.HTML.Events as TE
import VirtualDOM.Impl.ReactBasic as VirtualDOM.React
import Web.DOM as DOM
```
### Framework agnostic view
```hs
type State = Int

data Msg
  = Increment Int
  | Decrement Int

counterUpdate :: Msg -> State -> State
counterUpdate msg state = case msg of
  Increment n -> state + n
  Decrement n -> state - n

counterView :: forall html ctx. Html html => { count :: Int } -> html ctx Msg
counterView props =
  T.div
    [ TA.style "border: 1px solid red"
    ]
    [ text "Counter"
    , T.div [] [ text $ show props.count ]
    , T.button [ TE.onClick (Increment 1) ]
        [ text "+" ]
    , T.button [ TE.onClick (Decrement 1) ]
        [ text "-" ]
    ]
```
### React Basic Hook component
```hs
mkApp :: React.Component {}
mkApp = do
  React.component "Counter" \_props -> React.do

    state /\ setState <- React.useState 0

    let
      handler msg = setState $ counterUpdate msg

    pure
      $ VirtualDOM.React.runReactHTML unit handler
      $ counterView { count: state }
```
### Main
```hs
foreign import elemById :: String -> Effect DOM.Element

main :: Effect Unit
main = do
  rootElem <- elemById "root"
  app <- mkApp
  reactRoot <- ReactBasicDOM.createRoot rootElem
  ReactBasicDOM.renderRoot reactRoot (app {})
```

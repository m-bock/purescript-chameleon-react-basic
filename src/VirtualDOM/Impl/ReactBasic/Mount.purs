module VirtualDOM.Impl.ReactBasic.Mount where

import Prelude

import Effect (Effect)
import React.Basic.DOM.Client as ReactBasicDOM
import React.Basic.Hooks ((/\))
import React.Basic.Hooks as React
import VirtualDOM.Impl.ReactBasic.Html (ReactHtml, defaultConfig, runReactHtml)
import Web.DOM as DOM

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type UI html msg sta =
  { view :: sta -> html msg
  , update :: msg -> sta -> sta
  , init :: sta
  }

--------------------------------------------------------------------------------
-- Halogen Component
--------------------------------------------------------------------------------

uiToReactComponent :: forall msg sta. UI ReactHtml msg sta -> React.Component {}
uiToReactComponent ui = do
  React.component "Root" \_props -> React.do

    state /\ setState <- React.useState $ ui.init

    let
      handler :: msg -> Effect Unit
      handler msg = setState $ ui.update msg

    pure
      $ runReactHtml { handler } defaultConfig
      $ ui.view state

--------------------------------------------------------------------------------
-- Mounting
--------------------------------------------------------------------------------

uiMountAtId :: forall msg sta. String -> UI ReactHtml msg sta -> Effect Unit
uiMountAtId id ui = do
  rootElem <- elemById id
  app <- uiToReactComponent ui
  reactRoot <- ReactBasicDOM.createRoot rootElem
  ReactBasicDOM.renderRoot reactRoot (app {})

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

foreign import elemById :: String -> Effect DOM.Element
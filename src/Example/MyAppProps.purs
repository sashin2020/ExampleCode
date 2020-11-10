module Example.MyAppProps where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
--import Effect (Effect)
--import Effect.Exception (throw)
import Example.AppState (MyAppState(MyAppState), myAppInitState)
import Example.Library (MyAppProps, myAppPropsFromJS, theMyAppClass)
import Example.LoggingFn (logTrue)

import Example.StateTransFn (StateTransFn, myAppAddWord, myAppDeleteSelected, myAppSetDefinitionString, myAppSetSearchString)
import Example.Stateless.MyDictEntryProps (componentPropsFor, myDictionaryView)
import React.Basic (ReactComponent, Self, toReactComponent)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Events as Event
import React.Basic.Events (EventHandler)
-- import Web.DOM.NonElementParentNode (getElementById)
-- import Web.HTML (window)
-- import Web.HTML.HTMLDocument (toNonElementParentNode)
-- import Web.HTML.Window (document)

-- Define the main React Component
--
-- The 'newMyApp' function here is called from the 'main' function of this program, so this is the
-- actual entry-point of your React program. This function makes use of some boilerplate code which
-- we define in the next section.

-- | The top-level react component must provide an initial state and a rendering function which
-- calls all other rendering functions.
newMyApp :: ReactComponent MyAppProps
newMyApp = toReactComponent myAppPropsFromJS theMyAppClass
  { initialState: myAppInitState
  , render: \ self ->
    let cprops = componentPropsFor self in -- 'componentPropsFor' is defined below.
    DOM.div_
    [ DOM.div_
      [ DOM.span_ [DOM.text "Word:"]
      , DOM.input
        { onChange: textEntryEventHandler self "search" myAppSetSearchString
        , onSubmit: submitEventHandler self
        }
      ]
    , DOM.div_
      [ DOM.span_ [DOM.text "Definition:"]
      , DOM.input
        { onChange: textEntryEventHandler self "definition" myAppSetDefinitionString
        }
      ]
    , DOM.div_
      [ DOM.button
        { children: [DOM.text "Add"]
        , onClick: submitEventHandler self
        }
      , DOM.button
        { children: [DOM.text "Delete"]
        , onClick: Event.capture_ $ do
            self.setState $ unwrap $ myAppDeleteSelected
            logTrue "Deleted selected items"
        }
      ]
    , myDictionaryView cprops
      -- Here we pass 'cprops' to allow stateless components access to the 'state' and 'setState'
      -- function of this main app component.
    ]
  }

-- | This is the submit event handler.
submitEventHandler :: Self {} MyAppState -> EventHandler
submitEventHandler self = Event.capture_ $ do
  let (MyAppState state) = self.state
  self.setState $ unwrap myAppAddWord -- Here we provide a 'StateTransFn'
  logTrue $ "Add word " <>
    show (state.mySearchString) <> " = " <>
    show (state.myDefinitionString)

-- | This is the text input event handler. This function can take one of two 'StateTransFn', which
-- could be 'myAppSetSearchString' or 'myAppSetDefinitionString'. In this way, we can parameterize
-- the event handler over the state transition function.
textEntryEventHandler
  :: Self {} MyAppState
  -> String -- ^ indicate which 'StateTransFn' we are using to report in the log message
  -> (Maybe String -> StateTransFn MyAppState)
  -> EventHandler
textEntryEventHandler self logWhich stransf = Event.capture targetValue $ \ inputString -> do
  self.setState $ unwrap $ stransf inputString
  logTrue $ "Set " <> logWhich <> " string to: " <> show inputString

module Example.Stateless.MyDictEntryProps where

import Prelude 

import Example.AppState (MyAppState(..), WordDefinition)
import Example.LoggingFn (logTrue)
import React.Basic (Component, JSX, createComponent, makeStateless, Self)
import Data.Newtype (unwrap)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as Event
--import Web.DOM.Document (importNode)
import Example.StateTransFn (StateTransFn, myAppWordToggleSelect)
import Effect (Effect)
import Data.Array as Array
import Data.String as String
import Data.String.CodeUnits as Code
import Data.String.Pattern (Pattern(..))


-- | This is the data type we will use for all statelses React components -- let's call these
-- stateless components "views", and the 'ViewProp's contain the properties the view needs to
-- render, including the entire current state of the main component.
-- 
-- React-Basic requires all components constructed with 'makeStateless' take a "properties"
-- value. For the main component (which is not stateless), both properties and state are contained
-- in a record type called Self (see 'React.Basic.Self'). A 'Self' data type cannot contain itself,
-- but we can construct a type that contains the two properties we care about, which are 'state' and
-- 'setState', along with the props for the stateles component.
type ViewProps props state =
    { state    :: state
    , setState :: StateTransFn state -> Effect Unit
    , props    :: props
    }

-- | This function takes a 'Self' value, which is provided by the React Basic environment whenever
-- an event takes place in the DOM, and extracts the two fields that are relevant for use by our
-- stateless view-only React components. The 'ViewProps' type can be used as the "props" type
-- for any stateless component.
componentPropsFor :: forall props state . Self props state -> ViewProps props state
componentPropsFor self =
  { props: self.props
  , state: self.state
  , setState: unwrap >>> self.setState
  }

newtype MyDictEntryProps = MyDictEntryProps MyDictEntryPropsRec

--derive instance newtypeMyDictEntryProps :: Newtype MyDictEntryProps _

type MyDictEntryPropsRec =
    { i     :: Int
    , entry :: WordDefinition
    }

myDictEntryViewClass :: Component (ViewProps MyDictEntryProps MyAppState)
myDictEntryViewClass = createComponent "MyDictEntryView"

myDictEntryView :: ViewProps MyDictEntryProps MyAppState -> JSX
myDictEntryView = makeStateless myDictEntryViewClass $ \ cprops ->
  let (MyDictEntryProps props) = cprops.props in
  let (MyAppState state) = cprops.state in
  DOM.li
  { className:
    if props.entry.isSelected then "dict-entry-view-selected" else "dict-entry-view"
  , children:
    [ DOM.span
      { className: "entry-word"
      , children: [DOM.text props.entry.word]
      }
    , DOM.span
      { className: "entry-separator"
      , children: [DOM.text ":"]
      }
    , DOM.span
      { className: "entry-definition"
      , children: [DOM.text props.entry.definition]
      }
    ]
  , onClick: Event.capture_ $ do
      logTrue $ "Toggle selection on word " <> show props.entry.word
      cprops.setState $ myAppWordToggleSelect props.i
  }

-- Define Stateless Components
--
-- These components are views on the main app's state, and will be provided access to our main app's
-- state via the "properties" value, but they do not have any state of their own.

-- | This function is impure, which violates the rules of PureScript. However, it is necessary to
-- introduce a React component class in the program's top-level so that the react component can be
-- made visible to the React.js library. The string passed to 'createComponent' will be the name of
-- the JavaScript class that is created, so it must be a valid JavaScript class name.
myDictionaryViewClass :: Component (ViewProps {} MyAppState)
myDictionaryViewClass = createComponent "MyDictionaryView"

-- | This component renders a view of the dictionary. It renders a list of items, and each item is
-- also a component.
myDictionaryView :: ViewProps {} MyAppState -> JSX
myDictionaryView = makeStateless myDictionaryViewClass $ \ cprops ->
  let (MyAppState state) = cprops.state in
  DOM.ul
  { className: "dictionary-view"
  , children:
    ( Array.mapWithIndex -- construct the list of elements to be rended
      (\ i entry ->
         { jsx: myDictEntryView (cprops{ props = MyDictEntryProps })
         , word: entry.word
         }
      ) >>>
      ( if String.null state.mySearchString then identity else
        Array.filter -- remove elements that do not match the query string
        (\ jsxWord -> Code.contains (Pattern state.mySearchString) jsxWord.word
        )
      ) >>>
      map (\ jsxWord -> jsxWord.jsx) -- extract the JSX values
    ) $
    state.myDictionary
  } 
 

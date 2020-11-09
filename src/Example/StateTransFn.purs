module Example.StateTransFn where
  
import Prelude

import Data.Array as Array
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Example.AppState (MyAppState)

-- State transition abstractions:
--
-- This will eventually go in a separate module.

newtype StateTransFn a = StateTransFn (a -> a)
  
instance composeStateTransFn :: Semigroup (StateTransFn a) where
  append (StateTransFn a) (StateTransFn b) = StateTransFn (a >>> b)

instance emptyStateTransFn :: Monoid (StateTransFn a) where
  mempty = StateTransFn identity

-- Let's also instantiate 'wrap' and 'unwrap' from the 'Data.Newtype' module.
instance newtypeStateTransFn :: Newtype (StateTransFn a) (a -> a) where
  unwrap (StateTransFn f) = f
  wrap = StateTransFn

-- | This is a constructor for a 'StateTransFn' that automatically unwraps and rewrites a 'Newtype'
-- state, apply the unwrapped record type value to the given function. This is more convenient as it
-- allows you to update the state without unwrapping and re-wrapping it yourself.
stateTransFn
  :: forall outer inner . Newtype outer inner
  => (inner -> inner)
  -> StateTransFn outer
stateTransFn f = StateTransFn (unwrap >>> f >>> wrap)

----------------------------------------------------------------------------------------------------

{- -- | This is the data type we will use for all statelses React components -- let's call these
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
  } -}

-- State Transition Functions

-- Now let's define the state transitions that can occur for this 'MyAppState'. The 'StateTransFn'
-- data type will be defined below, at the end of this module. For now, let's just assume that we
-- need to define functions of type 'StateTransFn' in order to update the state of our app.

-- | Add an element to the dictionary
myAppAddWord :: StateTransFn MyAppState
myAppAddWord = stateTransFn $ \ this ->
  this
  { myDictionary =
    Array.cons
    { word:       this.mySearchString
    , definition: this.myDefinitionString
    , isSelected: false
    }
    this.myDictionary
  }

-- | Toggle whether a dictionary element is selected, given the entry's index in the 'myDictionary'
-- Array.
myAppWordToggleSelect :: Int -> StateTransFn MyAppState
myAppWordToggleSelect i = stateTransFn $ \ this ->
  this
  { myDictionary =
      fromMaybe this.myDictionary $
      Array.modifyAt i
      (\ entry -> entry { isSelected = not entry.isSelected })
      this.myDictionary
  }

-- | Delete selected elements
myAppDeleteSelected :: StateTransFn MyAppState
myAppDeleteSelected = stateTransFn $ \ this ->
  this
  { myDictionary =
    Array.filter (\ entry -> not entry.isSelected) this.myDictionary
  }

-- | When a user inputs a query string, place this query string into the state. This value will be
-- received from an "<input>" element in the DOM, which may give a Nothing value, so the parameter
-- to this function must be of type "Maybe String".
myAppSetSearchString :: Maybe String -> StateTransFn MyAppState
myAppSetSearchString mquery = stateTransFn $ \ this ->
  this
  { mySearchString = fromMaybe "" mquery
  }

-- | When a user inputs a definition string, place this query string into the state. This value will
-- be received from an "<input>" element in the DOM, which may give a Nothing value, so the
-- parameter to this function must be of type "Maybe String".
myAppSetDefinitionString :: Maybe String -> StateTransFn MyAppState
myAppSetDefinitionString mquery = stateTransFn $ \ this ->
  this
  { myDefinitionString = fromMaybe "" mquery
  }
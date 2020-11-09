module Example.AppState where

import Data.Newtype (class Newtype)
-- The App's stateful data
--
-- Here we define the data that contains the internal state of your program. In this example, it is
-- a simple dictionary relating word strings to definition strings.

-- | This could be a 'data MyAppState' type as well, but I prefer to create a 'newtype' so that the
-- | record fields of the type do not get exported to the local namespace. This allows you to reuse
-- | the names of record fields for multiple data types.
newtype MyAppState = MyAppState MyAppStateRec
derive instance newtypeMyAppState :: Newtype MyAppState _

-- | Here we have the record type for your App's state:
type MyAppStateRec =
    { myDictionary       :: Array WordDefinition
    , mySearchString     :: String
    , myDefinitionString :: String
    }

-- | This is the type of the records in our dictionary.
type WordDefinition =
    { word       :: String
    , definition :: String
    , isSelected :: Boolean -- ^ We want a list that can have elements selected.
    }

-- | Create an, initial state for our app, we can put some initial definitions in for testing
-- purposes.
myAppInitState :: MyAppState
myAppInitState = MyAppState
  { myDictionary: testDictionary -- set to [] when done testing
  , mySearchString: ""
  , myDefinitionString: ""
  }

testDictionary :: Array WordDefinition
testDictionary =
  let is word definition = {word, definition, isSelected: false} in
  [ "HTTP" `is` "Hyper-Text Transfer Protocol"
  , "XML" `is` "Extended Markup Language"
  , "XHR" `is` "XML HTTP Request"
  , "CSS" `is` "Cascading Style Sheets"
  , "HTML" `is` "Hyper-Text Markup Language"
  , "JSON" `is` "JavaScript Object Notation"
  , "AJAX" `is` "Asynchronous JavaScript and XML"
  , "REST" `is` "REpresentational State Transfer"
  , "ACID" `is` "Atomicity, Consistency, Isolation, Durability"
  , "CRUD" `is` "Create, Read, Update, Delete"
  , "REPL" `is` "Read, Eval, Print, Loop"
  , "UTF" `is` "Universal Text Format"
  , "ASCII" `is` "American Standard Code for Information Interchange"
  ]
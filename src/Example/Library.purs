module Example.Library where
  
import Prelude
import React.Basic (Component, createComponent)

-- React Component Boilerplate
--
-- This code is necessary for all React-Basic programs that are to be run in the web browser

-- | Define the data type that is to be received from the any React components using this program
-- itself as a component. This is only necessary in React Basic programs that are to be used as a
-- library of React components. Since this program is the app itself, not a library, this data type
-- should be empty.
type MyAppProps = {}

myAppInitProps :: MyAppProps
myAppInitProps = {}

-- | This function does nothing because 'MyAppProps' is empty.
myAppPropsFromJS :: forall jsProps . { | jsProps } -> MyAppProps
myAppPropsFromJS = const myAppInitProps

-- | This function is impure, which violates the rules of PureScript. However, it is necessary to
-- introduce a React component class in the program's top-level so that the react component can be
-- made visible to the React.js library. The string passed to 'createComponent' will be the name of
-- the JavaScript class that is created, so it must be a valid JavaScript class name.
theMyAppClass :: Component MyAppProps
theMyAppClass = createComponent "MyApp"
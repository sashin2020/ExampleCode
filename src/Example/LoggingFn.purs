module Example.LoggingFn where

import Prelude
import Effect (Effect)
import Effect.Console as Console

logTrue :: String -> Effect Unit
logTrue =  Console.log


logFalse :: String -> Effect Unit
logFalse _ = pure unit
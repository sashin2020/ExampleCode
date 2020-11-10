module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Example.Library (myAppInitProps)
import Example.LoggingFn (logFalse)
import Example.MyAppProps (newMyApp)

import React.Basic (element)
import React.Basic.DOM as DOM
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  logFalse "Begin Facade main function"
  doc <- window >>= document
  logFalse "Get the element identified as \"container\" from the DOM of index.html"
  maybeContainer <- getElementById "container" $ toNonElementParentNode doc
  case maybeContainer of
    Nothing        -> throw "Container element not found"
    Just container -> DOM.render (element newMyApp myAppInitProps) container


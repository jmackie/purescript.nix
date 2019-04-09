module Main where

import Prelude
import Effect (Effect)
import Effect.Console as Console
import Data.Five (five)

main :: Effect Unit
main = Console.log (show five)

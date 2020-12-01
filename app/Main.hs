module Main where

import Dimmer
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("dim":_) -> dim
    ("restore":_) -> restore

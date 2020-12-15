module Main where

import Test.Hspec
import qualified DimmerSpec

main :: IO ()
main = hspec $ do
  describe "DimmerSpec" DimmerSpec.spec

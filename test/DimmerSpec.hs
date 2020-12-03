module DimmerSpec where

import Dimmer
import Test.Hspec

spec = describe "Dimmer genIncrements" $ do
  it "should create a decreasing list of values when target > current" $ do
    genIncrements 10 1 5 `shouldBe` [10, 9, 8, 7, 6, 5]

  it "should create an increasing list of values when current < target" $ do
    genIncrements 5 1 7 `shouldBe` [5, 6, 7]

  it "should return a the current brightness if current == target" $ do
    genIncrements 5 1 5`shouldBe` [5]

  it "should generate the appropriate increments" $ do
    let res = genIncrements 6 2 10
    case res of
      (a:b:_) -> abs (a - b) `shouldBe` 2

  it "should return the current brigntness for zero increments" $ do
    genIncrements 100 0 5000 `shouldBe` [100]

  it "should return the current brightness for invalid increments" $ do
    genIncrements 100 (-1) 5000 `shouldBe` [100]





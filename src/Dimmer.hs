module Dimmer (dim, restore, genIncrements) where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           System.Directory

targetDimDurationMs = 300
targetUndimDurationMs = 300
targetFPS = 60
targetDimBrightness= 100
lastBrightnessFile = "/home/fahad/.local/lastbrightness"
brightnessFile = "/sys/class/backlight/intel_backlight/brightness"
-- >>> usPerFrame
-- 16666
usPerFrame = div (1 * 1000000) targetFPS

dim :: IO ()
dim = do
  alreadyDimmed <- doesFileExist lastBrightnessFile
  unless alreadyDimmed $ do
    currentB <- readB brightnessFile
    writeB lastBrightnessFile currentB
    transition targetDimBrightness targetDimDurationMs currentB


restore :: IO ()
restore = do
  dimmed <- doesFileExist lastBrightnessFile
  when dimmed $ do
    lastB <- readB lastBrightnessFile
    currentB <- readB brightnessFile
    removeFile lastBrightnessFile
    when (currentB <= lastB) $ do
      transition lastB targetUndimDurationMs currentB


readB :: FilePath -> IO Int
readB f = do
    val <- readFile f
    return (read val :: Int)

writeB :: FilePath -> Int -> IO ()
writeB f b = writeFile f $ show b

{-|
   transition will call genIncrements to generate brightness
   increments and apply them to the brightnessFile with a fixed
   delay in between each apply
-}
transition :: Int -> Int -> Int -> IO ()
transition targetB targetD currentB = forM_ bIncrements $ \b -> do
  writeB brightnessFile b
  threadDelay usPerFrame
  where inc = (targetB - currentB) `div` ((targetFPS * targetD) `div` 1000)
        bIncrements = genIncrements currentB inc targetB

genIncrements :: Int -> Int -> Int -> [Int]
genIncrements current inc target =
  case [target, target - inc..current] of
    []  -> [current]
    all -> reverse all

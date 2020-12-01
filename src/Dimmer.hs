module Dimmer (dim, restore) where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           System.Directory

targetDimDurationMs = 300
targetUndimDurationMs = 300
targetFPS = 60
targetDimBrightness= 100
usPerFrame = div (1 * 1000000) targetFPS
lastBrightnessFile = "/home/fahad/.local/lastbrightness"
brightnessFile = "/sys/class/backlight/intel_backlight/brightness"

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

transition :: Int -> Int -> Int -> IO ()
transition targetB targetD currentB = forM_ bIncrements $ \b -> do
  writeB brightnessFile b
  threadDelay usPerFrame
  where inc = (currentB - targetB) `div` ((targetFPS * targetD) `div` 1000)
        --endCondition = if targetB > currentB then (>=) else (<=)
        bIncrements = genIncrements currentB inc targetB

-- >>> genIncrements 5000 500 100
-- [5000,4500,4000,3500,3000,2500,2000,1500,1000,500,100]
-- >>> genIncrements 100 (-500) 5000
-- [100,600,1100,1600,2100,2600,3100,3600,4100,4600,5000]
-- >>> genIncrements 100 0 5000
-- [100]
-- >>> genIncrements 100 500 5000
-- [5000]
genIncrements :: Int -> Int -> Int -> [Int]
genIncrements current 0 _ = [current]
genIncrements current inc target
  | compareF current target = [target]
  | otherwise = current : genIncrements (current - inc) inc target
  where compareF = if inc < 0 then (>=) else (<=)


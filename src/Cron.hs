module Cron where

import Control.Concurrent

multiplier :: Int
multiplier = 10 ^ (6 :: Int)

cron :: Int -> IO () -> IO a
cron seconds0 action = loop seconds0
    where maxSeconds = (maxBound :: Int) `div` multiplier
          loop seconds =
              if seconds <= maxSeconds
                then do threadDelay (multiplier * seconds)
                        action
                        loop seconds0
                else do threadDelay (multiplier * maxSeconds)
                        loop (seconds - maxSeconds)

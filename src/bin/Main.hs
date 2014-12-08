module Main where
import Stubby (stubby, wait)
import Stubby.Settings (stubbyArgs)

main :: IO ()
main = stubbyArgs >>= stubby >>= wait

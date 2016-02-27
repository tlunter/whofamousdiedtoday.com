module Main where

import Control.Concurrent (threadDelay)
import Data.Maybe (isNothing)
import Data.Text (Text)

import DeathFinder

printListings :: Maybe Text -> IO ()
printListings pid = do
    print $ "Checking after " ++ show pid
    (newPid, deadListings) <- listings pid
    print deadListings
    threadDelay 10000000
    if isNothing newPid
        then printListings pid
        else printListings newPid

main :: IO ()
main = printListings Nothing

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Time
import Network
import System.Environment (lookupEnv)

import Database.Redis
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as BL (toStrict)

import DeathFinder

redisConnectionInfo :: IO ConnectInfo
redisConnectionInfo = do
    possibleHost <- lookupEnv "REDIS_HOST"
    possiblePort <- lookupEnv "REDIS_PORT"
    let host = fromMaybe "localhost" possibleHost
        port = fromInteger (read $ fromMaybe "6379" possiblePort :: Integer)
    return defaultConnectInfo { connectHost = host, connectPort = PortNumber port }

getListings :: Maybe Text -> IO (Maybe Text, [PossibleDeath])
getListings pid = do
    print $ "Checking after " ++ show pid
    (newPid, deadListings) <- listings pid
    print deadListings
    return (newPid, deadListings)

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = fmap (toGregorian . utctDay) getCurrentTime

saveListings' :: [PossibleDeath] -> Redis (Either Reply Integer)
saveListings' deaths = do
    (year, month, day) <- liftIO date
    let key = "deaths:" ++ show year ++ ":" ++ show month ++ ":" ++ show day
        keyBS = B.pack key
    rpush keyBS $ map (BL.toStrict . encode) deaths

saveListings :: Maybe Text -> Redis ()
saveListings pid = do
    (newPid, deadListings) <- liftIO $ getListings pid
    saveListings' deadListings
    liftIO $ threadDelay 10000000
    saveListings newPid

main :: IO ()
main = do
    connectInfo <- redisConnectionInfo
    conn <- connect connectInfo
    runRedis conn $ saveListings Nothing

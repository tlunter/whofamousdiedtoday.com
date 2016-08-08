{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay, forkIO, yield)
import Control.Monad (forever, mapM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, mapMaybe, catMaybes)
import Data.Text (Text)
import Data.Text.Lazy (pack)
import Data.Time
import Network
import System.Environment (lookupEnv)

import Database.Redis (Redis, Reply, Connection, ConnectInfo, keys, lrange, rpush, runRedis, defaultConnectInfo, connect, connectHost, connectPort)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Internal as BI (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString, toStrict, fromStrict)
import Web.Scotty as Scotty

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

getRecordedListings :: Connection -> IO [PossibleDeath]
getRecordedListings conn = runRedis conn $ do
    possibleDeathDates <- keys "deaths:*"
    let deathDates = either (const []) id possibleDeathDates
    possibleDeaths <- mapM (\x -> lrange x 0 (-1)) deathDates
    let deaths = concatMap (either (const []) id) possibleDeaths
    return $ mapMaybe (decode . BL.fromStrict) deaths

webServer :: Connection -> IO ()
webServer conn = do
    index <- readFile "../../frontend/index.html"
    elm <- readFile "../../frontend/target/elm.js"
    scotty 3000 $ do
        get "/deaths" $ liftIO (getRecordedListings conn) >>= json
        get "/elm.js" $ html $ Data.Text.Lazy.pack elm
        get "/" $ html $ Data.Text.Lazy.pack index

main :: IO ()
main = do
    connectInfo <- redisConnectionInfo
    conn <- connect connectInfo
    forkIO $ runRedis conn $ saveListings Nothing
    forkIO $ liftIO $ webServer conn
    forever $ threadDelay 10000000

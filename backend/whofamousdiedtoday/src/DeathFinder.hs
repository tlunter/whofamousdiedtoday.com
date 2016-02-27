{-# LANGUAGE OverloadedStrings #-}
module DeathFinder
    ( listings
    , PossibleDeath
    , firstName
    , lastName
    , link
    ) where

import Data.Either
import Data.Maybe
import Data.List (filter, head)
import Data.Text (Text, isInfixOf, pack)

import Reddit
import Reddit.Actions.Post
import Reddit.Types.Listing
import Reddit.Types.Options (Options, limit, pagination)
import Reddit.Types.Post (Post, PostID, postID, title, permalink)

import TitleParser

data PossibleDeath =
    PossibleDeath { firstName :: Text
                  , lastName :: Text
                  , link :: Text
                  }
    deriving (Show, Eq)

defaultOptions = Options { limit = Nothing, pagination = Nothing }

newListings :: Maybe PostID -> Reddit PostListing
newListings pid = getPosts' (options pid) New Nothing
    where options (Just x) = defaultOptions { pagination = Just $ Before x }
          options Nothing  = defaultOptions

convertMatch :: Match -> Post -> PossibleDeath
convertMatch match post =
    PossibleDeath { firstName = pack fName
                  , lastName = pack lName
                  , link = plink
                  }
    where fName = matchFirstName match
          lName = matchLastName match
          plink = permalink post

deadPost :: Post -> Maybe PossibleDeath
deadPost post = if isJust match
                    then Just $ convertMatch (fromJust match) post
                    else Nothing
    where match = parseTitle $ title post

filterListings :: [Post] -> [PossibleDeath]
filterListings = mapMaybe deadPost

firstPid :: [Post] -> Maybe Text
firstPid [] = Nothing
firstPid x  = Just . fix . postID $ head x
    where fix (PostID y) = y

listings' :: Maybe Text -> IO (Either (APIError RedditError) (Maybe Text, [PossibleDeath]))
listings' pid = runRedditAnon $ do
    unfilteredListings <- newListings $ makePid pid
    let filteredListings = filterListings $ contents unfilteredListings
        newPid           = firstPid       $ contents unfilteredListings
    return (newPid, filteredListings)
    where makePid Nothing = Nothing
          makePid (Just x) = Just (PostID x)

listings :: Maybe Text -> IO (Maybe Text, [PossibleDeath])
listings pid = listings' pid >>= finish
    where finish (Left x) = print x >> return (Nothing, [])
          finish (Right x) = return x

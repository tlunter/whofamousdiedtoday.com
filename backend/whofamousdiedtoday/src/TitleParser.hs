module TitleParser
    ( parseTitle
    , Match
    , matchFirstName
    , matchLastName
    ) where

import Data.Char (isAlphaNum)
import Data.Maybe
import Data.Text

import Text.Parsec
import Text.Parsec.Text

import CommonNames

data Match = Match { matchFirstName :: String
                   , matchLastName :: String
                   , matchAge :: Maybe Int
                   }
                deriving (Show)

diedExpressions :: Parser String
diedExpressions = satisfy (not . isAlphaNum) >> exprs
    where exprs = died <|> dead <|> passed <|> passes
          died = string "died"
          dead = string "dead"
          passed = string "passed away"
          passes = string "passes away"

nameExpression :: Parser (String, String)
nameExpression = do
    firstName <- Prelude.foldl1 (\a e -> (a <|> try (satisfy (not . isAlphaNum) >> e))) $ fmap string commonNames
    skipMany1 space
    lastName <- many1 letter
    return (firstName, lastName)

ageExpression :: Parser Int
ageExpression = do
    satisfy (not . isAlphaNum)
    string "age"
    many letter
    skipMany1 space
    age <- many1 digit
    return $ read age

yearExpression :: Parser Int
yearExpression = do
    satisfy (not . isAlphaNum)
    age <- many1 digit
    skipMany1 space
    string "years old"
    return $ read age

maybeParse :: Text -> Parser a -> Maybe a
maybeParse t p = either (const Nothing) Just $ parse p "" t

deadTitle :: Text -> Maybe Match
deadTitle title =
    case died of
        Nothing -> Nothing
        Just x -> case names of
            Nothing -> Nothing
            Just (first, last) -> Just Match { matchFirstName = first
                                             , matchLastName  = last
                                             , matchAge       = age
                                             }
    where died  = maybeParse title diedExprs
          names = maybeParse title nameExpr
          age   = maybeParse title ageExpr
          diedExprs = try diedExpressions <|> (anyChar >> diedExprs)
          nameExpr  = try nameExpression <|> (anyChar >> nameExpr)
          ageExpr   = try ageExpression <|> try yearExpression <|> (anyChar >> ageExpr)

parseTitle :: Text -> Maybe Match
parseTitle title = deadTitle (toLower title)

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

data Match = Match { matchFirstName :: String
                   , matchLastName :: String
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
    x <- upper
    xs <- many letter
    spaces
    y <- upper
    ys <- many letter
    return (x:xs, y:ys)

deadTitle :: Parser (Maybe Match)
deadTitle = do
    lookAhead diedExpr
    (first, last) <- lookAhead nameExpr
    return $ Just Match { matchFirstName = first, matchLastName = last }
    where diedExpr = try diedExpressions <|> (anyChar >> diedExpr)
          nameExpr = try nameExpression <|> (anyChar >> nameExpr)

parseTitle :: Text -> Maybe Match
parseTitle title = either (const Nothing) id $ parse deadTitle "" title

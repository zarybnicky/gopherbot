{- 
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module RobotsTxt  where

-- FIXME: should only consider first user-agent match?

import Text.ParserCombinators.Parsec
import Data.String.Utils
import Network.URI

ws :: Parser String
ws = many (oneOf " \v\f\t")

eol :: Parser String
eol = string "\n" <|> string "\r\n" -- <|> (eof >> return "")

badline :: Parser String
badline = many (noneOf "\r\n") >> eol

comment :: Parser String
comment = do
  _ <- char '#'
  _ <- many (noneOf "\r\n")
  eol

toeol :: Parser String
toeol = ws >> (eol <|> comment)

emptyline :: Parser String
emptyline = try comment <|> toeol

value :: Parser String
value = many (noneOf "#\t\n\r")

defline :: String -> Parser String
defline key = string key >> kv

kv :: Parser String
kv = do
  _ <- ws
  _ <- char ':'
  _ <- ws
  v <- value
  _ <- toeol
  return (rstrip v)

line :: String -> Parser String
line key = try (defline key) <|> (kv >> fail "foo") <|> (emptyline >> line key)

useragent :: Parser String
useragent = try (line "User-agent")

disallow :: Parser String
disallow = try (line "Disallow")

clauses :: Parser [([String], [String])]
clauses = do
  agents <- many useragent
  disallows <- many disallow
  if null agents
    then fail "Found no User-agent"
    else ((agents, disallows) :) <$> try clauses <|> return []

{- | Parse a robots.txt file and return a list corresponding to a clause.
Each tuple in the list contains a list of user agents that the rule applies to,
plus a list of Disallow records. -}
parseRobots :: FilePath -> IO [([String], [String])]
parseRobots fp = do
  r <- parseFromFile clauses fp
  case r of
    Left _ -> return []
    Right x -> return x

{- | Given a parsed file, a user agent, and a URL, determine whether
it's OK to process that URL. -}
isURLAllowed :: [([String], [String])] -> String -> String -> Bool
isURLAllowed parsed agent url =
  let agentsfiltered =
        filter (\i -> "*" `elem` fst i || agent `elem` fst i) parsed
      disallowparts = concatMap snd agentsfiltered
      escapedurl = escapeURIString (`notElem` " ?\n\r\0&") url
   in not (any (\i -> startswith i url || startswith i escapedurl) disallowparts)

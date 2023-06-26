{-# LANGUAGE OverloadedStrings #-}

module TinyRegex.Parser
( Parser
) where

import TinyRegex.Core (RegexAST(..), Predicate(..))
import qualified Data.Text as Text
import Control.Applicative ((<|>))
import Data.Char (isDigit, isSpace, isAlphaNum,  toLower)

{- Parser with no dependencies outside of Data.Text.
 -
 - Supports the following regex characters:
 - + * and ?
 - [a-zA-Z0-9] (character classes)
 - (a|b) (alternatives + capture groups)
 -
 - Should parse the following examples:
 - - ab*c+(a|b+){2,}[a-zA-Z0-9]{2, 6}
 -}

type Parser a = Either Text.Text a
type MaybeParse a = Maybe (a, Text.Text)

parse :: Text.Text -> Parser a
parse = undefined

trace :: Text.Text -> Parser a -> Parser a
trace x (Left e) = (Left . Text.concat) [ e, "\nString: ", x ]
trace _ p = p

consMP :: a -> MaybeParse [a] -> MaybeParse [a]
x `consMP` Just (xs, t)   = Just (x:xs, t)
x `consMP` Nothing        = Just ([x], Text.empty)

{- A little 'choice' combinator like Megaparsec's.
 - As a side note, the Semigroup instance for Either e a is funny:
 - https://hackage.haskell.org/package/base-4.18.0.0/docs/src/Data.Either.html#line-140 -}

catchError :: Parser a -> (Text.Text -> Parser a) -> Parser a
catchError (Left x) fn = fn x
catchError x _ = x

isWordChar :: Char -> Bool
isWordChar x = isAlphaNum x || x == '_'

escapeChar :: Char -> Char
escapeChar x = case x of
   'n' -> '\n'
   'f' -> '\f'
   'b' -> '\b'
   't' -> '\t'
   _   -> x

charClass :: Char -> Char -> Bool
charClass 'd' = isDigit
charClass 's' = isSpace
charClass 'w' = isWordChar
charClass 'D' = not . isDigit
charClass 'S' = not . isSpace
charClass 'W' = not . isWordChar
charClass _   = undefined -- This should never be reached.

reserved :: [Char]
reserved  = [ '*', '+', '.', '?', '{', '[', '(', ')', '|' ]

operators :: [Char]
operators = [ '*', '+', '?', '{' ]

parseChar :: (Char -> Bool) -> Text.Text -> MaybeParse Char
parseChar fn txt = Text.uncons txt >>= \char@(x, _) ->
    if fn x then Just char else Nothing

parseSingle :: (Char -> Bool) -> Text.Text -> MaybeParse RegexAST
parseSingle fn txt = parseChar fn txt >>= \(x, rest) -> Just (Single x, rest)

parseEscape :: Text.Text -> MaybeParse RegexAST
parseEscape txt = parseChar (== '\\') txt >>= \(x, rest) -> do
    if toLower x `elem` [ 's', 'd', 'w' ]
        then let special = CharacterClass . Predicate . charClass
             in Just (special x, rest)
        else let escape = Single . escapeChar
             in Just (escape x, rest)

parseToken :: Text.Text -> MaybeParse [RegexAST]
parseToken txt
    | Text.null txt = Nothing
    | t == '{' = undefined
    | t == '[' = undefined
    | t == '(' = undefined
    | otherwise = do
        let char = parseEscape txt <|> parseSingle (`notElem` reserved) txt
        char >>= \(x, rest) -> x `consMP` parseToken rest
    where t = Text.head txt

followedBy :: (Char -> Bool) -> Text.Text -> Bool
followedBy fn txt = (not . Text.null) txt || (fn . Text.head) txt

notFollowedBy :: (Char -> Bool) -> Text.Text -> Bool
notFollowedBy = (not .) . followedBy

parseEsc :: Text.Text -> MaybeParse Char
parseEsc txt = parseChar (const True) txt >>= \(x, rest) -> Just (escapeChar x, rest)

parseSeq :: Text.Text -> MaybeParse [Char]
parseSeq txt
    | Text.null txt = Nothing
    | t == '\\' = parseEsc txt >>= \(x, rest) ->
        if followedBy (`elem` operators) rest
            then Nothing
            else x `consMP` parseSeq rest
    | otherwise = parseChar (`notElem` reserved) txt >>= \(x, rest) ->
        if followedBy (`elem` operators) rest
            then Nothing
            else x `consMP` parseSeq rest
    where t = Text.head txt

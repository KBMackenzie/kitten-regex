{-# LANGUAGE OverloadedStrings #-}

module TinyRegex.Parser
( Parser
, parse
, parseSeq
, parseChar
, parseClass'
) where

import TinyRegex.Core (RegexAST(..), Predicate(..))
import TinyRegex.Pretty (prettyError)
import qualified Data.Text as Text
import Control.Applicative ((<|>), empty, liftA2)
import Data.Char (isDigit, isSpace, isAlphaNum,  toLower)
import Data.Bifunctor (first, second)
import Text.Read (readMaybe)
import Data.List (singleton)

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

type Parser a = Either Text.Text (a, Text.Text)
type MaybeParse a = Maybe (a, Text.Text)


{- Helpers-}
-----------------------------------------------------------------
parse :: Text.Text -> Parser [RegexAST]
parse txt = trace txt (parseTokens txt)

trace :: Text.Text -> Parser a -> Parser a
trace x (Left e) = (Left . Text.concat) [ e, "\nString: ", x ]
trace _ p = p

consMP :: (a, Text.Text) -> MaybeParse [a] -> MaybeParse [a]
(x, _ ) `consMP` Just (xs, t2)  = Just (x:xs, t2)
(x, t1) `consMP` Nothing        = Just ([x],  t1)

mpAsParser :: MaybeParse a -> Parser a
mpAsParser (Just x) = Right x
mpAsParser Nothing  = Left "Unexpected input: "

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
charClass x   = (== x) -- This should never be reached.

reserved :: [Char]
reserved  = [ '*', '+', '.', '?', '{', '}', '[', ']', '(', ')', '|', '-' ]

operators :: [Char]
operators = [ '*', '+', '?', '{' ]

closing :: [Char]
closing = [ '}', ']', ')' ]

followedBy :: (Char -> Bool) -> Text.Text -> Bool
followedBy fn txt = not (Text.null txt) && (fn . Text.head) txt

notFollowedBy :: (Char -> Bool) -> Text.Text -> Bool
notFollowedBy = (not .) . followedBy


{- Parsing -}
-----------------------------------------------------------------
parseChar :: (Char -> Bool) -> Text.Text -> MaybeParse Char
parseChar fn txt = Text.uncons txt >>= \char@(x, _) ->
    if fn x then Just char else Nothing

parseSpecial :: Text.Text -> MaybeParse Predicate
parseSpecial txt = parseChar (== '\\') txt >>= \(x, rest) ->
    if toLower x `elem` [ 's', 'd', 'w' ]
        then let special = Predicate . charClass
             in Just (special x, rest)
        else Nothing

parseEscape :: Text.Text -> MaybeParse Char
parseEscape txt = first escapeChar <$> parseChar (const True) txt 

singleChar :: (Char -> Bool) -> Text.Text -> MaybeParse Char
singleChar isValid txt = case Text.uncons txt of
    Nothing -> Nothing
    (Just ('\\', xs)) -> parseEscape xs
    (Just (x,  rest)) -> if isValid x then Just (x, rest) else Nothing

parseSeq :: (Char -> Bool) -> Text.Text -> MaybeParse [Char]
parseSeq isValid txt = singleChar isValid txt >>= \(x, rest) ->
    if followedBy (`elem` operators) rest
        then Nothing
        else (x, rest) `consMP` parseSeq isValid rest

parseText :: Text.Text -> MaybeParse RegexAST
parseText txt = foldl (<|>) empty
    [ first (const AnyChar) <$> parseChar (== '.') txt
    , first CharacterClass  <$> parseSpecial txt
    , first (Verbatim . Text.pack) <$> parseSeq (`notElem` reserved) txt
    , first (Verbatim . Text.singleton) <$> parseChar (`notElem` reserved) txt ]


{- Character class [a-zA-Z0-9] -}
-----------------------------------------------------------------
validClassChar :: Char -> Bool
validClassChar = flip notElem [ '\\', ']' ]

charRange :: Text.Text -> MaybeParse (Char -> Bool)
charRange txt = do
    (a, r1) <- singleChar validClassChar txt
    (_, r2) <- parseChar (== '-') r1
    (b, r3) <- singleChar validClassChar r2
    Just ((`elem` [a..b]), r3)

charPred :: Text.Text -> MaybeParse (Char -> Bool)
charPred txt = first (==) <$> singleChar validClassChar txt

classChars :: Text.Text -> MaybeParse [Char -> Bool]
classChars txt = let chars = charRange txt <|> charPred txt
    in chars >>= \(x, rest) -> (x, rest) `consMP` classChars rest

parseClass' :: Text.Text -> Parser RegexAST
parseClass' txt = case classChars txt of
    (Just (fs, r1)) -> case snd <$> parseChar (== ']') r1 of
        (Just rest) -> do
            let ps = foldl (liftA2 (||)) (const False) fs
            Right (makeToken ps, rest)
        Nothing     -> Left (prettyError "Unclosed character class:" r1)
    Nothing -> Left (prettyError "Invalid character in character class:" txt)
    where makeToken = CharacterClass . Predicate


{- Numeric range {2, 6} -}
-----------------------------------------------------------
singleNum :: Text.Text -> MaybeParse (RegexAST -> RegexAST)
singleNum txt = parseSeq isDigit txt >>= \(s, r1) -> do
    n <- readMaybe s :: Maybe Int
    (_, r2) <- parseChar (== '}') r1
    Just (Count n, r2)

numRange :: Text.Text -> MaybeParse (RegexAST -> RegexAST)
numRange txt = do 
    (s1, r1) <- parseSeq isDigit txt
    let a = readMaybe s1 :: Maybe Int
    (_,  r2) <- parseChar (== ',') r1
    (s2, r3) <- parseSeq isDigit r2
    let b = readMaybe s2 :: Maybe Int
    (_ , r4) <- parseChar (== '}') r3
    Just (CountRange a b, r4)

parseRange :: Text.Text -> MaybeParse (RegexAST -> RegexAST)
parseRange txt = singleNum txt <|> numRange txt


{- Parse groups (a, b) -}
-----------------------------------------------------------
parseGroup :: Text.Text -> Parser RegexAST
parseGroup txt = do
    (xs, r1) <- parseTokens txt
    case parseChar (== ')') r1 of
        (Just (_, r2)) -> Right (MatchGroup xs, r2)
        Nothing -> Left (prettyError "Unclosed group:" txt)


{- Parse groups (a, b) -}
-----------------------------------------------------------
parseOps :: Text.Text -> MaybeParse (RegexAST -> RegexAST)
parseOps txt = foldl (<|>) empty
    [ first (const MatchStar) <$> parseChar (== '*') txt
    , first (const MatchPlus) <$> parseChar (== '+') txt
    , first (const MatchQues) <$> parseChar (== '?') txt ]


{- Parse token -}
-----------------------------------------------------------
parseToken :: Text.Text -> Parser RegexAST
parseToken txt
    | x == '('              = parseGroup xs
    | x == '['              = parseClass' xs
    | x `notElem` closing   = case parseText txt of
        (Just (y, rest)) -> Right (y, rest)
        Nothing          -> Left unexpected
    | otherwise             = Left unexpected
    where -- Intentionally partial function.
        x = Text.head txt
        xs = Text.tail txt
        unexpected = prettyError "Unexpected character:" txt

parseToken' :: Text.Text -> Parser RegexAST
parseToken' txt = do
    (tok, r1) <- parseToken txt
    Left ""

parseTokens :: Text.Text -> Parser [RegexAST]
parseTokens txt
    | Text.null txt = Right ([], txt)
    | otherwise = parseToken txt >>= \(x, rest) -> first (x:) <$> parseTokens rest

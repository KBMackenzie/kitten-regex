{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module KittenRegex.Internal.Parser
( Parser
, parseExpr
, parseRegex
, isWordChar
) where

import KittenRegex.Internal.Core
import qualified Data.Text as Text
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad (void, liftM2)
import Data.Maybe (isJust)
import Data.Char (isDigit, isAlphaNum, isSpace)
import Data.Void (Void)

type Parser = Mega.Parsec Void Text.Text

{- Parsing helpers: -}
-----------------------------------------------------
braces :: Parser a -> Parser a
braces = Mega.between (MChar.char '{') (MChar.char '}')

parens :: Parser a -> Parser a
parens = Mega.between (MChar.char '(') (MChar.char ')')

brackets :: Parser a -> Parser a
brackets = Mega.between (MChar.char '[') (MChar.char ']')

reserved :: [Char]
reserved = [ '*', '.', '?', '+', '^', '$', '{', '(', ')', '[', ']', '|', '\\' ]

operators :: [Char]
operators = [ '*', '?', '+', '{' ]

isOperator :: Parser Bool
isOperator = True <$ Mega.satisfy (`elem` operators)

isWordChar :: Char -> Bool
isWordChar x = isAlphaNum x || x == '_'

escapeChar :: [Char] -> Parser Char
escapeChar xs = Mega.choice
    [ '*'   <$ MChar.string "\\*"
    , '.'   <$ MChar.string "\\."
    , '?'   <$ MChar.string "\\?"
    , '+'   <$ MChar.string "\\+"
    , '^'   <$ MChar.string "\\^"
    , '$'   <$ MChar.string "\\$"
    , '('   <$ MChar.string "\\("
    , ')'   <$ MChar.string "\\)"
    , '['   <$ MChar.string "\\["
    , ']'   <$ MChar.string "\\]"
    , '|'   <$ MChar.string "\\|"
    , '-'   <$ MChar.string "\\-"
    , '\\'  <$ MChar.string "\\\\"
    , '\"'  <$ MChar.string "\\\""
    , '\n'  <$ MChar.string "\\n"
    , '\b'  <$ MChar.string "\\b"
    , '\t'  <$ MChar.string "\\t"
    , '\f'  <$ MChar.string "\\f"
    , '\r'  <$ MChar.string "\\r"
    , '{'   <$ (MChar.char '{' >> Mega.notFollowedBy MChar.digitChar)
    , Mega.satisfy (`notElem` xs) ] <?> "valid character"

{- Parsing operators: -}
-----------------------------------------------------
parsePlus :: Parser (RegexAST -> RegexAST)
parsePlus = ASTMatchPlus <$ MChar.char '+'

parseStar :: Parser (RegexAST -> RegexAST)
parseStar = ASTMatchStar <$ MChar.char '*'

parseQues :: Parser (RegexAST -> RegexAST)
parseQues = ASTMatchQues <$ MChar.char '?'

parseCount :: Parser (RegexAST -> RegexAST)
parseCount = ASTCount <$> braces Lexer.decimal <?> "integer number"

postfixes :: [Parser (RegexAST -> RegexAST)]
postfixes = 
    [ parsePlus
    , parseStar
    , parseQues ]

--manyPostfixes :: Parser (RegexAST -> RegexAST)
--manyPostfixes = foldl1 (flip (.)) <$> Mega.some parsePostfix

parsePostfix :: Parser (RegexAST -> RegexAST)
parsePostfix = Mega.choice postfixes

{- Parsing numeric ranges (e.g. a{2, 5}) -}
parseRange :: Parser (RegexAST -> RegexAST)
parseRange = braces $ do
    start <- Mega.optional Lexer.decimal <?> "start of range"
    (void . MChar.char) ','
    end   <- Mega.optional Lexer.decimal <?> "end of range"
    return (ASTCountRange start end)

parseNumRange :: Parser (RegexAST -> RegexAST)
parseNumRange = Mega.try parseCount <|> parseRange


{- Parsing tokens: -}
-----------------------------------------------------
parseDot :: Parser RegexAST
parseDot = ASTAnyChar <$ MChar.char '.'

parseChar :: Parser RegexAST
parseChar = ASTVerbatim . Text.singleton <$> escapeChar reserved

parseVerbatim :: Parser RegexAST
parseVerbatim = ASTVerbatim . Text.pack <$> Mega.some char
    where char = Mega.try $ escapeChar reserved <* Mega.notFollowedBy isOperator

parseSpecial :: Parser (Char -> Bool)
parseSpecial = Mega.choice
    [ isDigit              <$ MChar.string "\\d"
    , isSpace              <$ MChar.string "\\s"
    , isWordChar           <$ MChar.string "\\w"
    , (not . isDigit)      <$ MChar.string "\\D"
    , (not . isSpace)      <$ MChar.string "\\S"
    , (not . isWordChar)   <$ MChar.string "\\W" ]

parseSpecial' :: Parser RegexAST
parseSpecial' = ASTCharacterClass . Predicate <$> parseSpecial

-- Reserved chars in character classes.
classReserved :: [Char]
classReserved = [ '\\', ']', '^' ]

{- Parsing character ranges (e.g. [a-Z]) -}
charRange :: Parser (Char -> Bool)
charRange = do
    a <- escapeChar classReserved
    (void . MChar.char) '-'
    b <- escapeChar classReserved
    return (\x -> x `elem` [a..b])

parseCharClass :: Parser RegexAST
parseCharClass = brackets $ do
    isNot <- isJust <$> Mega.optional (MChar.char '^')
    fns <- Mega.many (Mega.try charRange <|> parseSpecial <|> charPred)
    let predicate = foldr (liftM2 (||)) (const False) fns
    (return . ASTCharacterClass . Predicate) (if isNot then not . predicate else predicate)
    where
        char = escapeChar classReserved <* Mega.notFollowedBy (MChar.char '-')
        charPred = (==) <$> Mega.try char

{- Parsing capture groups (e.g. a(b*)c) -}
parseGroup :: Parser RegexAST
parseGroup = Mega.try nonCaptureGroup <|> captureGroup

captureGroup :: Parser RegexAST
captureGroup = ASTCaptureGroup <$> parens parseTokens

nonCaptureGroup :: Parser RegexAST
nonCaptureGroup = ASTNonCaptureGroup <$> parens (MChar.string "?:" >> parseTokens)

parseStart :: Parser RegexAST
parseStart = ASTTokenStart <$ MChar.char '^'

parseEnd :: Parser RegexAST
parseEnd = ASTTokenEnd <$ MChar.char '$' 

{- Combining everything: -}
parseTerm :: Parser RegexAST
parseTerm = Mega.choice
    [ parseStart        <?> "start (^)"
    , parseEnd          <?> "eol ($)"
    , parseGroup        <?> "capture group"
    , parseCharClass    <?> "character class"
    , parseSpecial'     <?> "character class"
    , parseDot          <?> "dot (.)"
    , parseVerbatim     <?> "sequence of characters"
    , parseChar         <?> "single character"      ]

parseToken :: Parser RegexAST
parseToken = do
    term  <- parseTerm
    p1    <- parseNumRange <|> return id
    p2    <- parsePostfix <|> return id
    (return . p2 . p1) term

parseAlt :: Parser ([RegexAST] -> RegexAST)
parseAlt = flip ASTAlternative <$> (MChar.char '|' >> parseTokens)

parseTokens :: Parser [RegexAST]
parseTokens = do
    xs <- Mega.many parseToken
    Mega.optional parseAlt >>= \case
        Nothing   -> return xs
        (Just fn) -> return [fn xs]

parseExpr :: Parser [RegexAST]
parseExpr = parseTokens <* Mega.eof

parseRegex :: Text.Text -> Either Text.Text [RegexAST]
parseRegex contents = case Mega.parse parseExpr "<regex>" contents of
    (Left x)  -> (Left . Text.pack . errorBundlePretty) x
    (Right x) -> Right x

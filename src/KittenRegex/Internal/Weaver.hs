module KittenRegex.Internal.Weaver
( RegexBuilder(..)
, string
, char
, charWhere
, digit
, space
, letter
, wordChar
, anyChar
, anyAmountOf
, optional
, oneOrMore
, amountOf
, amountBetween
, minimumOf
, maximumOf
, capture
, (<.|>)
, toRegex
, startOfLine
, endOfLine
) where

import KittenRegex.Internal.Core (Regex(..), RegexAST(..), Predicate(..))
import KittenRegex.Internal.Compile (compile)
import KittenRegex.Internal.Parser (isWordChar)
import qualified Data.Text as Text
import Data.Char (isDigit, isSpace, isLetter)

data RegexList =
      RegexNode RegexAST RegexList
    | RegexEnd
    deriving (Show)

class RegexBuilder a where
    -- I'm unsure about the exact number for the precedence.
    infixr 6 <.+>
    (<.+>) :: RegexAST -> a -> RegexList
    single :: a -> RegexList

instance RegexBuilder RegexList where
    -- O(1) appending.
    expr <.+> list = RegexNode expr list
    single = id

instance RegexBuilder RegexAST where
    -- O(1) appending.
    -- This allows two expressions to create a list together.
    a <.+> b = RegexNode a (RegexNode b RegexEnd)
    single = flip RegexNode RegexEnd

toList :: RegexList -> [RegexAST]
toList (RegexNode r next) = r : toList next
toList RegexEnd           = []

toRegex :: (RegexBuilder a) => a -> Regex
toRegex = Regex . compile . asList

asList :: (RegexBuilder a) => a -> [RegexAST]
asList = toList . single

{- Builders: -}
----------------------------------------------------------
-- Aliases that make building regexes far more intuitive: 
string :: String -> RegexAST
string = ASTVerbatim . Text.pack

char :: Char -> RegexAST
char = ASTVerbatim . Text.singleton

charWhere :: (Char -> Bool) -> RegexAST
charWhere = ASTCharacterClass . Predicate

digit :: RegexAST
digit = charWhere isDigit

space :: RegexAST
space = charWhere isSpace

letter :: RegexAST
letter = charWhere isLetter

wordChar :: RegexAST
wordChar = charWhere isWordChar

anyChar :: RegexAST
anyChar = ASTAnyChar

{- Modifiers -}
---------------------------------------------------------------------------------------
-- They're polymorphic and wrapped in non-capture groups to make them easier to combine.
-- This has a compile-time cost, but after compilation there are no real differences.

groupUp :: (RegexBuilder a) => a -> RegexAST
groupUp = ASTNonCaptureGroup . asList

anyAmountOf :: (RegexBuilder a) => a -> RegexAST
anyAmountOf = ASTMatchStar . groupUp

optional :: (RegexBuilder a) => a -> RegexAST
optional = ASTMatchQues . groupUp

oneOrMore :: (RegexBuilder a) => a -> RegexAST
oneOrMore = ASTMatchPlus . groupUp

amountOf :: (RegexBuilder a) => Int -> a -> RegexAST
amountOf n = ASTCount n . groupUp

amountBetween :: Int -> Int -> RegexAST -> RegexAST
amountBetween a b = ASTCountRange (Just a) (Just b) . groupUp

minimumOf :: Int -> RegexAST -> RegexAST
minimumOf a = ASTCountRange (Just a) Nothing . groupUp

maximumOf :: Int -> RegexAST -> RegexAST
maximumOf b = ASTCountRange Nothing (Just b) . groupUp

-- Polymorphic capture group function.
-- It accepts both a single expression and a whole list.
capture :: (RegexBuilder a) => a -> RegexAST
capture = ASTCaptureGroup . asList

-- Polymorphic alternative infix function.
-- It accepts both a single expression and a whole list.
(<.|>) :: (RegexBuilder a, RegexBuilder b) => a -> b -> RegexAST
(<.|>) a b = ASTAlternative (asList a) (asList b)

{- Special combinators: -}
startOfLine :: RegexAST
startOfLine = ASTTokenStart

endOfLine :: RegexAST
endOfLine = ASTTokenEnd

{-# LANGUAGE OverloadedStrings #-}

module TinyRegex.Internal.Weaver
( RegexBuilder(..)
, RegexAST(..)
, example
) where

import TinyRegex.Internal.Core (Regex(..), RegexAST(..), Predicate(..))
import TinyRegex.Internal.Compile (compile)
import TinyRegex.Internal.Regexable(Regexable(..))
import qualified Data.Text as Text
import Data.Bifunctor (bimap)

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

toRegex :: RegexList -> Regex
toRegex = Regex . compile . toList

asList :: (RegexBuilder a) => a -> [RegexAST]
asList = toList . single

example :: RegexList
example = ASTVerbatim "c" <.+> ASTVerbatim "a" <.+> ASTVerbatim "b"

{- Builders: -}
----------------------------------------------------------
-- Aliases that make building regexes far more intuitive: 
anyAmountOf :: RegexAST -> RegexAST
anyAmountOf = ASTMatchStar

optional :: RegexAST -> RegexAST
optional = ASTMatchQues

oneOrMore :: RegexAST -> RegexAST
oneOrMore = ASTMatchPlus

string :: Text.Text -> RegexAST
string = ASTVerbatim

char :: Char -> RegexAST
char = ASTVerbatim . Text.singleton

charWhere :: (Char -> Bool) -> RegexAST
charWhere = ASTCharacterClass . Predicate

anyChar :: RegexAST
anyChar = ASTAnyChar

-- Polymorphic group-maker function.
-- It accepts both a single expression and a whole list.
groupWhere :: (RegexBuilder a) => a -> RegexAST
groupWhere = ASTMatchGroup . asList

-- Polymorphic alternative function.
-- It accepts both a single expression and a whole list.
alternative :: (RegexBuilder a, RegexBuilder b) => a -> b -> RegexAST
alternative a b = ASTAlternativeGroup (asList a) (asList b)

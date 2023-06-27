{-# LANGUAGE OverloadedStrings #-}

module TinyRegex.Core
( RegexAST(..)
, Regex(..)
, RegexOutput(..)
, RegexComp(..)
, Predicate(..)
) where

import qualified Data.Text as Text

newtype Predicate = Predicate { getPredicate :: Char -> Bool }
instance Show Predicate where show _ = "<predicate>"
newtype Regex = Regex [RegexComp]

data RegexOutput = RegexOutput
    { groups    :: [(Int, Text.Text)]
    , leftovers :: Text.Text          } 
    deriving (Show)

data RegexAST =
      Verbatim Text.Text
    | AnyChar
    | CharacterClass Predicate
    | Count Int RegexAST
    | CountRange (Maybe Int) (Maybe Int) RegexAST
    | AlternativeGroup [RegexAST] [RegexAST]
    | MatchGroup [RegexAST]
    | MatchStar RegexAST
    | MatchQues RegexAST
    | MatchPlus RegexAST
    | TokenStart | TokenEnd
    deriving (Show)

data RegexComp =
      Sequence Text.Text
    | Character Predicate
    | ZeroOrOne [RegexComp]
    | ZeroOrMany [RegexComp]
    | Alternative [RegexComp] [RegexComp]
    | GroupStart Int | GroupEnd Int | Start | End
    deriving (Show)

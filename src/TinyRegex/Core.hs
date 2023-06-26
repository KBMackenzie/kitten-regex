{-# LANGUAGE OverloadedStrings #-}

module TinyRegex.Core
( Predicate(..)
, RegexAST(..)
, RegexComp(..)
, RegexOutput(..)
, RegexEngine
) where

import qualified Data.Text as Text

newtype Predicate = Predicate { getPredicate :: Char -> Bool }
instance Show Predicate where show _ = "<predicate>"

data RegexAST =
      Verbatim Text.Text
    | Single Char
    | CharacterClass Predicate
    | Count Int RegexAST
    | CountRange (Maybe Int) (Maybe Int) RegexAST
    | AlternativeGroup [RegexAST] [RegexAST]
    | MatchGroup [RegexAST]
    | LineStart | LineEnd
    deriving (Show)

data RegexComp =
      Sequence Text.Text
    | Character Predicate
    | ZeroOrOne RegexComp
    | Alternative RegexComp RegexComp
    | GroupStart Int | GroupEnd Int
    deriving (Show)

data RegexOutput =
      MatchOutput Text.Text
    | GroupLabel Int Bool
    deriving (Show)

type RegexEngine = Maybe ([RegexOutput], Text.Text)

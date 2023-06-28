{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TinyRegex.Internal.Core
( RegexAST(..)
, Regex(..)
, RegexOutput(..)
, RegexComp(..)
, Predicate(..)
, getGroup
) where

import qualified Data.Text as Text

newtype Predicate = Predicate { getPredicate :: Char -> Bool }
instance Show Predicate where show _ = "<predicate>"

-- A compiled regex newtype, meant to be used as an opaque type.
newtype Regex = Regex [RegexComp]

data RegexOutput = RegexOutput
    { groups    :: [(Int, Text.Text)]
    , leftovers :: Text.Text          } 
    deriving (Eq, Show)

getGroup :: RegexOutput -> Int -> Maybe Text.Text
getGroup = flip lookup . groups

data RegexAST =
      ASTVerbatim Text.Text
    | ASTAnyChar
    | ASTCharacterClass Predicate
    | ASTCount Int RegexAST
    | ASTCountRange (Maybe Int) (Maybe Int) RegexAST
    | ASTAlternative [RegexAST] [RegexAST]
    | ASTCaptureGroup [RegexAST]
    | ASTNonCaptureGroup [RegexAST]
    | ASTMatchStar RegexAST
    | ASTMatchQues RegexAST
    | ASTMatchPlus RegexAST
    | ASTTokenStart
    | ASTTokenEnd
    deriving (Show)

data RegexComp =
      Sequence Text.Text
    | Character Predicate
    | ZeroOrOne [RegexComp]
    | ZeroOrMany [RegexComp]
    | Alternative [RegexComp] [RegexComp]
    | GroupStart Int | GroupEnd Int | Start | End
    deriving (Show)

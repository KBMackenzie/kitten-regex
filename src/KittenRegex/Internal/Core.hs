{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module KittenRegex.Internal.Core
( RegexAST(..)
, Regex(..)
, RegexOutput(..)
, RegexComp(..)
, Predicate(..)
, getGroup
) where

import qualified Data.Text as Text
import qualified Data.IntMap.Strict as Map

newtype Predicate = Predicate { getPredicate :: Char -> Bool }
instance Show Predicate where show _ = "<predicate>"

-- A compiled regex newtype, meant to be used as an opaque type.
newtype Regex = Regex [RegexComp]

data RegexOutput a = RegexOutput
    { groups    :: Map.IntMap a
    , leftovers :: a            }
    deriving (Eq, Show)

instance Functor RegexOutput where
    fmap f (RegexOutput xs l) = RegexOutput (Map.map f xs) (f l)

getGroup :: RegexOutput a -> Int -> Maybe a
getGroup = flip Map.lookup . groups

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

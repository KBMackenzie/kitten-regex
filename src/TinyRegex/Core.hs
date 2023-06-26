{-# LANGUAGE OverloadedStrings #-}

module TinyRegex.Core
( Predicate(..)
, RegexAST(..)
, RegexComp(..)
, RegexOutput(..)
, RegexEngine
, ParserMonad(..)
) where

import qualified Data.Text as Text
import Control.Applicative (Alternative, empty, (<|>))

newtype Predicate = Predicate { getPredicate :: Char -> Bool }
instance Show Predicate where show _ = "<predicate>"

data RegexAST =
      Verbatim Text.Text
    | AnyChar
    | CharacterClass Predicate
    | Count Int RegexAST
    | CountRange (Maybe Int) (Maybe Int) RegexAST
    | AlternativeGroup [RegexAST] [RegexAST]
    | MatchGroup [RegexAST]
    | MatchStar RegexAST
    | MatchPlus RegexAST
    | MatchQues RegexAST
    | LineStart | LineEnd
    | EmptyTransition
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


{- ParserMonad -}
----------------------------------------------------

data ParserMonad a =
      Parsing a
    | Failure 
    | ParseError Text.Text

instance Functor ParserMonad where
    fmap _ Failure          = Failure
    fmap _ (ParseError e)   = ParseError e
    fmap f (Parsing x)      = Parsing (f x)

instance Applicative ParserMonad where
    pure = Parsing
    Failure <*> _ = Failure
    Parsing f <*> x = f <$> x
    ParseError e <*> _ = ParseError e

instance Monad ParserMonad where
    return = pure
    Parsing x >>= f = f x
    Failure >>= _ = Failure
    ParseError e >>= _ = ParseError e

{- I'm unsure if this satisfies the laws of the Alternative typeclass.
 - It looooooooooks like it does? -}
instance Alternative ParserMonad where
    empty = Failure
    Failure <|> x = x
    x <|> _ = x

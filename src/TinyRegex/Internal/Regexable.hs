{-# LANGUAGE OverloadedStrings #-}

module TinyRegex.Internal.Regexable
( Regexable(..)
, ReString(..)
, getGroup
) where

import TinyRegex.Internal.Core
import TinyRegex.Internal.Evaluate 
import qualified Data.Text as Text
import Data.Maybe (isJust)
import Data.String (IsString(..))

class Regexable a where
    -- Safely compile regex.
    buildEither :: a -> Either Text.Text Regex
    -- Safely run a match.
    match :: Regex -> a -> Maybe RegexOutput

    -- A partial function meant to be used only with constant regex strings
    -- that you're sure have no syntax errors.
    -- You should not use this if you can't ensure the validity of the input.
    build :: a -> Regex
    build re = case buildEither re of
        (Left  e)   -> (error . Text.unpack . message) e
        (Right x)   -> x
        where message = Text.append "Couldn't build regex from input:\n"

    isMatch :: Regex -> a -> Bool
    isMatch = (isJust .) . match

    -- Infix alias for 'match'.
    (<.*>) :: Regex -> a -> Maybe RegexOutput
    (<.*>) = match

    -- Infix alias for 'isMatch'.
    (<.?>) :: Regex -> a -> Bool
    (<.?>) = isMatch

{- Text -}
------------------------------------------------------
-- Regexable instance for Data.Text.
-- It has more efficient functions than the default.
instance Regexable Text.Text where
    buildEither = regexBuild
    match = regexMatch

    -- A more efficient 'isMatch' definition than the
    -- default definition provided in the typeclass.
    isMatch (Regex re) = isJust . runStart re

{- String -}
------------------------------------------------------
-- A small string wrapper so that I can define an instance of Regexable
-- for strings at all, since that's sadly a limitation.
newtype ReString = ReString { unReString :: String } deriving (Eq, Ord, Show)

-- Regexable instance for String/[Char].
-- It just packs the string, that's all.
instance Regexable ReString where
    buildEither = buildEither . Text.pack . unReString
    match regex = match regex . Text.pack . unReString

-- Instance of 'IsString' so that ReString can be
-- used with the 'OverloadedStrings' extension.
instance IsString ReString where
    fromString = ReString

{- Helper functions: -}
-------------------------------------------------
getGroup :: RegexOutput -> Int -> Maybe Text.Text
getGroup = flip lookup . groups

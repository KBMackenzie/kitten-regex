{-# LANGUAGE OverloadedStrings #-}

module TinyRegex.Regexable
( Regexable(..)
, getGroup
) where

import TinyRegex.Core
import TinyRegex.Evaluate 
import qualified Data.Text as Text
import Data.Maybe (isJust)

class Regexable a where
    -- Safely compile a regex and match.
    -- The two required functions:
    buildEither :: a -> Either Text.Text Regex
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

    -- Infix operator for 'match'.
    (<.*>) :: Regex -> a -> Maybe RegexOutput
    (<.*>) = match

    -- Infix operator for 'isMatch'.
    (<.?>) :: Regex -> a -> Bool
    (<.?>) = isMatch


-- Regexable instance for Data.Text.
-- It has more efficient functions than the default.
instance Regexable Text.Text where
    buildEither :: Text.Text -> Either Text.Text Regex
    buildEither = regexBuild

    match :: Regex -> Text.Text -> Maybe RegexOutput
    match = regexMatch

    -- A more efficient 'isMatch' definition than the
    -- default definition provided in the typeclass.
    isMatch :: Regex -> Text.Text -> Bool
    isMatch (Regex re) = isJust . runStart re


-- Regexable instance for String/[Char].
-- It just packs the string, that's all.
instance Regexable String where
    buildEither = buildEither . Text.pack
    match regex input  = match regex (Text.pack input)


{- Helper functions: -}
-------------------------------------------------
getGroup :: RegexOutput -> Int -> Maybe Text.Text
getGroup = flip lookup . groups

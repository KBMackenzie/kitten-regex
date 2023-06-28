{-# LANGUAGE OverloadedStrings #-}

module TinyRegex.Internal.Regexable
( Regexable(..)
, ReString(..)
, buildRe
) where

import TinyRegex.Internal.Core
import TinyRegex.Internal.Evaluate 
import qualified Data.Text as Text
import Data.Maybe (isJust)
import Data.String (IsString(..))

class Regexable a where
    -- Safely compile regex.
    buildEither :: a -> Either Text.Text Regex
    -- Safely run match.
    match :: Regex -> a -> Maybe RegexOutput
    split :: Regex -> a -> Maybe [a]
    replace :: Regex -> a -> a -> Maybe a

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
instance Regexable Text.Text where
    buildEither = regexBuild
    match = regexMatch
    split = regexSplit
    replace = regexReplace

    -- A more efficient 'isMatch' definition.
    isMatch (Regex re) = isJust . runStart re

{- String -}
------------------------------------------------------
-- A small string wrapper so that I can define an instance of Regexable for String,
-- since String is just an alias for [Char].
newtype ReString = ReString { unReString :: String } deriving (Eq, Ord, Show)

-- Pack ReString into Text.
rePack :: ReString -> Text.Text
rePack = Text.pack . unReString

-- Unpack Text into ReString.
reUnpack :: Text.Text -> ReString
reUnpack = ReString . Text.unpack

-- Regexable instance for String/[Char].
-- It just packs the string, that's all.
instance Regexable ReString where
    buildEither = buildEither . Text.pack . unReString
    match regex = match regex . Text.pack . unReString
    split regex input = map reUnpack <$> split regex (rePack input)
    replace regex input r = reUnpack <$> replace regex (rePack input) (rePack r)

-- Instance of 'IsString' so that ReString can be used with the 'OverloadedStrings' extension.
instance IsString ReString where
    fromString = ReString

{- Helper functions: -}
-------------------------------------------------
buildRe :: String -> Regex
buildRe = build . ReString

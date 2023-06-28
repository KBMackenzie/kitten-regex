{-# LANGUAGE OverloadedStrings #-}

module KittenRegex.Internal.Regexable
( Regexable(..)
, ReString(..)
, buildRe
, unReStringAll
) where

import KittenRegex.Internal.Core
import KittenRegex.Internal.Evaluate 
import qualified Data.Text as Text
import Data.String (IsString(..))
import qualified Data.Text.Encoding as TextEnc
import qualified Data.ByteString as Byte
import Data.Maybe (isJust)
import Control.Monad ((>=>))

class Regexable a where
    -- Safely compile regex.
    buildEither :: a -> Either Text.Text Regex
    -- Safely run match.
    match :: Regex -> a -> Maybe (RegexOutput a)
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
    (<.*>) :: Regex -> a -> Maybe (RegexOutput a)
    (<.*>) = match

    -- Infix alias for 'isMatch'.
    (<.?>) :: Regex -> a -> Bool
    (<.?>) = isMatch

    matchMany :: Regex -> a -> [RegexOutput a]
    matchMany regex input = case match regex input of
        Nothing -> []
        (Just output) -> output : matchMany regex (leftovers output)

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
newtype ReString = ReString { unReString :: String } deriving (Eq, Ord)
instance Show ReString where show = show . unReString

-- Pack ReString into Text.
rePack :: ReString -> Text.Text
rePack = Text.pack . unReString

-- Unpack Text into ReString.
reUnpack :: Text.Text -> ReString
reUnpack = ReString . Text.unpack

-- Regexable instance for String/[Char].
-- It just packs the string, that's all.
instance Regexable ReString where
    buildEither = buildEither . rePack
    match regex input = fmap reUnpack <$> match regex (rePack input)
    split regex input = fmap reUnpack <$> split regex (rePack input)
    replace regex input r = reUnpack <$> replace regex (rePack input) (rePack r)
    isMatch regex = isMatch regex . rePack

-- Instance of 'IsString' so that ReString can be used with the 'OverloadedStrings' extension.
instance IsString ReString where
    fromString = ReString


{- ByteString -}
------------------------------------------------------
-- A Regexable instance for UTF8-encoded (!!) ByteStrings.
-- It contains no partial functions and the decoding functions either return an error
-- in an Either or return Nothing in the 'match' and 'split' functions.
--
-- The downside of all of this is that in the 'match', 'split' and 'replace' functions
-- decoding errors and failed matches are indistinguishable from one another, since
-- they both return Nothing. Optimally they would return something else, but I really
-- don't wanna change the core definition of the Regexable typeclass JUST for this. I
-- don't think a lot of people use Regex with ByteStrings like this either way.
--
-- TL;DR: If you're using UTF16/UTF32 ByteStrings, it'd be safer for you to decode them
-- into Text or String and use those for regexing. The RegexOutput type is a Functor, 
-- so you can encode the entire input to ByteString later on if you want to with fmap.
instance Regexable Byte.ByteString where
    buildEither = safeDecode >=> buildEither
    match regex input = fmap TextEnc.encodeUtf8 <$> (match regex =<< maybeDecode input)
    split regex input = fmap TextEnc.encodeUtf8 <$> (split regex =<< maybeDecode input)
    replace regex input r = TextEnc.encodeUtf8 <$> do
        input' <- maybeDecode input
        replacement' <- maybeDecode r
        replace regex input' replacement'

safeDecode :: Byte.ByteString -> Either Text.Text Text.Text
safeDecode bstr = case TextEnc.decodeUtf8' bstr of
    (Left x) -> (Left . Text.pack . show) x
    (Right x) -> Right x

maybeDecode :: Byte.ByteString -> Maybe Text.Text
maybeDecode = eitherToMaybe . safeDecode

{- Helper functions: -}
-------------------------------------------------
buildRe :: String -> Regex
buildRe = build . ReString

unReStringAll :: RegexOutput ReString -> RegexOutput String
unReStringAll = fmap unReString

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe e = case e of
    (Left _) -> Nothing
    (Right x) -> Just x
    

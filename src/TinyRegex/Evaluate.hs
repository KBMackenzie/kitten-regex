{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StrictData #-}

module TinyRegex.Evaluate
( regexBuild
, regexMatch
, regexMatchT
, runStart
) where

import TinyRegex.Core
import qualified Data.Text as Text
import Control.Applicative ((<|>), empty)
import Data.Functor ((<&>))
import TinyRegex.Parser (parseRegex)
import TinyRegex.Engine (compile)
import Data.List (singleton, groupBy, nub)
import Data.Bifunctor (first)

type Input = Text.Text
type Output = [Evaluated]
type Evaluator = Maybe (Output, Input)
data Evaluated =
      TextOutput Text.Text
    | LabelStart Int
    | LabelEnd Int
    deriving (Show)

evalPack :: Text.Text -> [Evaluated]
evalPack =  singleton . TextOutput

evalPackC :: Char -> [Evaluated]
evalPackC = evalPack . Text.singleton

consMP :: Output -> (Output, Input) -> (Output, Input)
consMP x = first (x ++)

evaluate :: Input -> [RegexComp] -> Evaluator
evaluate input (x:xs) = case x of
    (Sequence str) -> Text.stripPrefix str input >>= continue xs . (evalPack str,)
    (Character fn) -> Text.uncons input >>=
        \(c, rest) -> if getPredicate fn c
            then (evalPackC c `consMP`) <$> evaluate rest xs
            else empty
    (ZeroOrMany zs) -> do --todo: done: fixed mistake in here
        let pathA = evaluate input zs >>= continue (x:xs)
        let pathB = evaluate input xs
        pathA <|> pathB
    (ZeroOrOne  zs) -> do
        let pathA = evaluate input zs >>= continue xs
        let pathB = evaluate input xs
        pathA <|> pathB
    (Alternative as bs) -> do
        let runPath ts = evaluate input (ts ++ xs)
        runPath as <|> runPath bs
    Start -> evaluate input xs
    End -> if Text.null input then Just ([], Text.empty) else Nothing
    (GroupStart n) -> first ([LabelStart n] ++) <$> evaluate input xs
    (GroupEnd n)   -> first ([LabelEnd   n] ++) <$> evaluate input xs
evaluate input [] = Just ([], input)

continue :: [RegexComp] -> (Output, Input) -> Evaluator
continue toks (output, rest) = (output `consMP`) <$> evaluate rest toks

runStart :: [RegexComp] -> Input -> Evaluator
runStart (x@Start:xs) input = do
    let pathA = evaluate input xs
    let pathB = Text.uncons input >>= flip evaluate (x:xs) . snd
    pathA <|> pathB
runStart xs input = evaluate input xs

{- Building -}
---------------------------------------------------------------------
regexBuild :: Text.Text -> Either Text.Text Regex
regexBuild = fmap (Regex . compile) . parseRegex

{- Evaluator -}
---------------------------------------------------------------------
regexMatch :: Regex -> Input -> Maybe RegexOutput
regexMatch (Regex re) input = runStart re input <&> makeOutput

-- todo: add:
-- regex split, regex replace

-- Abstractions over types of input:
regexMatchT :: Text.Text -> Input -> Either Text.Text RegexOutput
regexMatchT regex input = regexBuild regex >>= \x -> case regexMatch x input of
    Nothing -> (Left . Text.concat) [ "No match. | Regex: ", regex, " | Input: ", input ]
    (Just output) -> return output

--runRegex :: Regex -> Input -> Either Text.Text [(Int, Text.Text)]
--runRegex regex input = runSingleRe regex input <&> getGroups . fst

--runRegexC :: Regex -> Input -> Either Text.Text [(Int, Text.Text)]
--runRegexC (Regex re) input = runStart re input <&> getGroups . fst

{- Groups -}
---------------------------------------------------------------------
takeGroups :: Int -> [Text.Text] -> [Evaluated] -> [(Int, Text.Text)]
takeGroups n ts [] = [(n, (Text.concat . reverse) ts)]
takeGroups n ts ((LabelStart x) : xs) = takeGroups x [] xs ++ takeGroups n ts xs
takeGroups n ts ((LabelEnd   x) : xs) = if x == n
    then [(n, (Text.concat . reverse) ts)]
    else takeGroups n ts xs
takeGroups n ts ((TextOutput x) : xs) = takeGroups n (x:ts) xs

getGroups :: [Evaluated] -> [(Int, Text.Text)]
getGroups = zip [0..] . reverse . map joinGroup . nub . groupBy sameGroup . takeGroups 0 []
    where sameGroup a b = fst a == fst b
          joinGroup = Text.concat . map snd

makeOutput :: (Output, Input) -> RegexOutput
makeOutput (xs, rest) = RegexOutput { groups = getGroups xs, leftovers = rest }

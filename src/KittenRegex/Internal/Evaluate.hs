{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StrictData #-}

module KittenRegex.Internal.Evaluate
( regexBuild
, regexMatch
, regexMatchT
, runStart
, regexSplit
, regexReplace
) where

import KittenRegex.Internal.Core
import qualified Data.Text as Text
import Control.Applicative ((<|>), empty)
import Data.Functor ((<&>))
import KittenRegex.Internal.Parser (parseRegex)
import KittenRegex.Internal.Compile (compile)
import Data.Bifunctor (first, second)
import Data.List (singleton)
import Control.Monad.State (State, execState, modify)
import qualified Data.IntMap.Strict as Map

type Input = Text.Text
type Output = [Labels]
type Evaluation = (Output, Input)
type Evaluator = Maybe Evaluation
type IsStart = Bool

data Labels =
      TextOutput Text.Text
    | LabelStart Int
    | LabelEnd Int
    deriving (Show)

labelPack :: Text.Text -> [Labels]
labelPack =  singleton . TextOutput

labelChar :: Char -> [Labels]
labelChar = labelPack . Text.singleton

consOutput :: Output -> Evaluation -> Evaluation
consOutput x = first (x ++)

evaluate :: Input -> [RegexComp] -> IsStart -> Evaluator
evaluate input (x:xs) s = case x of
    (Sequence str) -> Text.stripPrefix str input >>= continue xs . (labelPack str,)
    (Character fn) -> Text.uncons input >>=
        \(c, rest) -> if getPredicate fn c
            then (labelChar c `consOutput`) <$> evaluate rest xs False
            else empty
    (ZeroOrMany zs) -> do
        let pathA = evaluate input zs s >>= continue (x:xs)
        let pathB = evaluate input xs s
        pathA <|> pathB
    (ZeroOrOne  zs) -> do
        let pathA = evaluate input zs s >>= continue xs
        let pathB = evaluate input xs s
        pathA <|> pathB
    (Alternative as bs) -> do
        let runPath ts = evaluate input (ts ++ xs) s
        runPath as <|> runPath bs
    Start -> if s then evaluate input xs False else empty
    End -> ([],) <$> isEOL input
    (GroupStart n) -> first ([LabelStart n] ++) <$> evaluate input xs s
    (GroupEnd n)   -> first ([LabelEnd   n] ++) <$> evaluate input xs s
evaluate input [] _ = Just ([], input)

continue :: [RegexComp] -> Evaluation -> Evaluator
continue toks (output, rest) = (output `consOutput`) <$> evaluate rest toks False

runStart :: IsStart -> [RegexComp] -> Input -> Evaluator
runStart s xs input = do
    let pathA = evaluate input xs s
    let pathB = Text.uncons input >>= runStart False xs . snd
    pathA <|> pathB

runStart' :: IsStart -> [RegexComp] -> Int -> Input -> Maybe (Int, Evaluation)
runStart' s xs n input = do
    let pathA = (n,) <$> evaluate input xs s
    let pathB = Text.uncons input >>= runStart' False xs (succ n) . snd
    pathA <|> pathB

isEOL :: Input -> Maybe Text.Text
isEOL input = if Text.null input
    then Just input
    else Text.stripPrefix "\n" input <|> Text.stripPrefix "\r\n" input <|> Text.stripPrefix "\r" input

{- Building -}
---------------------------------------------------------------------
regexBuild :: Text.Text -> Either Text.Text Regex
regexBuild = fmap (Regex . compile) . parseRegex

{- Evaluator -}
---------------------------------------------------------------------
regexMatch :: Regex -> Input -> Maybe (RegexOutput Text.Text)
regexMatch (Regex re) input = runStart True re input <&> makeOutput

-- Abstractions over types of input:
regexMatchT :: Text.Text -> Input -> Either Text.Text (RegexOutput Text.Text)
regexMatchT regex input = regexBuild regex >>= \x -> case regexMatch x input of
    Nothing -> (Left . Text.concat) [ "No match. | Regex: ", regex, " | Input: ", input ]
    (Just output) -> return output


{- Groups -}
---------------------------------------------------------------------
type GroupMap = Map.IntMap Text.Text

getGroups :: Int -> [Labels] -> GroupMap -> GroupMap
getGroups _ [] m = m
getGroups n ((TextOutput x) : xs) m = getGroups n xs (Map.insertWith (flip Text.append) n x m)
getGroups n ((LabelStart x) : xs) m = getGroups n xs (getGroups x xs m)
getGroups n ((LabelEnd   x) : xs) m = if x == n then m else getGroups n xs m

runGetGroups :: [Labels] -> GroupMap
runGetGroups xs = getGroups 0 xs Map.empty

makeOutput :: (Output, Input) -> RegexOutput Text.Text
makeOutput (xs, rest) = RegexOutput { groups = runGetGroups xs, leftovers = rest }


{- Splitting + Replacing -}
---------------------------------------------------------------------
regexSplit :: Regex -> Input -> Maybe [Text.Text]
regexSplit = regexSplit' True

regexSplit' :: IsStart -> Regex -> Input -> Maybe [Text.Text]
regexSplit' s re@(Regex regex) input = do
    (n, xs) <- second makeOutput <$> runStart' s regex 0 input 
    let start = Text.take n input
    let end = leftovers xs
    Just [start] <> (regexSplit' False re end <|> Just [end])

regexReplace :: Regex -> Input -> Text.Text -> Maybe Text.Text
regexReplace = regexReplace' True

regexReplace' :: IsStart -> Regex -> Input -> Text.Text -> Maybe Text.Text
regexReplace' s re@(Regex regex) input replacement = do
    (n, xs) <- second makeOutput <$> runStart' s regex 0 input
    let start = Text.take n input
    let end = leftovers xs
    let str = start `Text.append` replacement
    Just str <> (regexReplace' False re end replacement <|> Just end)

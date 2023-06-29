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
import Control.Monad.State (State, execState , modify)
import qualified Data.IntMap.Strict as Map

type Input = Text.Text
type Output = [Labels]
type Evaluation = (Output, Input)
type Evaluator = Maybe Evaluation

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

evaluate :: Input -> [RegexComp] -> Evaluator
evaluate input (x:xs) = case x of
    (Sequence str) -> Text.stripPrefix str input >>= continue xs . (labelPack str,)
    (Character fn) -> Text.uncons input >>=
        \(c, rest) -> if getPredicate fn c
            then (labelChar c `consOutput`) <$> evaluate rest xs
            else empty
    (ZeroOrMany zs) -> do
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
    End -> ([],) <$> isEOL input
    (GroupStart n) -> first ([LabelStart n] ++) <$> evaluate input xs
    (GroupEnd n)   -> first ([LabelEnd   n] ++) <$> evaluate input xs
evaluate input [] = Just ([], input)

continue :: [RegexComp] -> Evaluation -> Evaluator
continue toks (output, rest) = (output `consOutput`) <$> evaluate rest toks

runStart :: [RegexComp] -> Input -> Evaluator
runStart (Start:xs) input = evaluate input xs
runStart xs input = do
    let pathA = evaluate input xs
    let pathB = Text.uncons input >>= runStart xs . snd
    pathA <|> pathB

runStart' :: [RegexComp] -> Int -> Input -> Maybe (Int, Evaluation)
runStart' (Start:xs) n input = (n,) <$> evaluate input xs
runStart' xs n input = do
    let pathA = (n,) <$> evaluate input xs
    let pathB = Text.uncons input >>= runStart' xs (succ n) . snd
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
regexMatch (Regex re) input = runStart re input <&> makeOutput

-- Abstractions over types of input:
regexMatchT :: Text.Text -> Input -> Either Text.Text (RegexOutput Text.Text)
regexMatchT regex input = regexBuild regex >>= \x -> case regexMatch x input of
    Nothing -> (Left . Text.concat) [ "No match. | Regex: ", regex, " | Input: ", input ]
    (Just output) -> return output


{- Groups -}
---------------------------------------------------------------------
type GroupMap a = State (Map.IntMap Text.Text) a

-- Yes, I know this is a dirty little hack, but I enjoy using the State
-- monad in this way when I have to keep track of state through so many
-- layers of recursion.
getGroups :: Int -> [Labels] -> GroupMap ()
getGroups _ [] = return ()
getGroups n ((TextOutput x) : xs) = do
    let f = flip Text.append
    modify (Map.insertWith f n x) >> getGroups n xs
getGroups n ((LabelStart x) : xs) = getGroups x xs >> getGroups n xs
getGroups n ((LabelEnd   x) : xs) = if x == n
    then return ()
    else getGroups n xs

runGetGroups :: [Labels] -> Map.IntMap Text.Text
runGetGroups = flip execState Map.empty . getGroups 0

makeOutput :: (Output, Input) -> RegexOutput Text.Text
makeOutput (xs, rest) = RegexOutput { groups = runGetGroups xs, leftovers = rest }


{- Splitting + Replacing -}
---------------------------------------------------------------------
regexSplit :: Regex -> Input -> Maybe [Text.Text]
regexSplit re@(Regex regex) input = do
    (n, xs) <- second makeOutput <$> runStart' regex 0 input 
    let start = Text.take n input
    let end = leftovers xs
    Just [start] <> (regexSplit re end <|> Just [end])

regexReplace :: Regex -> Input -> Text.Text -> Maybe Text.Text
regexReplace re@(Regex regex) input replacement = do
    (n, xs) <- second makeOutput <$> runStart' regex 0 input
    let start = Text.take n input
    let end = leftovers xs
    let str = start `Text.append` replacement
    Just str <> (regexReplace re end replacement <|> Just end)

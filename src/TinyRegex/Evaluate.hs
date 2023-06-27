{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TinyRegex.Evaluate
( runEvaluator
, runRegex
) where

import TinyRegex.Core
import qualified Data.Text as Text
import Control.Applicative ((<|>), empty)
import Data.Functor ((<&>))
import TinyRegex.Parser (parseRegex)
import TinyRegex.Engine (compile)
import Data.Either (fromRight)
import Data.List (singleton, groupBy, nub)
import Data.Bifunctor (first)

type Input = Text.Text
type Output = [Evaluated]
type Evaluator = Maybe (Output, Input)
data Evaluated = TextOutput Text.Text | Label Int deriving (Show)

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
    (GroupStart n) -> first ([Label n] ++) <$> evaluate input xs
    (GroupEnd n)   -> first ([Label n] ++) <$> evaluate input xs
evaluate input [] = Just ([], input)

continue :: [RegexComp] -> (Output, Input) -> Evaluator
continue toks (output, rest) = (output `consMP`) <$> evaluate rest toks

runStart :: [RegexComp] -> Input -> Evaluator
runStart (x@Start:xs) input = do
    let pathA = evaluate input xs
    let pathB = Text.uncons input >>= flip evaluate (x:xs) . snd
    pathA <|> pathB
runStart xs input = evaluate input xs

runEvaluator :: Text.Text -> Input -> Either Text.Text (Output, Input)
runEvaluator regex input = case parseRegex regex of
    (Left x) -> Left x
    (Right x) -> case runStart (compile x) input of
        (Just y) -> Right y
        Nothing -> (Left . Text.concat) [ "No match. | Regex: ", regex, " | Input: ", input ]

runRegex :: Text.Text -> Input -> Either Text.Text [(Int, Text.Text)]
runRegex regex input = runEvaluator regex input <&> getGroups . fst

{- Groups -}
---------------------------------------------------------------------
takeGroups :: Int -> [Text.Text] -> [Evaluated] -> [(Int, Text.Text)]
takeGroups n ts [] = [(n, (Text.concat . reverse) ts)]
takeGroups n ts ((Label x) : xs) = if x == n
    then [(n, (Text.concat . reverse) ts)]
    else takeGroups n ts xs ++ takeGroups (n + 1) [] xs
takeGroups n ts ((TextOutput x) : xs) = takeGroups n (x:ts) xs

getGroups :: [Evaluated] -> [(Int, Text.Text)]
getGroups = zip [0..] . map joinGroup . nub . groupBy sameGroup . takeGroups 0 []
    where sameGroup a b = fst a == fst b
          joinGroup = Text.concat . map snd

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TinyRegex.Evaluate
( runEvaluator
) where

import TinyRegex.Core
import qualified Data.Text as Text
import Control.Applicative ((<|>), empty)
import Data.Functor ((<&>))
import TinyRegex.Parser (parseRegex)
import TinyRegex.Engine (compile)
import Data.Either (fromRight)
import Data.List (singleton)
import Data.Bifunctor (first)

type Input = Text.Text
type Output = [Evaluated]
type Evaluator = Maybe (Output, Input)
data Evaluated = TextOutput Text.Text | Label Int

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
        let pathA = evaluate input (zs ++ xs)
        let pathB = evaluate input xs
        pathA <|> pathB
    (ZeroOrOne  zs) -> do
        let pathA = evaluate input (zs ++ xs)
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

runEvaluator :: Text.Text -> Input -> Maybe (Output, Input)
runEvaluator regex input = case parseRegex regex of
    (Left x) -> Just ([], x)
    (Right x) -> evaluate input (compile x)

{- How to shave off the start of the list:
 -Start -> do
        let pathA = evaluate input xs
        let pathB = Text.uncons input >>= flip evaluate (x:xs) . snd
        pathA <|> pathB
 -}

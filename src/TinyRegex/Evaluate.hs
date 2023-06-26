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

type Input = Text.Text
type Output = Text.Text
type Evaluator = Maybe (Output, Input)

consMP :: Output -> (Output, Input) -> (Output, Input)
consMP x (y, ts) = (x `Text.append` y, ts)

evaluate :: Input -> [RegexComp] -> Evaluator
evaluate input (x:xs) = case x of
    (Sequence str) -> Text.stripPrefix str input <&> (str,) >>= \(output, rest) ->
        (output `consMP`) <$> evaluate rest xs
    (Character fn) -> Text.uncons input >>= \(c, rest) -> if getPredicate fn c
        then (Text.singleton c `consMP`) <$> evaluate rest xs
        else empty
    (ZeroOrMany zs) -> do
        let pathA = evaluate input (zs ++ xs) >>= \(output, rest) -> (output `consMP`) <$> evaluate rest (x:xs)
        let pathB = evaluate input xs >>= \(output, rest) -> (output `consMP`) <$> evaluate rest xs
        pathA <|> pathB
    (Alternative as bs) -> do
        let runPath zs = evaluate input (zs ++ xs) >>= \(output, rest) ->
                (output `consMP`) <$> evaluate rest xs
        runPath as <|> runPath bs
    Start -> do
        let pathA = evaluate input xs
        let pathB = Text.uncons input >>= flip evaluate (x:xs) . snd
        pathA <|> pathB
    End -> if Text.null input then Just (Text.empty, Text.empty) else Nothing
    _   -> undefined
evaluate input [] = Just (Text.empty, input)

runEvaluator :: Text.Text -> Input -> Maybe (Output, Input)
runEvaluator regex input = case parseRegex regex of
    (Left x) -> Just (Text.empty, x)
    (Right x) -> evaluate input (compile x)

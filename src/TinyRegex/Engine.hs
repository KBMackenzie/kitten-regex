module TinyRegex.Engine
( compile
) where

import TinyRegex.Core (RegexAST(..), RegexComp(..), Predicate(..))
import Control.Monad.State (State, evalState, get, modify)
import Data.List (singleton)

compileAST :: RegexAST -> State Int [RegexComp]
compileAST (MatchStar xs) = singleton . ZeroOrMany <$> compileAST xs
compileAST (MatchPlus xs) = do 
                            c <- compileAST xs
                            return (c ++ [ZeroOrMany c])
compileAST (MatchQues xs) = singleton . ZeroOrOne <$> compileAST xs
compileAST (Verbatim x)   = return [Sequence x]
compileAST AnyChar        = return [(Character . Predicate) (const True)]
compileAST (CharacterClass f) = return [Character f]
compileAST (Count n xs)             = concat . replicate n <$> compileAST xs
compileAST (CountRange a b xs)      = case (a, b) of
    (Just start, Just end) -> do
        c <- compileAST xs
        let matchStart = concat (replicate start c)
        let matchEnd   = concat (replicate (abs (end - start)) [ZeroOrOne c])
        return (matchStart ++ matchEnd)
    (Just start, Nothing)  -> concat . replicate start <$> compileAST xs
    (Nothing, Just end)    -> concat . replicate end . singleton . ZeroOrOne <$> compileAST xs
    (Nothing, Nothing)     -> return []
compileAST (AlternativeGroup as bs) = singleton <$> (Alternative <$> compileAll as <*> compileAll bs)
compileAST (MatchGroup xs) = do
    xs' <- compileAll xs
    n <- get
    modify succ
    return $ (GroupStart (n + 1) : xs') ++ [GroupEnd (n + 1)]
compileAST LineStart = return [Start]
compileAST LineEnd = return [End]

compileAll :: [RegexAST] -> State Int [RegexComp]
compileAll xs = do
    xs' <- mapM compileAST xs
    return (concat xs')

runCompile :: State Int a -> a
runCompile st = evalState st 0

compile :: [RegexAST] -> [RegexComp]
compile xs = runCompile (compileAll xs)

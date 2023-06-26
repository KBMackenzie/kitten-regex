module TinyRegex.Engine
( compileAST
) where

import TinyRegex.Core (RegexAST(..), RegexComp(..), Predicate(..))
import Control.Monad.State (State, runState)
import Data.List (singleton)

compileAST :: RegexAST -> State Int [RegexComp]
compileAST (MatchStar xs) = singleton . ZeroOrMany <$> compileAST xs
compileAST (MatchPlus xs) = let c = compileAST xs in return c ++ [ZeroOrMany c]
compileAST (MatchQues xs) = pure [ZeroOrOne (compileAST xs)]
compileAST (Verbatim x)   = pure [Sequence x]
compileAST AnyChar                  = [(Character . Predicate) (const True)]
compileAST (CharacterClass f)       = [Character f]
compileAST (Count n xs)             = let c = compileAST xs in concat (replicate n c)
compileAST (CountRange a b xs)      = let c = compileAST xs in case (a, b) of
    (Just start, Just end) -> let matchStart = concat (replicate start c)
                                  matchEnd   = concat (replicate end [ZeroOrOne c])
                                  in matchStart ++ matchEnd
    (Just start, Nothing)  -> concat (replicate start c)
    (Nothing, Just end)    -> concat (replicate end [ZeroOrOne c])
    (Nothing, Nothing)     -> []
compileAST (AlternativeGroup as bs) = [Alternative (compileAll as) (compileAll bs)]
compileAST (MatchGroup xs)          = let cs = compileAll xs
                                      in (GroupStart : cs) ++ [GroupEnd]
compileAST LineStart                = [Start]
compileAST LineEnd                  = [End]

compileAll :: [RegexAST] -> [RegexComp]
compileAll = concatMap compileAST

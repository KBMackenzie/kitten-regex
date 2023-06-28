module KittenRegex.Internal.Compile
( compile
) where

import KittenRegex.Internal.Core (RegexAST(..), RegexComp(..), Predicate(..))
import Control.Monad.State (State, evalState, get, modify)
import Data.List (singleton)
import Data.Functor ((<&>))

compileMany :: [RegexAST] -> State Int [RegexComp]
compileMany xs = do
    xs' <- mapM compileAST xs
    return (concat xs')

runCompile :: State Int a -> a
runCompile st = evalState st 1 -- Subgroups start indexing at '1'!

compile :: [RegexAST] -> [RegexComp]
compile xs = runCompile (compileMany xs)

-- The main workhorse of regex compilation.
-- The state monad is so that group numbering remains consistent within nested groups.
compileAST :: RegexAST -> State Int [RegexComp]
compileAST (ASTMatchStar xs) = singleton . ZeroOrMany <$> compileAST xs
compileAST (ASTMatchQues xs) = singleton . ZeroOrOne <$> compileAST xs
compileAST (ASTMatchPlus xs) = compileAST xs <&> \c -> c ++ [ZeroOrMany c]
compileAST (ASTVerbatim x)   = return [Sequence x]
compileAST ASTAnyChar        = return [(Character . Predicate) (const True)]
compileAST (ASTCharacterClass f) = return [Character f]
compileAST (ASTCount n xs)   = concat . replicate n <$> compileAST xs
compileAST (ASTCountRange a b xs) = case (a, b) of
    (Just start, Just end) -> do
        c <- compileAST xs
        let matchStart = concat (replicate start c)
        let matchEnd   = concat (replicate (abs (end - start)) [ZeroOrOne c])
        return (matchStart ++ matchEnd)
    (Just start, Nothing)  -> concat . replicate start <$> compileAST xs
    (Nothing, Just end)    -> concat . replicate end . singleton . ZeroOrOne <$> compileAST xs
    (Nothing, Nothing)     -> return []
compileAST (ASTAlternative as bs) = singleton <$>
    (Alternative <$> compileMany as <*> compileMany bs)
compileAST (ASTCaptureGroup xs) = do
    c <- compileMany xs
    num <- get
    modify succ
    return $ (GroupStart num : c) ++ [GroupEnd num]
compileAST (ASTNonCaptureGroup xs) = compileMany xs
compileAST ASTTokenStart = return [Start]
compileAST ASTTokenEnd = return [End]

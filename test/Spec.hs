import KittenRegex
import KittenRegex.Builder

reTest :: IO ()
reTest = do
    let re = build (ReString "ab+c+d")
    return ()

emailRegex :: Regex
emailRegex = build (ReString "^[\\w\\-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$")

validateEmail :: ReString -> Bool
validateEmail email = emailRegex <.?> email

examples :: IO ()
examples = do
    let example = ReString "test@gmail.com"
    let isValid = validateEmail example
    let inspect = emailRegex <.*> example
    print isValid -- It prints 'True'.
    print inspect -- It prints:
    -- Just (RegexOutput {groups = [(0,"test@gmail.com"),(1,"gmail.")], leftovers = ""})

emailRegex' :: Regex
emailRegex' = toRegex $
    startOfLine
    <.+> oneOrMore (wordChar <.|> char '-' <.|> char '.')
    <.+> char '@'
    <.+> capture (oneOrMore (wordChar <.|> char '-') <.+> char '.')
    <.+> amountBetween 2 4 (wordChar <.|> char '-')
    <.+> endOfLine

examples' :: IO ()
examples' = do
    let example = ReString "test@gmail.com"
    let isValid = emailRegex' <.?> example
    let inspect = emailRegex' <.*> example
    print isValid -- It prints 'True'.
    print inspect -- It prints the exact same output as before:
    -- Just (RegexOutput {groups = [(0,"test@gmail.com"),(1,"gmail.")], leftovers = ""})

{-
    <.+> oneOrMore $ capture (
        oneOrMore (wordChar <.|> char '-')
        <.+> char '.'
    )
-}


main :: IO ()
main = examples' --putStrLn "Test suite not yet implemented"

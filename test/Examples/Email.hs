module Examples.Email
( emailExamples
, emailExamples'
) where

import KittenRegex
import KittenRegex.Builder

emailRegex :: Regex
emailRegex = build (ReString "^[\\w\\-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$")

validateEmail :: ReString -> Bool
validateEmail email = emailRegex <.?> email

emailExamples :: IO ()
emailExamples = do
    let example = ReString "test@gmail.com"
    let isValid = validateEmail example
    let inspect = emailRegex <.*> example
    print isValid -- It prints 'True'.
    print inspect -- It prints:
    -- Just (RegexOutput {groups = fromList [(0,"test@gmail.com"),(1,"gmail.")], leftovers = ""})

emailRegex' :: Regex
emailRegex' = toRegex $
    startOfLine
    <.+> oneOrMore (wordChar <.|> char '-' <.|> char '.')
    <.+> char '@'
    <.+> capture (oneOrMore (wordChar <.|> char '-') <.+> char '.')
    <.+> amountBetween 2 4 (wordChar <.|> char '-')
    <.+> endOfLine

emailExamples' :: IO ()
emailExamples' = do
    let example = ReString "test@gmail.com"
    let isValid = emailRegex' <.?> example
    let inspect = emailRegex' <.*> example
    print isValid -- It prints 'True'.
    print inspect -- It prints the exact same output as before:
    -- Just (RegexOutput {groups = fromList [(0,"test@gmail.com"),(1,"gmail.")], leftovers = ""})

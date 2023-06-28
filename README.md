A small, minimalistic Haskell Regex library written in pure Haskell. I initially worked on this as a side feature for my esotering programming language [Meowscript](https://github.com/KBMackenzie/Meowscript) and as a learning exercise, but it escalated into something usable!

This library is primarily based on `Data.Text` instead of Haskell's native String type (due to the inefficiency of Haskell's native String type). It supports regexing with both Text, ByteString *and* String, however. (Though Strings have to be wrapped in a special newtype.)

It supports:
1. Matching regex such as the following: (more details later)
2. Capture groups and non-capture groups
3. Regex 'split' and 'replace' functions for strings
4. A way to dynamically build a regex with combinator functions

An example of it in action:

```haskell
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
```

And an example of how to dynamically build that same regex above with combinators:
```haskell
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
```

[ To Do: ] 
1. Make a better README.
2. Add more examples.
3. Add documentation for all of the supported operations.

A small, minimalistic Haskell Regex library written in pure Haskell. I initially worked on this as a side feature for my esotering programming language [Meowscript](https://github.com/KBMackenzie/Meowscript) and as a learning exercise, but it escalated into something usable!

This library supports a __subset of PCRE__. You can see all the supported patterns [here](#supported-regex-patterns).

This library is primarily based on `Data.Text` instead of Haskell's native String type (due to the inefficiency of Haskell's native String type). It supports regexing with both Text, ByteString *and* String, however. (Though Strings have to be wrapped in a special newtype.)

It supports:
1. Matching regex such as the following: (more details later)
2. Capture groups and non-capture groups
3. Regex 'split' and 'replace' functions for strings
4. A way to dynamically build a regex with combinator functions

## Regex Matching
An example of it in action, with a neat regex string for email validation taken from [this website](https://regexr.com/3e48o):

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
    -- Just (RegexOutput {groups = fromList [(0,"test@gmail.com"),(1,"gmail.")], leftovers = ""})
```

Disclaimer: You *shouldn't* validate emails with regex, this is just a convenient example!

Additionally, while I wrapped the strings in the example above in the ReString newtype, you don't *have* to. The Regexable typeclass (which handles regex compilation and matching) has instances for Text and ByteString too.

I heavily encourage you to use Text for everything Regex-related, in fact! The inner workings of the Regex engine use Text already; every other string type is just converted to Text behind the scenes.

> *"Why not have a raw instance for String?"*

Because String is a type alias for [Char], and [Char] cannot be made an instance of a typeclass without using the FlexibleInstances language extension and allowing for some possible unpredictability down the road with the compiler, and I didn't think that was worth the trouble.

The ReString newtype is an instance of IsString, too, so the OverloadedStrings extension should work fine with it, in case you want to/have to work with Haskell's native String type.


## Dynamically Building Regexes
An example of how to dynamically build that same email validation regex above with combinators:

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
    -- Just (RegexOutput {groups = fromList [(0,"test@gmail.com"),(1,"gmail.")], leftovers = ""})
```

## Supported Regex Patterns
This library supports a subset of PCRE.
The following Regex patterns are supported by this library:

- Escape sequences with a backslash (\\).
- The star (\*), plus (\+) and question mark (?) operators.
- Character classes between square brackets. (e.g. `[a-zA-Z0-9]`)
- Capture groups (e.g. `(ab)c`) and non-capture groups (e.g. `(?:ab)c`)
- Repetition ranges between curly braces. (e.g. `a{3}`, `b{3, 5}`, `c{3,}`, etc).
- Alternative branches with '|'. (e.g. `a(b|c)`, `a*b|c+`).
- Generic character types (`\w`, `\s`, `\d`) and their negations (`\W`, `\S`, `\D`)
- The start of line (^) and end of line ($) anchors.

Additionally, through Regex combinators, this library supports completely new character classes made out of Haskell functions.

[ To Do: ] 
1. Make a better README.
2. Add more examples.
3. Add documentation for all of the supported operations.

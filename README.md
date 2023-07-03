A small, minimalistic Haskell Regex library written in pure Haskell. This library supports a __subset of PCRE__. You can see all the supported patterns [here](#supported-regex-patterns).

I made this for fun as a side feature for my esoteric programming language [Meowscript](https://github.com/KBMackenzie/Meowscript) and as a learning exercise. It's **not meant** to be used in production!

This library is primarily based on `Data.Text` instead of Haskell's native String type (due to the inefficiency of Haskell's native String type). It supports regexing with both Text, ByteString *and* String, however. (Though Strings have to be wrapped in a special newtype, *'ReString'*.)

You can see the supported regex patterns [here](#supported-regex-patterns).

This library has functions for **matching**, **splitting** strings with regex and **replacing** regex matches in strings with another string.

## Regex Matching
An example of it in action, with a little regex to get the contents of an HTML img tag:

```haskell
imgTagRe :: Regex
imgTagRe = build $ ReString "<\\s*img(.*)>"

imageExample :: IO ()
imageExample = do
    let tag = ReString "<img src=\"example.png\" />"
    print $ imgTagRe <.?> tag -- Prints true.
    print $ imgTagRe <.*> tag -- Prints:
    -- Just (RegexOutput {groups = fromList [(0,"<img src=\"example.png\" />"),(1," src=\"example.png\" /")], leftovers = ""})
```
Do note that 'build' is a partial function. It should only be used when you're sure the regex string contains no syntax errors. A safe alternative is 'buildEither', which has a return type of `Either Text Regex`.

## String vs Text
While I wrapped the strings in the example above in the ReString newtype, you don't *have* to. The Regexable typeclass (which handles regex compilation and matching) has instances for Text and ByteString too.

I heavily encourage you to use Text for everything Regex-related, in fact! The inner workings of the Regex engine use Text already; every other string type is just converted to Text behind the scenes.

> *"Why not just have an instance for String?"*

Because String is a type alias for [Char], and [Char] cannot be made an instance of a typeclass without using the FlexibleInstances language extension and allowing for some possible unpredictability down the road with the compiler, and I didn't think that was worth the trouble.

The ReString newtype is an instance of IsString, too, so the OverloadedStrings extension should work fine with it, in case you want to/have to work with Haskell's native String type.


## Dynamically Building Regexes
An example of how to dynamically build that same regex above with combinators:

```haskell
imgTagRe' :: Regex
imgTagRe' = toRegex $
    char '<'
    <.+> anyAmountOf whitespace
    <.+> string "img"
    <.+> capture (anyAmountOf anyChar)
    <.+> char '>'
    -- Just (RegexOutput {groups = fromList [(0,"<img src=\"example.png\" />"),(1," src=\"example.png\" /")], leftovers = ""})
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
- The start of string (^) and end of string ($) anchors.

**Note:** The ^ and $ anchors match **only** the start and end of the string, respectively.
This library doesn't yet support multiline matching.

Additionally, through Regex combinators, this library supports completely new character classes made out of Haskell functions.

[ To Do: ] 
1. Make a better README.
2. Add more examples.
3. Add documentation for all of the supported operations.

module Examples.HTML
( imageExample
) where

import KittenRegex
import KittenRegex.Builder

imgTagRe :: Regex
imgTagRe = build $ ReString "<\\s*img(.*)>"

imgTagRe' :: Regex
imgTagRe' = toRegex $
    char '<'
    <.+> anyAmountOf whitespace
    <.+> string "img"
    <.+> capture (anyAmountOf anyChar)
    <.+> char '>'
    -- Just (RegexOutput {groups = fromList [(0,"<img src=\"example.png\" />"),(1," src=\"example.png\" /")], leftovers = ""})

imageExample :: IO ()
imageExample = do
    let tag = ReString "<img src=\"example.png\" />"
    print $ imgTagRe' <.?> tag -- Prints true.
    print $ imgTagRe' <.*> tag -- Prints:
    -- Just (RegexOutput {groups = fromList [(0,"<img src=\"example.png\" />"),(1," src=\"example.png\" /")], leftovers = ""})

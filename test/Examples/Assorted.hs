module Examples.Assorted
( reTest
, assertion
) where

import KittenRegex

reTest :: IO ()
reTest = do
    let re = build (ReString "ab+c+d")
    return ()

assertion :: IO ()
assertion = do
    let re1 = build (ReString "ab+")
    let re2 = build (ReString "abb*")
    let str = ReString "abbbbbbbb"
    let out1 = re1 <.*> str
    let out2 = re2 <.*> str
    print out1
    print out2
    print $ out1 == out2



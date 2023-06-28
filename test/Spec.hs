import KittenRegex

reTest :: IO ()
reTest = do
    let re = build (ReString "ab+c+d")
    return ()


main :: IO ()
main = putStrLn "Test suite not yet implemented"

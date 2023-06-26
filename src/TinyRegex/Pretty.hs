{-# LANGUAGE OverloadedStrings #-}

module TinyRegex.Pretty
( prettyError
) where

import qualified Data.Text as Text

prettyError :: Text.Text -> Text.Text -> Text.Text
prettyError message str = Text.concat
    [ message, "\n" , "\"", str, "\"\n" , " ^\n" ]

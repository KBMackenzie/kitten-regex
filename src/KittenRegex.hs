module KittenRegex
( Regexable(..)
, ReString(..)
, Regex -- Intentionally opaque.
, RegexOutput(..)
, getGroup
) where

import KittenRegex.Internal.Core (Regex, RegexOutput(..), getGroup)
import KittenRegex.Internal.Regexable (Regexable(..), ReString(..))

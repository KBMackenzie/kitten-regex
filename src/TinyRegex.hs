module TinyRegex
( Regexable(..)
, ReString(..)
, Regex -- Intentionally opaque.
, RegexOutput(..)
, getGroup
) where

import TinyRegex.Internal.Core (Regex, RegexOutput(..))
import TinyRegex.Internal.Regexable (Regexable(..), ReString(..), getGroup)

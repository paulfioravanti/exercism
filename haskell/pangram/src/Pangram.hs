module Pangram (isPangram) where

import Data.Char as Char
import Data.Function ((&))
import Data.Set as Set
import Data.Text as Text

isPangram :: String -> Bool
isPangram text =
  let numberOfLettersInAlphabet = 26
  in
    text
    & Text.pack
    & Text.toLower
    & Text.filter Char.isAsciiLower
    & Text.unpack
    & Set.fromList
    & Set.size
    & (==) numberOfLettersInAlphabet

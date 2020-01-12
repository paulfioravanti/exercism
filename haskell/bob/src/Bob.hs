{-# LANGUAGE OverloadedStrings #-}

module Bob (responseFor) where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text

responseFor :: String -> String
responseFor input =
  let parsedInput = input & Text.pack & Text.strip
  in
    case parsedInput of
      "" -> "Fine. Be that way!"

      remark -> respondToVerbalRemark remark

-- PRIVATE

respondToVerbalRemark :: Text -> String
respondToVerbalRemark remark =
  let
    isQuestion = Text.isSuffixOf "?" remark

    isShouting = Text.toUpper remark == remark && Text.toLower remark /= remark
  in case (isQuestion, isShouting) of
    (True, True) -> "Calm down, I know what I'm doing!"

    (True, False) -> "Sure."

    (False, True) -> "Whoa, chill out!"

    (False, False) -> "Whatever."

module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse rnaTranscriptions

-- PRIVATE

rnaTranscriptions :: Char -> Either Char Char
rnaTranscriptions nucleotide = case nucleotide of
  'G' -> Right 'C'
  'C' -> Right 'G'
  'T' -> Right 'A'
  'A' -> Right 'U'
  invalid -> Left invalid

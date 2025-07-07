module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse transcribe

-- Helper function to convert one nucleotide
transcribe :: Char -> Either Char Char
transcribe 'G' = Right 'C'
transcribe 'C' = Right 'G'
transcribe 'T' = Right 'A'
transcribe 'A' = Right 'U'
transcribe x   = Left x  -- invalid character

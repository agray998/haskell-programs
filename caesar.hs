import Data.Char

let_to_int c | isLower c = ord c - ord 'a'
             | isUpper c = ord c - ord 'A'

int_to_lower_let n = chr (ord 'a' + n)

int_to_upper_let n = chr (ord 'A' + n)

shift n c | isLower c = int_to_lower_let ((let_to_int c + n) `mod` 26)
          | isUpper c = int_to_upper_let ((let_to_int c + n) `mod` 26)
          | otherwise = c

encode n xs = [shift n x | x <- xs]

main = do
  print $ encode 3 "Haskell is fun"

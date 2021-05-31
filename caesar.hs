import Data.Char

lettoint c = ord c - ord 'a'

inttolet n = chr (ord 'a' + n)

shift n c | isLower c = inttolet ((lettoint c + n) `mod` 26) | otherwise = c

encode n xs = [shift n x | x <- xs]

main = do
  print $ encode 3 "haskell is fun"

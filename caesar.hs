import Data.Char

count x xs = length [x' | x' <- xs, x == x']

letters xs = length [x | x <- xs, isLower x || isUpper x]

positions x xs = [i | (x', i) <- zip xs [0..], x == x']

let_to_int c | isLower c = ord c - ord 'a'
             | isUpper c = ord c - ord 'A'

int_to_lower_let n = chr (ord 'a' + n)

int_to_upper_let n = chr (ord 'A' + n)

shift n c | isLower c = int_to_lower_let ((let_to_int c + n) `mod` 26)
          | isUpper c = int_to_upper_let ((let_to_int c + n) `mod` 26)
          | otherwise = c

encode n xs = [shift n x | x <- xs]

freq_table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent n m = (fromIntegral n / fromIntegral m) * 100

freqs xs = [percent ((count x xs) + (count y xs)) (letters xs) | x <- ['a'..'z'], y <- ['A'..'Z'], let_to_int x == let_to_int y]

chi_sqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate n xs = drop n xs ++ take n xs

crack xs = encode (-factor) xs
           where
             factor = head (positions (minimum chi_tab) chi_tab)
             chi_tab = [chi_sqr (rotate n (freqs xs)) freq_table | n <- [0..25]]

main = do
  let c_text = encode 3 "ThIs Is In SpOnGeBoB cAsE"
  print c_text
  print $ crack c_text

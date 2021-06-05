loop i = do
  print (show i ++ " bottles of beer on the wall")
  print (show i ++ " bottles of beer")
  print "Take one down and pass it around"
  print (show (i - 1) ++ " bottles of beer on the wall")
  print " "
  let a = i - 1
  if a /= 0 then loop a else print "done"

main = do
  loop 99

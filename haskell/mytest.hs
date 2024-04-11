mytest :: Bool
mytest = case reta of
  'a' | 'b' -> True
  _ -> False

reta :: Char
reta = 'a'

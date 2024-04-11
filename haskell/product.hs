--myProduct :: (Foldable t, Eq n, Num n) => t n -> n
--myProduct xs = foldr step id xs 1 where
--    step 0 f acc = 0
--    step x f acc = f $! acc * x

--myProduct :: (Foldable t, Eq n, Num n) => t n -> n
--myProduct xs = foldr go id xs 1 where
--    go 0 f acc = 0
--    go x f acc = f $! acc * x

--myProduct :: (Foldable t, Eq n, Num n) => t n -> n
--myProduct xs = foldr func id xs 1
--
--func :: (Num b, Eq b) => b -> (b -> b) -> b -> b
--func 0 f acc = 0
--func x f acc = f $! acc * x

--
--myProduct :: (Foldable t, Eq n, Num n) => t n -> n
--myProduct = flip go 1 where
--    go = foldr
--        (\x f -> if x == 0 then \acc -> 0 else \acc -> acc `seq` f (acc * x))
--        (\acc -> acc)
--

-- Наибольший общий делитель двух чисел
myGcd :: Int -> Int -> Int
myGcd a b | (abs a < abs b) = myGcd b a
          | (b == 0) = abs a
          | otherwise = myGcd b (a `rem` b)

-- Существование натурального числа, являющегося квадратом другого числа
-- между двумя заданными целыми числами
isSqr :: Int -> Int -> Bool
isSqr a b =  ( ceiling $ sqrt $ fromIntegral a )
          <= (floor $ sqrt $ fromIntegral b)

-- Возведение целого числа в целую степень ("Индийский алгоритм")
pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow x n | odd n = x * expr
        | even n = expr
        where expr = (pow x (n `div` 2)) * (pow x (n `div` 2))

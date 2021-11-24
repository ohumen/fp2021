{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
-- Лабораторна робота №3
-- студентки групи КН-32 підгрупи 1
-- Гумен Ольга
-- Варіант №10

-- Мета: Набути досвiду визначення та використання функцiй вищого порядку.

-- Завдання 1. Визначте вказанi функцiї в кожному з завдань:
-- а) без застосування, 
-- б) з застосуванням вбудованих функцiй вищого порядку.

-- 1.10 Видалити кожен n-й елемент списку, напр. 
-- при n=2: "1234590" ⇒ "1350".

-- a)

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (x:xs) = xs
remove n (x:xs) = x : remove (n-1) xs

-- б)

groups :: Int -> [a] -> [[a]]
groups n = map (take n) . takeWhile (not . null) . iterate (drop n)

removeEveryNth :: Int -> [a] -> [a]
removeEveryNth n = concatMap (take (n-1)) . groups n

-- Завдання 2. 

-- 2.10 Знайти усi простi числа в указаному дiапазонi.

-- a)

prime :: Integer -> Integer -> [Integer]
prime m n = [x | x <- list, x>=m, x<=n]

list :: [Integer]
list = 2 : [x | x <- [3..], right x]

right :: Integer -> Bool
right x = foldr (\p r -> p*p>x || mod x p /= 0 && r) True list


-- б)

primesR :: Integral a => a -> a -> [a]
primesR a b | even a = filter isPrime [a+1,a+3..b]
            | otherwise   = filter isPrime [a,a+2..b]

-- Висновок: Під час даної лабораторної роботи я набула досвiду визначення та використання функцiй вищого порядку
-- за допомогою чого потім я застосувала вивчене на практиці.
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
remove n (x:xs) = x : remove (n-1) (xs)

-- б)

groups :: Int -> [a] -> [[a]]
groups n = map (take n) . takeWhile (not . null) . iterate (drop n)

removeEveryNth :: Int -> [a] -> [a]
removeEveryNth n = concatMap (take (n-1)) . groups n

-- Завдання 2. 

-- 2.10 Знайти усi простi числа в указаному дiапазонi.

-- a)

isPrime :: Integral a => a -> Bool
isPrime n = all check [2..n `div` 2]
            where check x = n `mod` x /= 0

-- б)

is_prime :: Int -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n | not (null ([x | x <- [2 .. n-1], mod n x == 0])) = False
   | otherwise = True

-- Висновок: Під час даної лабораторної роботи я набула досвiду визначення та використання функцiй вищого порядку
-- за допомогою чого потім я застосувала вивчене на практиці.
-- Лабораторна робота №2
-- студентки групи КН-32 підгрупи 1
-- Гумен Ольга
-- Варіант №10

-- Мета: Набути досвiду визначення рекурсивних функцiй, 
-- використання механiзму зiставлення зi зразком i роботи з кортежами та списками.

-- Завдання 1. Визначте вказанi функцiї в кожному з завдань:
-- а) без застосування, 
-- б) з застосуванням вбудованих функцiй.

-- 1.10 Визначити частоту кожного елемента списку, напр.: 
-- "aaabbcaadddd"⇒[(’a’,5), (’b’,2), (’c’,1), (’d’,4)].

-- a)

import Data.Map (fromListWith, toList)
import Data.List (group, sort)
import Control.Arrow ((&&&))

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

-- б)

countOccurences :: Ord a => [a] -> [(a, Int)]
countOccurences = map (head &&& length) . group . sort

-- Завдання 2. 

-- 2.10 Знайти простi дiльники числа.

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

-- Висновок: Під час даної лабораторної роботи я ознайомилась з мовою Haskell та інтерпретатором ghci, також я ознайомилась
-- з основними типами мови, зі структурою та функціями мови та застосувала вивчене на практиці.
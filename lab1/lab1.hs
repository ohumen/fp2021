{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Лабораторна робота №1
-- студентки групи КН-32 підгрупи 1
-- Гумен Ольга
-- Варіант №10

-- Мета: Ознайомитись з основними типами мови. Ознайомитись зi структурою та функцiями Glasgow Haskell Compiller. 
-- Набути навичок роботи з iнтерпретатором ghci та визначення найпростiших функцiй.

-- Завдання 1. Наведіть приклади виразу вказаного типу (([Double],[Char]),[Integer]).  
-- Кожен список має містити кілька елементів.
-- Перегляньте тип прикладів, як їх визначає ghci. Прокоментуйте.

a :: [(([Double], [Char]), [Integer])]
a = [(([1.5, 1.25],['a', 'b']),[3, 7])]

b :: [(([Double], [Char]), [Integer])]
b = [(([0.69, 0.75],['f', 'c']),[2, 9])]

c :: [(([Double], [Char]), [Integer])]
c = [(([0.5, 3.35, 4.2],['d', 'e', 'f']),[1, 6, 8])]

-- Завдання 2. Визначте два варіанта вказаної фунції (функцiя приймає три числа i повертає найменше з них). 
-- Перший варіант - з одним аргуменом-кортежем, другий - без використання кортежів чи списків.

function1 :: [Integer] -> Integer

function1 (x : []) = x

function1 (x : xs) = if x < m then x 
    else m
    where 
        m = function1 xs

function2 :: Integer -> Integer -> Integer -> Integer

function2 a b c = minimum [a, b, c]

-- Висновок: Під час даної лабораторної роботи я ознайомилась з мовою Haskell та інтерпретатором ghci, також я ознайомилась
-- з основними типами мови, зі структурою та функціями мови та застосувала вивчене на практиці.
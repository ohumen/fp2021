-- Лабораторна робота №5
-- студентки групи КН-32 підгрупи 1
-- Гумен Ольга
-- Варіант №10

-- Мета: Ознайомитись з модульною органiзацiєю програм та засобами введення-виведення.
-- Набути досвiду компiляцiї Haskell-програм. 

-- Завдання 5.1. Реалiзувати та скомпiлювати одну з програм, 
-- розроблених у лабораторнiй роботi No 3 для Вашого варiанта з введенням даних: 
-- а) з клавiатури; 
-- б) з файлу;
-- та виведенням результатiв: 
-- в) на екран;
-- г) у файл.

-- 3.10 Видалити кожен n-й елемент списку, напр. 
-- при n=2: "1234590" ⇒ "1350".

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (x:xs) = xs
remove n (x:xs) = x : remove (n-1) xs

main :: IO()
main = do

-- a) введення з клавiатури:

putStrLn "Input:"
str<-getLine
putStrLn(reverse str)

-- б) введення з файлу:

str<-readFile "input.txt"
putStrLn(reverse str)

-- в) виведенням результатiв на екран:

putStrLn "Input:"
str<-getLine
writeFile "output.txt" (reverse str)

-- г) виведенням результатiв у файл: 

str<-readFile "input.txt"
writeFile "output.txt" (reverse str)

-- Висновок: Під час даної лабораторної роботи я ознайомилась з модульною органiзацiєю програм та засобами введення-виведення.
-- Також я набула досвiду компiляцiї Haskell-програм. та застосувала вивчене на практиці.
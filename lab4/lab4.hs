{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
-- Лабораторна робота №4
-- студентки групи КН-32 підгрупи 1
-- Гумен Ольга
-- Варіант №10

-- Мета: Ознайомитись з системою типiв та класiв типiв. 
-- Набути досвiду визначення нових типiв та класiв типiв i їх використання.

-- Завдання. Публiкацiї.
-- Зберiгаються данi про публiкацiї, якi можуть бути книгою (автор/спiвавтори, назва, мiсто, видавництво, рiк), 
-- статтею (автор/спiвавтори, назва статтi, назва журналу, рiк, номер журналу, сторiнки)
-- або тезами доповiдi (автор/спiвавтори, назва доповiдi, назва конференцiї, мiсто, рiк, сторiнки). 
-- Визначне функцiї для :

-- 10. статистика публiкацiй автора — кiлькiсть, обсяг (у сторiнках), тип;

type Author  = String
type Name = String 
type SourceName = String
type Year = Int
type SourceNumber = Int
type Pages = Int
type City = String

data Publitactions = Book Author Name City SourceName Year |
                     Article Author Name SourceName Year SourceNumber Pages |
                     ThesisOfReport Author Name SourceName City Year Pages
                     deriving (Eq, Show)

publitactions :: [Publitactions]
publitactions = [
    Book "Ernest Hemingway" "Islands in the Stream: A Novel" "Havana" "Scribner" 1970,
    Book "Paulo Coelho" "By the River Piedra I Sat Down and Wept: A Novel of Forgiveness" "Paris" "HarperCollins" 2009,
    Book "Ernest Hemingway" "The Complete Short Stories Of Ernest Hemingway: The Finca Vigia Edition" "Berlin" "Scribner" 1998,
    
    Article "Warren Winick-Ng" "Cell-type specialization is encoded by specific chromatin topologies" "Nature" 2021 19 200,
    Article "Can Cao" "Structure, function and pharmacology of human itch GPCRs" "Nature" 2021 20 150,
    
    ThesisOfReport "Andrés Guadamuz" "Analysis of UK/EU Law on Data Mining in Higher Education Institutions" "University of Sussex" "London" 2013 30]


applyAll :: [a -> b] -> [a] -> [b]
applyAll  _ [] = []
applyAll (x:xs) (y:ys) = x y : applyAll xs ys

function :: String -> [Publitactions] -> String
function _ [] = "False"
function x (y:ys) = if x == Author y then Name Pages publitactions y else function x ys

check :: String -> [String]
check a = [function a publitactions]

-- Висновок: Під час даної лабораторної роботи я ознайомилась з системою типiв та класiв типiв. 
-- Також я набула досвiду визначення нових типiв та класiв типiв i їх використання після чого застосувала вивчене на практиці.
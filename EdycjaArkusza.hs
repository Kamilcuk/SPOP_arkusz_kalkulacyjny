module EdycjaArkusza where
import Data.CSV
import System.IO

--rows - length (a:as) - zakladajac ze pierwszy wiersz jest najdluzszy (albo wszystkie sa rowne)
--cols - length ( head (a:as))
displayFile:: String -> [[Int]] -> IO()
displayFile name (a:as)= do
  putStrLn ("Name of the file: " ++ name)
  putStrLn (printFstLine [x| x<-[0..length ( head (a:as)) ]] )
  putStrLn (printRowLine [x | x<- [1..length (a:as)] ] (a:as))


printFstLine :: [Int] -> String
printFstLine [] = "0"
printFstLine (x:xs) | length(xs) == 0 = show x
                    | otherwise = (show x) ++ " " ++ printFstLine xs

printRowLine :: [Int] -> [[Int]] -> String
printRowLine [] _ = "0"
printRowLine _ [] = "0"
printRowLine (x:xs) (a:as) | length xs == 0 && length as == 0  = show x ++ "|" ++ printFstLine a
                           | otherwise =  (show x) ++ "|" ++ printFstLine a ++ "\n" ++ printRowLine xs as

edytujKomorke wiersz kolumna = do
  return 0

otwarcieArkusza nazwa = do
  -- writeFile nazwa "[[\"1\", \"2\", \"3\"]]"
  -- str <- csvFile ""  nazwa
  return 0

edycjaArkusza nazwa = do
  putStrLn "Otwieranie arkusza..."
  otwarcieArkusza nazwa
  putStrLn ""
  putStrLn ("> menu arkusza " ++ nazwa)
  putStrLn "Wybierz jedną z poniższych opcji:"
  putStrLn "w - wypisz arkusz na ekran"
  putStrLn "e - edytuj dane w podanej komórce"
  putStrLn "x - wróć do menu głównego"
  a <- getLine
  let w = if length a > 0 then (a !! 0) else ' '
  case w of
    'w' -> do
      return 0
    'e' -> do
      putStrLn "Wiersz: "
      wiersz <- getLine
      putStrLn "Kolumna: "
      kolumna <- getLine
      edytujKomorke wiersz kolumna
      return 0
    'x' -> do
      return 0
    _ -> do
      putStrLn "Nieznana opcja"
      return 0
  if w /= 'x' then 
    edycjaArkusza nazwa
  else
    return 0
  
{-  e <- getLine
  if e=='ac' then 
     do addColumn
  else if e=='ar' then
     do addRow
  else if e=='dc' then 
     do delColumn
  else if e=='dr' then
     do delRow
  else if e=='ed' then
     do editData
  else if e=='sum' then
     do doSum
  else if e=='mult' then
     do doMult
  else if e=='ave' then
     do doAve
  else
     do putStrLn "Don't know that option"
     edit
 

addColumn = do
  putStrLn "How many columns would you like to add?"
  num <- getLine

addRow = do
  putStrLn "How many rows would you like to add?"
  num <- getLine

delColumn = do
  putStrLn "Which column would you like to delete?"
  num <- getLine

delRow = do
  putStrLn "Which row would you like to delete?"
  num <- getLine

editData = do

doSum = do

doMult = do

doAve = do
-}








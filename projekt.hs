import System.IO

main = do
  putStrLn "Welcome to Hexcel!"
  menu

-----MENU
menu = do
  putStrLn "Please, choose one of the following options:"
  putStrLn "f - f"
  putStrLn "e - edit"
  putStrLn "x - exit"
  m <- getChar
  if m=='f' then 
     do file
  else if m=='e' then
     do edit
  else if m=='x' then
     do exit
  else
     do putStrLn "Don't know that option"
        menu -- podwaja sie napis - nie wiem czemu... 

------FILE
file = do
  putStrLn "Please, choose one of the following options:"
  putStrLn "o - open an existing file"
  putStrLn "n - create a new file"
  putStrLn "s - save file"
  a <- getChar
  if a=='o' then 
    do putStrLn "Opening a file... "
  else if a=='n' then 
    do fileName <- createFile
       putStrLn fileName
       menu
  else if a=='s' then 
    do putStrLn "Saving a file... " 
       fileName <- saveFile
       putStrLn ("Saved file: " ++ fileName)
  else
    do putStrLn "Dont know that action... Try again:"
       file
       
createFile = do
  putStrLn "Creating new file... "
  getLine -- bez tego cos nie dzialalo
-- chyba nie ma co sie pytac o nazwe?
--  putStrLn "Name of the file:"
--  name <- getLine  
  putStrLn "How many rows?"
  rows <- getLine
  putStrLn "How many columns?"
  cols <- getLine
--  displayFile name rows cols [[]]
  return(rows)


-- dziala jak [[Int]] ale trzeba miec jakikolwiek typ danych :/

displayFile:: String -> [[Int]] -> IO()

--rows - length (a:as) - zakladajac ze pierwszy wiersz jest najdluzszy (albo wszystkie sa rowne)
--cols - length ( head (a:as))
displayFile name (a:as)= do
  putStrLn ("Name of the file: " ++ name)
  putStrLn ( printFstLine [x| x<-[0..length ( head (a:as)) ]] )
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



openAFile = do
  putStrLn "Name of the file you would like to open?"
  name <- getLine
  return (name)

saveFile = do
  putStrLn "The file will be saved as: "
  getLine
  name <- getLine
  return (name)  






-----EDIT
edit = do
  putStrLn "Please, choose one of the following options:"
  putStrLn "ac - add column(s)"
  putStrLn "ar - add row(s)"
  putStrLn "dc - delete column(s)"
  putStrLn "dr - delete row(s)"
  putStrLn "ed - edit data in a specified cell"
  putStrLn "sum - sum numbers in a given range of cells"
  putStrLn "mult - multiply numbers in a given range of cells"
  putStrLn "ave - average of the numbers in a given range of cells"
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

----- EXIT
exit = do
  -- sprawdzic czy plik zapisany?
  putStrLn  "Bye Bye..."








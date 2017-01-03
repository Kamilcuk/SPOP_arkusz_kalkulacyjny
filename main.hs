import System.IO
import System.Exit
import System.Directory
import Data.List
import Text.Printf


version :: Float
version = 0.1

main = do
  putStrLn "Welcome to Hexcel!"
  printf   "Wersja: %.2f\n" version
  putStrLn "Hexcel jest to uniwersalne rozwiązanie potrzebne w każdym domu!"
  putStrLn "Program zapisuje pliki w aktualnym katalogu z rozszerzeniem .hexcel"
  menu

menu = do
  putStrLn ""
  putStrLn "> menu główne"
  putStrLn "Wybierz jedną z poniższych opcji:"
  putStrLn " n - utwórz nowy arkusz"
  putStrLn " o - otwórz istniejący arkusz"
  putStrLn " u - usuń arkusz"
  putStrLn " w - wyświetl istniejące arkusze"
  putStrLn " x - exit"
  putStr   "$ "
  a <- getLine
  let w = if length a > 0 then (a !! 0) else ' '
  case w of
    'n' -> do 
      putStrLn "> Tworzenie nowego arkusza"
      putStrLn "Jeśli arkusz o podanej nazwie już istnieje, zostanie usunięty."
      putStrLn "Nazwa arkusza:"
      temp <- getLine
      let name = temp ++ ".hexcel"
      writeFile name ""
      putStrLn ("> Utworzono arkusz " ++ name)
      return 0
    'o' -> do 
      putStrLn "> Otwieranie arkusza"
      putStrLn "Nazwa arkusza:"
      temp <- getLine
      let name = temp ++ ".hexcel"
      -- otwarcie edycja arkusza
      let arkusz = "" -- tutaj bedzie funkcja do otwierania arkusza
      edycjaArkuszaMenu name
      return 0
    'u' -> do
      putStrLn "> Usuwanie arkusza"
      putStrLn "Nazwa arkusza:"
      temp <- getLine
      let name = temp ++ ".hexcel"
      removeFile name
      putStrLn ("> Arkusz " ++ name ++ "usunięty")
      return 0
    'w' -> do
      putStrLn "> Dostępne arkusze"
      allindirectory <- getDirectoryContents "."
      let filtered = filter (isSuffixOf ".hexcel") allindirectory
      print filtered
      return 0
    'x' -> do 
      putStrLn  "Bye Bye..."
      return 0
    _ -> do 
      putStrLn "Nieznana akcja"
      return 0
  if w /= 'x' then 
    menu
  else
    return 0

edycjaArkuszaMenu :: [Char] -> IO Int
edycjaArkuszaMenu nazwa arkusz = do
  putStrLn ""
  putStrLn ("> menu arkusza " ++ nazwa)
  putStrLn "Wybierz jedną z poniższych opcji:"
  putStrLn "w - wypisz arkusz na ekran"
  putStrLn "e - edytuj dane w podanej komórce"
  putStrLn "s - zapisz arkusz na dysk"
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
      -- edytujKomorke arkusz wiersz kolumna
      return 0
    's' -> do
      return 0
    'x' -> do
      return 0
    _ -> do
      putStrLn "Nieznana opcja"
      return 0
  if w /= 'x' then 
    edycjaArkuszaMenu nazwa arkusz
  else
    return 0

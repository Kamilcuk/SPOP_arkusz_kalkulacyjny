import System.IO
import System.Exit
import System.Directory
import Data.List
import Text.Printf
import EdycjaArkusza


version :: Float
version = 0.1

main = do
  putStrLn "Witaj w Hexcel!"
  printf   "Wersja: %.2f\n" version
  putStrLn "Hexcel jest to uniwersalne rozwiązanie potrzebne w każdym domu!"
  putStrLn "Program zapisuje pliki w aktualnym katalogu z rozszerzeniem .hexcel"
  menu

menu = do
  putStrLn ""
  putStrLn "> **** menu główne ****"
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
      writeFile name "[[Pusta]]"
      putStrLn ("> Utworzono arkusz " ++ name)
      return 0
    'o' -> do 
      putStrLn "> Otwieranie arkusza"
      putStrLn "Nazwa arkusza:"
      temp <- getLine
      let name = temp ++ ".hexcel"
      -- otwarcie edycja arkusza
      arkusz1 <- eaArkuszZPliku name
      arkusz2 <- edycjaArkuszaMenu name arkusz1
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
      putStrLn  "Dziękujemy za skorzystanie z Hexcel..."
      return 0
    _ -> do 
      putStrLn "Nieznana akcja"
      return 0
  if w /= 'x' then 
    menu
  else
    return 0

edycjaArkuszaMenu nazwa arkusz = do
  putStrLn ""
  putStrLn ("> menu arkusza " ++ nazwa)
  putStrLn "Wybierz jedną z poniższych opcji:"
  putStrLn "w - wypisz arkusz na ekran"
  putStrLn "e - edytuj dane w podanej komórce"
  putStrLn "c - dodanie kolumny"
  putStrLn "u - usuniecie kolumny"
  putStrLn "a - dodanie wiersza"
  putStrLn "d - usuniecie wiersza"
  putStrLn "s - zapisz arkusz na dysk"
  putStrLn "x - wróć do menu głównego"
  a <- getLine
  let w = if length a > 0 then (a !! 0) else ' '
  nowyarkusz <- case w of
    'w' -> do
      --let name = nazwa
      --arkusz <- eaArkuszZPliku name
      --let ark = eaArkuszaObliczWartoscWszystkichKomorek  $ [[Liczba 1, Napis "abs"], [Liczba 2, Suma (Zakres (0,0,0,2) Pusty) ]]
      let ark =eaArkuszaObliczWartoscWszystkichKomorek arkusz 
      print ark
      return arkusz
    'e' -> do
      putStrLn "Którą komórkę edytować? Numerowanie zaczynamy od (0, 0)"
      putStrLn "Wiersz: "
      wiersz <- getLine
      putStrLn "Kolumna: "
      kolumna <- getLine
      -- Zmiana String na Integer
      let x = read kolumna :: Integer
      let y = read wiersz  :: Integer
      -- Mozliwa edycja komorki to wstawienie : Liczby, Napisu, Sumy, Iloczynu lub Sredniej
      putStrLn "Wartosc do wstawienia: "
      putStrLn "Aby wstawic liczbe poprzedz ja slowem : Liczba , na przyklad: Liczba 5"
      putStrLn "Aby wstawic napis poprzedz go slowem : Napis , na przyklad: Napis \"abc\" (pamietaj o cudzyslowiu)"
      putStrLn "Aby obliczyc sume z danego zakresu : Suma , na przyklad: Suma (Komorka (0,1) (Komorka (0,2) Pusty))"
      putStrLn "Aby obliczyc iloczyn z danego zakresu : Iloczyn , na przyklad: Iloczyn (Zakres (0,0,0,3) (Komorka (1,1) Pusty))"      
      putStrLn "Aby obliczyc srednia : Srednia , na przyklad: Srednia (Komorka (1,2) (Zakres (0,0,0,3) Pusty)) "
      wart <- getLine
      let var1 = read wart :: Komorka
      --wywolanie funkcji 
      let arkusz2 = eaWstawWartosc var1 (fromIntegral x,fromIntegral y) arkusz
      arkusz3  <- if (eaCzyObliczalna (fromIntegral x,fromIntegral y) arkusz2 ) == False then
        do putStrLn "komenda zle wprowadzona, sprobuj ponownie"
           let ark = eaWstawWartosc Pusta (fromIntegral x,fromIntegral y) arkusz2
           return ark
      else 
        do let ark = eaWstawWartosc var1 (fromIntegral x,fromIntegral y) arkusz2
           putStrLn "Wartosc wpisana"
           return ark
      return arkusz3
    's' -> do
      putStrLn "Zapisywanie arkusza"
      eaArkuszDoPliku nazwa arkusz
      return arkusz
    'c' -> do
      putStrLn "Dodanie kolumny"
      putStrLn "Gdzie chciałbyś/chciałabyś dodać kolumne (po ktorej z istniejacych wstawic nowa)"
      num <- getLine
      let num1 = read num::Integer
      let ark = eaArkuszWstawKolumne (fromIntegral num1) arkusz
      return ark
    'u' -> do
      putStrLn "Usuniecie kolumny"
      putStrLn "Którą kolumnę usunąć? (pamietaj numeracja od zera)"
      num <- getLine
      let num1 = read num::Integer
      let ark = eaArkuszUsunKolumne (fromIntegral num1) arkusz
      return ark
    'a' -> do
      putStrLn "Dodanie wiersza"
      putStrLn "Gdzie chciałbyś/chciałabyś dodać wiersz (po ktorym z istniejacych wstawic nowy)"
      num <- getLine
      let num1 = read num::Integer
      let ark = eaArkuszWstawWiersz (fromIntegral num1) arkusz  
      return ark
    'd' -> do
      putStrLn "Usuniecie wiersza"
      putStrLn "Który wiersz usunąć? (pamietaj numeracja od zera)"
      num <-getLine
      let num1 = read num::Integer
      let ark = eaArkuszUsunWiersz (fromIntegral num1) arkusz  
      return ark
    'x' -> do
      return arkusz
    _ -> do
      putStrLn "Nieznana opcja"
      return arkusz 
  if w /= 'x' then 
    edycjaArkuszaMenu nazwa nowyarkusz
  else
    return 0

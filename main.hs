import System.IO
import System.Exit
import System.Directory
import Data.List
import Text.Printf
import EdycjaArkusza
import Data.Maybe
import Text.Read


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
      putStrLn ("> Utworzono pusty arkusz " ++ name)
      return 0
    'o' -> do 
      putStrLn "> Otwieranie arkusza"
      putStrLn "Nazwa arkusza:"
      temp <- getLine
      let name = temp ++ ".hexcel"
      exists <- doesFileExist name
      if exists then do
        -- otwarcie edycja arkusza
        arkusz1 <- eaArkuszZPliku name
        arkusz2 <- edycjaArkuszaMenu name arkusz1
        return 0
      else do
        putStrLn "Taki arkusz nie istnieje"
        return 0
      return 0
    'u' -> do
      putStrLn "> Usuwanie arkusza"
      putStrLn "Nazwa arkusza:"
      temp <- getLine
      let name = temp ++ ".hexcel"
      exists <- doesFileExist name
      if exists then do
        removeFile name
        putStrLn ("> Arkusz " ++ name ++ "usunięty")
        return 0
      else do
        putStrLn "Taki arkusz nie istnieje"
        return 0
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
  putStrLn "w - wypisz obliczony arkusz na ekran"
  putStrLn "W - wypisz arkusz na ekran w postaci nieobliczonej"
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
      let ark1 = eaArkuszaObliczWartoscWszystkichKomorekMaybe arkusz 
      if not $ eaCzyMoznaMaybeArkuszToArkusz ark1 then do
        putStrLn "Arkusz jest niepoprawny - niemożliwe jest pełne obliczenie wszystkich komórek arkusza."
        putStrLn "Wyswietlam arkusz w postaci nieobliczonej"
        print arkusz
        return 0
      else do
        let ark3 = eaMaybeArkuszToArkusz ark1
        print ark3
        return 0
      return arkusz
    'W' -> do
      print arkusz
      return arkusz
    'e' -> do

      -- pobieranie wartości
      putStrLn "Którą komórkę edytować? Numerowanie zaczynamy od (0, 0)"
      
      putStrLn "Kolumna: "
      kolumna <- getLine
      let x1 = readMaybe kolumna :: Maybe Integer

      putStrLn "Wiersz: "
      wiersz <- getLine
      let y1 = readMaybe wiersz :: Maybe Integer

      -- Mozliwa edycja komorki to wstawienie : Liczby, Napisu, Sumy, Iloczynu lub Sredniej
      putStrLn "Wartosc do wstawienia: "
      putStrLn "Aby wstawic liczbe poprzedz ja slowem : Liczba , na przyklad: Liczba 5"
      putStrLn "Aby wstawic napis poprzedz go slowem : Napis , na przyklad: Napis \"abc\" (pamietaj o cudzyslowiu)"
      putStrLn "Aby obliczyc sume z danego zakresu : Suma , na przyklad: Suma (Komorka (0,1) (Komorka (0,2) Pusty))"
      putStrLn "Aby obliczyc iloczyn z danego zakresu : Iloczyn , na przyklad: Iloczyn (Zakres (0,0,0,3) (Komorka (1,1) Pusty))"      
      putStrLn "Aby obliczyc srednia : Srednia , na przyklad: Srednia (Komorka (1,2) (Zakres (0,0,0,3) Pusty)) "
      wart <- getLine
      let var1 = readMaybe wart :: Maybe Komorka

      -- sprawdzanie błędów
      arkusz5 <- if x1 == Nothing then do
        putStrLn "Podana zostala niepoprawna wartosc kolumny."
        return arkusz
      else if y1 == Nothing then do
        putStrLn "Podana zostala niepoprawna wartosc wiersza."
        return arkusz
      else if var1 == Nothing then do
        putStrLn "Podana zostala niepoprawna wartosc komorki."
        return arkusz
      else do
        let var2 = fromJust var1
        let x2 = fromIntegral $ fromJust x1
        let y2 = fromIntegral $ fromJust y1
        let arkusz2 = eaWstawWartosc var2 (x2,y2) arkusz
        arkusz3 <- if (eaCzyObliczalna (x2, y2) arkusz2 ) == False then do 
            putStrLn "Podana formuła jest niepoprawna - nie można obliczyć jej wartości."
            putStrLn "Występuje albo pętla wieczna przy obliczaniu jej wartości"
            putStrLn " lub do obliczania wartości wykorzystywana jest komórka z napisem"
            return arkusz
        else do 
            putStrLn "Wartosc zostala poprawnie wpisana do arkusza"
            return arkusz2
        return arkusz3
      return arkusz5
    's' -> do
      putStrLn "Zapisywanie arkusza"
      eaArkuszDoPliku nazwa arkusz
      return arkusz
    'c' -> do
      putStrLn "Dodanie kolumny"
      putStrLn "Gdzie chciałbyś/chciałabyś dodać kolumne (po ktorej z istniejacych wstawic nowa)"
      putStrLn "Podanie wartosci wiekszej od istniejacej wielkości arkusza spowoduje dodanie jednej kolumny na końcu arkusza."
      num <- getLine
      let num1 = readMaybe num :: Maybe Integer
      ark <- if num1 == Nothing then do
        putStrLn "Podano niepoprawno wartość kolumny."
        return arkusz
      else do
        let ark = eaArkuszWstawKolumne (fromIntegral $ fromJust num1) arkusz
        return ark
      return ark
    'u' -> do
      putStrLn "Usuniecie kolumny"
      ark <- if eaArkuszPobierzIloscWierszy arkusz > 1 then do
        putStrLn "Którą kolumnę usunąć? (pamietaj numeracja od zera)"
        putStrLn "Podanie wartości wiekszej od ilości kolumn w arkuszu nic nie zrobi"
        num <- getLine
        let num1 = readMaybe num :: Maybe Integer     
        ark <- if num1 == Nothing then do
          putStrLn "Podano niepoprawno wartość kolumny."
          return arkusz
        else do
          return $ eaArkuszUsunKolumne (fromIntegral $ fromJust num1) arkusz
        return ark
      else do
        putStrLn "Arkusz ma tylko jedną kolumnę - nie można usunąć ostatniego wiersza."
        return arkusz
      return ark
    'a' -> do
      putStrLn "Dodanie wiersza"
      putStrLn "Gdzie chciałbyś/chciałabyś dodać wiersz (po ktorym z istniejacych wstawic nowy)"
      putStrLn "Podanie wartosci wiekszej od istniejacej wielkości arkusza spowoduje dodanie jednego wiersza na końcu arkusza."
      num <- getLine
      let num1 = readMaybe num :: Maybe Integer     
      ark <- if num1 == Nothing then do
        putStrLn "Podano niepoprawno wartość kolumny."
        return arkusz
      else do
        return $ eaArkuszWstawWiersz (fromIntegral $ fromJust num1) arkusz  
      return ark
    'd' -> do
      putStrLn "Usuniecie wiersza"
      ark <- if eaArkuszPobierzIloscWierszy arkusz > 1 then do
        putStrLn "Który wiersz usunąć? (pamietaj numeracja od zera)"
        putStrLn "Podanie wartości wiekszej od ilości wierszy w arkuszu nic nie zrobi"
        num <-getLine
        let num1 = readMaybe num :: Maybe Integer
        ark <- if num1 == Nothing then do
          putStrLn "Podano niepoprawno wartość kolumny."
          return arkusz
        else do
          return $ eaArkuszUsunWiersz (fromIntegral $ fromJust num1) arkusz  
        return ark
      else do
        putStrLn "Arkusz ma tylko jeden wiersz - nie można usunąć ostatniego wiersza."
        return arkusz
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

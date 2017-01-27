import EdycjaArkusza
import EdycjaArkuszaInternal
import System.IO
import Data.Either
import Debug.Trace

myerror str = do
  print $ show str
  return 0

test1 = do
  print "START test1 - read and show na zwyklych variablech"
  let output = [[Liczba 2],[Napis "abc"],[Liczba 1],[Suma (Komorka (0,1) (Komorka (0,2) Pusty)) ]]
  let string = "[[Liczba 2.0],[Napis \"abc\"],[Liczba 1.0],[Suma (Komorka (0,1) (Komorka (0,2) Pusty))]]"
  let resstring = show output
  let resoutput = read string :: Arkusz
  -- print string
  -- print resstring
  -- print output
  -- print resoutput
  if output /= resoutput || string /= resstring then
    myerror "test1" else return 0
  print "STOP test1"

test2 = do
  let output = [[Iloczyn Pusty],[Napis "abc"],[Liczba 1],[Suma (Komorka (0,1) (Komorka (0,2) Pusty)) ]]
  print "START test2 - write and read from file"
  writeFile "test.hexcel" $ show output -- wczytywanie z pliku
  filetxt <- readFile "test.hexcel" -- zapisywanie do pliku trzeba w dwoch linijkach ... 
  let result = read filetxt :: Arkusz
  -- print output
  -- print result
  if output /= result then
    myerror "test2" else return 0
  print "STOP test2"

test25 = do
  print "START test25 - test minimalizacji wielkosći arkusza - prosta"
  let input = [
        [Liczba 0, Suma (Komorka (1,0) Pusty), Pusta],
        [Liczba 1, Suma (Komorka (0,1) Pusty), Pusta],
        [Pusta, Pusta, Pusta]
        ]
  let output = [
        [Liczba 0, Suma (Komorka (1,0) Pusty)],
        [Liczba 1, Suma (Komorka (0,1) Pusty)]
        ]
  -- print $ input
  -- print $ eaArkuszMinimalizuj input
  if eaArkuszMinimalizuj input /= output then
    myerror "test8" else return 0
  print "STOP  test25"

test3 = do
  print "START test3 - wstawianie wartosci"
  let input = [ [Liczba 1],[Liczba 2] ]

  let outputzminim = [
        [Iloczyn Pusty],
        [Liczba 2],
        [Pusta,Liczba 3] 
        ]
  let resultzminmin = 
        eaWstawWartosc (Liczba 3) (1,2) $ 
        eaWstawWartosc (Iloczyn Pusty) (0,0) input

  let outputbezminim = [
        [Iloczyn Pusty,Pusta],
        [Liczba 2,Pusta],
        [Pusta,Liczba 3] 
        ]
  let resultbezminimin = 
        eaWstawWartoscBezMinimalizacji (Liczba 3) (1,2) $
        eaWstawWartoscBezMinimalizacji (Iloczyn Pusty) (0,0) input
    
  --print input
  --print outputzminim
  --print resultzminmin
  --print outputbezminim
  --print resultbezminimin

  if outputbezminim /= resultbezminimin then
    myerror "test3.1" else return 0
  if outputzminim /= resultzminmin then
    myerror "test3.2" else return 0
  print "STOP test3"

test4 = do
  print "START test4 - test arytmetiki bez zakresow"
  let input = [
        [Liczba 1, Napis "abs"],
        [Liczba 2, Suma (Komorka (0,0) (Komorka (0,1) Pusty)) ],
        [Liczba 3, Iloczyn (Komorka (0,1) (Komorka (1,1) Pusty)) ],
        [Liczba 4, Srednia (Komorka (0,0) (Komorka (0,1) Pusty)) ]
        ]
  {-print $ eaObliczWartosc (0,0) input
  print $ eaObliczWartosc (0,1) input
  print $ eaObliczWartosc (0,2) input
  print $ eaObliczWartosc (0,3) input
  print $ eaObliczWartosc (1,1) input
  print $ eaObliczWartosc (1,2) input
  print $ eaObliczWartosc (1,3) input-}
  if
    eaObliczWartosc (0,0) input /= 1 ||
    eaObliczWartosc (0,1) input /= 2 ||
    eaObliczWartosc (0,2) input /= 3 ||
    eaObliczWartosc (0,3) input /= 4 ||
    eaObliczWartosc (1,1) input /= 3 ||
    eaObliczWartosc (1,2) input /= 6 ||
    eaObliczWartosc (1,3) input /= 1.5 
  then
    myerror "test4"
  else
    return 0
  print "STOP  test4"

test5 = do
  print "START test5 - test arytmetiki z zakresami"
  let input = [
        [Liczba 1, Napis "abs"],
        [Liczba 2, Suma (Zakres (0,0,0,2) Pusty) ],
        [Liczba 3, Iloczyn (Zakres (0,0,0,3) (Komorka (1,1) Pusty)) ],
        [Liczba 4, Srednia (Zakres (0,0,0,3) Pusty) ],
        [Liczba 4, Srednia (Zakres (0,0,0,3) (Komorka (1,2) Pusty)) ],
        [Liczba 5, Srednia (Komorka (1,2) (Zakres (0,0,0,3) Pusty)) ]
        ]
  -- print $ eaObliczWartosc (1,1) input
  -- print $ eaObliczWartosc (1,2) input
  -- print $ eaObliczWartosc (1,3) input
  -- print $ eaObliczWartosc (1,4) input
  -- print $ eaPobierzKomorke (1,1) input
  -- print $ eaPobierzPrzedzial (1,1) input
  -- print $ flip eaPrzedzialDoZbiorWspol input $ eaPobierzPrzedzial (1,1) input
  if eaObliczWartosc (1,1) input /= 6 then
    myerror $ eaWypiszKomorke (1,1) input
  else
    return 0
  if eaObliczWartosc (1,2) input /= 144 then
    myerror $ eaWypiszKomorke (1,2) input
  else
    return 0
  if eaObliczWartosc (1,3) input /= 2.5 then
    myerror $ eaWypiszKomorke (1,3) input
  else
    return 0
  if eaObliczWartosc (1,4) input /= 30.8 then
    myerror $ eaWypiszKomorke (1,4) input
  else
    return 0
  if eaObliczWartosc (1,4) input /= eaObliczWartosc (1,5) input then
    myerror $ eaWypiszKomorke (1,5) input
  else
    return 0
  print "STOP  test5"

test6 = do
  print "START test6 - wykrywanie cyklicznych zależności przy obliczaniu formuł"
  let input = [
        [Liczba 0, Suma (Komorka (1,0) Pusty)], -- True , smaa na siebie
        [Liczba 1, Suma (Komorka (0,1) Pusty)], -- False
        [Liczba 2, Suma (Zakres (0,2,1,2) Pusty) ], -- True , pokrywa sie zakresem
        [Liczba 3, Suma (Zakres (0,0,0,4) (Komorka (1,1) Pusty)) ], -- False
        [Liczba 4, Suma (Zakres (0,0,1,0) Pusty) ], -- True - zakres wskazuje na komorke ktora sie pokrywa
        -- False, przedzial wskazuje na komorke ktora wskazuje na zakres
        [Liczba 5, Suma (Zakres (0,3,1,3) (Komorka (1,3) Pusty)) ],
        -- True przedzil wskazje na zakres ktora wsakzuje na zakres który pokrywa się z samm sobą
        [Liczba 6, Suma (Zakres (0,2,1,2) Pusty) ]
        ]
  --print $ eaCzyNieskonczonaPetla (1,0) input
  --print $ eaCzyNieskonczonaPetla (1,1) input
  --print $ eaCzyNieskonczonaPetla (1,2) input
  --print $ eaCzyNieskonczonaPetla (1,3) input
  --print $ eaCzyNieskonczonaPetla (1,4) input
  --print $ eaCzyNieskonczonaPetla (1,5) input
  --print $ eaCzyNieskonczonaPetla (1,6) input
  if eaCzyNieskonczonaPetla (1,0) input /= True then myerror $ eaWypiszKomorke (1,0) input else return 0
  if eaCzyNieskonczonaPetla (1,1) input /= False then myerror $ eaWypiszKomorke (1,1) input else return 0
  if eaCzyNieskonczonaPetla (1,2) input /= True then myerror $ eaWypiszKomorke (1,2) input else return 0
  if eaCzyNieskonczonaPetla (1,3) input /= False then myerror $ eaWypiszKomorke (1,3) input else return 0
  if eaCzyNieskonczonaPetla (1,4) input /= True then myerror $ eaWypiszKomorke (1,4) input else return 0
  if eaCzyNieskonczonaPetla (1,5) input /= False then myerror $ eaWypiszKomorke (1,5) input else return 0
  if eaCzyNieskonczonaPetla (1,6) input /= True then myerror $ eaWypiszKomorke (1,6) input else return 0
  print "STOP  test6"

test7 = do
  print "START test7 - test zmiany wielkosci arkusza "
  let input = [
        [Napis "00", Liczba 10, Napis "20" ],
        [Napis "01", Liczba 11, Napis "21" ]
        ]
  let outputkolumna1 = [
        [Napis "00", Pusta, Liczba 10, Napis "20" ],
        [Napis "01", Pusta, Liczba 11, Napis "21" ]
        ]
  let outputkolunabez2 = [
        [Napis "00", Liczba 10 ],
        [Napis "01", Liczba 11 ]
        ]
  let outputwiersz2 = [
        [Napis "00", Liczba 10, Napis "20" ],
        [Napis "01", Liczba 11, Napis "21" ],
        [Pusta, Pusta, Pusta]
        ]
  let outputwierszbez0 = [
        [Napis "01", Liczba 11, Napis "21" ]
        ]
  -- print $ eaArkuszPobierzIloscKolumn input
  -- print $ eaArkuszPobierzIloscWierszy input
  -- print $ eaArkuszWstawKolumne 1 input
  -- print $ eaArkuszUsunKolumne 2 input
  -- print $ eaArkuszWstawWiersz 2 input
  -- print $ eaArkuszUsunWiersz 0 input
  if 
    eaArkuszPobierzIloscKolumn input /= 3 ||
    eaArkuszPobierzIloscWierszy input /= 2
    then myerror "test7.1" else return 0
  if 
    eaArkuszWstawKolumne 1 input /= outputkolumna1 ||
    eaArkuszUsunKolumne  2 input /= outputkolunabez2
    then myerror "test7.2" else return 0
  if 
    eaArkuszWstawWiersz 2 input /= outputwiersz2 ||
    eaArkuszUsunWiersz  0 input /= outputwierszbez0
    then myerror "test7.3" else return 0
  print "STOP  test7"

test8 = do
  print "START test8 - sprawdzanie wczytywania i zapisywania do pliku"
  let arkusz = [[Liczba 1]]
  let nazwaPliku = "test8.hexcel"
  eaArkuszDoPliku nazwaPliku arkusz   -- jak zapisujemy, musimy byc wewnatrz do block
  result <- eaArkuszZPliku nazwaPliku -- JAK WCZYTUJEMY TRZEBA ZROBIC '<-' wewnatrzn do block
  if 
    arkusz /= [[Liczba 1]] ||
    result /= [[Liczba 1]]
    then myerror "test8" else return 0
  print "STOP  test8"

test9 = do
  print "START test9 - sprawdzenie wstawiania wartości" 
  -- podczas minimalizacji arkusza nalezy zostawić wiersze zawierające minimum jedna kolumne pusta, jeśli nie są ostatnimi
  -- zatem minimalizacja arkusza [[Pusta,Pusta],[Pusta,Liczba 1],[Pusta,Pusta]] powinna dac: [[Pusta],[Pusta,Liczba 1]]
  if 
    eaWstawWartosc (Liczba 1) (1,2) [[Pusta,Pusta],[Pusta,Pusta]] /= [[Pusta],[Pusta],[Pusta,Liczba 1]] ||
    eaWstawWartosc (Liczba 1) (1,2) [[Pusta,Pusta],[Pusta,Pusta],[Pusta,Pusta],[Pusta,Pusta]] /= [[Pusta],[Pusta],[Pusta,Liczba 1]]
    then myerror "test9" else return 0
  print "STOP  test9"

test10 = do
  print "START test10 - obliczanie wartosci calego arkusza"
  let input1 = [
        [Liczba 1, Napis "abs"],
        [Liczba 2, Suma (Zakres (0,0,0,2) Pusty) ],
        [Liczba 3, Iloczyn (Zakres (0,0,0,3) (Komorka (1,1) Pusty)) ],
        [Liczba 4, Srednia (Zakres (0,0,0,3) Pusty) ],
        [Liczba 4, Srednia (Zakres (0,0,0,3) (Komorka (1,2) Pusty)) ],
        [Liczba 5, Srednia (Komorka (1,2) (Zakres (0,0,0,3) Pusty)) ]
        ]
  let output1 = [
        [Liczba 1.0,Napis "abs"],
        [Liczba 2.0,Liczba 6.0],
        [Liczba 3.0,Liczba 144.0],
        [Liczba 4.0,Liczba 2.5],
        [Liczba 4.0,Liczba 30.8],
        [Liczba 5.0,Liczba 30.8]
        ]
  if eaArkuszaObliczWartoscWszystkichKomorek input1 /= output1
    then myerror "test10.1" else return 0
  let input2 = [
        [Liczba 0, Suma (Komorka (1,0) Pusty)], -- True , smaa na siebie
        [Liczba 1, Suma (Komorka (0,1) Pusty)], -- False
        [Liczba 2, Suma (Zakres (0,2,1,2) Pusty) ], -- True , pokrywa sie zakresem
        [Liczba 3, Suma (Zakres (0,0,0,4) (Komorka (1,1) Pusty)) ], -- False
        [Liczba 4, Suma (Zakres (0,0,1,0) Pusty) ], -- True - zakres wskazuje na komorke ktora sie pokrywa
        -- False, przedzial wskazuje na komorke ktora wskazuje na zakres
        [Liczba 5, Suma (Zakres (0,3,1,3) (Komorka (1,3) Pusty)) ],
        -- True przedzil wskazje na zakres ktora wsakzuje na zakres który pokrywa się z samm sobą
        [Liczba 6, Suma (Zakres (0,2,1,2) Pusty) ]
        ]
  let output2 = [
        [Just (Liczba 0.0),Nothing],
        [Just (Liczba 1.0),Just (Liczba 1.0)],
        [Just (Liczba 2.0),Nothing],
        [Just (Liczba 3.0),Just (Liczba 11.0)],
        [Just (Liczba 4.0),Nothing],
        [Just (Liczba 5.0),Just (Liczba 25.0)],
        [Just (Liczba 6.0),Nothing]
        ]
  if eaArkuszaObliczWartoscWszystkichKomorekMaybe input2 /= output2
    then myerror "test10.2" else return 0
  print "STOP  test10"
  
test11 = do
  print "START test11 - wykrywanie czy formuła jest obliczalna"
  let input = [
        [Liczba (-1), Liczba 0], -- True, liczba jest obliczalna
        [Liczba 0, Napis "123"], -- false, napis nie jest obliczalny
        [Liczba 1, Suma (Komorka (1,1) Pusty)], -- False, bo oblicza z napisu
        [Liczba 2, Pusta], -- Pusta komorka jest obliczalna i jej wartosc wynosi 0
        [Liczba 3, Suma (Zakres (0,0,100,100) Pusty)] -- False
        -- nieistniejace komorki sa obliczalne, bo sa puste
        ]
  {-print $ eaCzyObliczalna (1,0) input
  print $ eaCzyObliczalna (1,1) input
  print $ eaCzyObliczalna (1,2) input
  print $ eaCzyObliczalna (1,3) input
  print $ eaCzyObliczalna (1,4) input
  print $ eaCzyObliczalna (1,5) input-}
  if eaCzyObliczalna (1,0) input /= True then myerror $ eaWypiszKomorke (1,0) input else return 0
  if eaCzyObliczalna (1,1) input /= False then myerror $ eaWypiszKomorke (1,1) input else return 0
  if eaCzyObliczalna (1,2) input /= False then myerror $ eaWypiszKomorke (1,2) input else return 0
  if eaCzyObliczalna (1,3) input /= True then myerror $ eaWypiszKomorke (1,3) input else return 0
  if eaCzyObliczalna (1,4) input /= False then myerror $ eaWypiszKomorke (1,4) input else return 0
  if eaCzyObliczalna (1,5) input /= True then myerror $ eaWypiszKomorke (1,5) input else return 0
  print "STOP  test11"

test12 = do
  print "START test12 - wstawianie + autoresize + autominimalizacja"
  if eaWstawWartosc (Liczba 1) (10,10) [[Pusta]] /= [[Pusta],[Pusta],[Pusta],[Pusta],[Pusta],[Pusta],[Pusta],[Pusta],[Pusta],[Pusta],[Pusta,Pusta,Pusta,Pusta,Pusta,Pusta,Pusta,Pusta,Pusta,Pusta,Liczba 1.0]]
    then myerror "test10.2" else return 0
  print "START test12"

test13 = do
  print "START test13 - check pruning"
  let arkusz = [[Liczba 1,Pusta],[Pusta,Pusta],[Pusta,Liczba 5,Pusta]]
  print $ eaArkuszMinimalizuj arkusz
  --if 
  --  then myerror "test10.2" else return 0
  print "START test13"  

main = do
  test1
  test25
  test2
  test3
  test4
  test5
  test6
  test7
  test8
  test9
  test10
  test11
  test12
  test13


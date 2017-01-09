import EdycjaArkusza
import EdycjaArkuszaInternal
import System.IO
import Data.Either
import Debug.Trace

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
    error "test1"
  else
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
    error "test2"
  else
    print "STOP test2"

test25 = do
  print "START test25 - test minimalizacji wielkosći arkusza"
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
    error "test8" else return 0
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
    error "test3.1" else return 0
  if outputzminim /= resultzminmin then
    error "test3.2" else return 0
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
    error "test4"
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
    error $ eaWypiszKomorke (1,1) input
  else
    return 0
  if eaObliczWartosc (1,2) input /= 144 then
    error $ eaWypiszKomorke (1,2) input
  else
    return 0
  if eaObliczWartosc (1,3) input /= 2.5 then
    error $ eaWypiszKomorke (1,3) input
  else
    return 0
  if eaObliczWartosc (1,4) input /= 30.8 then
    error $ eaWypiszKomorke (1,4) input
  else
    return 0
  if eaObliczWartosc (1,4) input /= eaObliczWartosc (1,5) input then
    error $ eaWypiszKomorke (1,5) input
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
  if eaCzyNieskonczonaPetla (1,0) input /= True then error $ eaWypiszKomorke (1,0) input else return 0
  if eaCzyNieskonczonaPetla (1,1) input /= False then error $ eaWypiszKomorke (1,1) input else return 0
  if eaCzyNieskonczonaPetla (1,2) input /= True then error $ eaWypiszKomorke (1,2) input else return 0
  if eaCzyNieskonczonaPetla (1,3) input /= False then error $ eaWypiszKomorke (1,3) input else return 0
  if eaCzyNieskonczonaPetla (1,4) input /= True then error $ eaWypiszKomorke (1,4) input else return 0
  if eaCzyNieskonczonaPetla (1,5) input /= False then error $ eaWypiszKomorke (1,5) input else return 0
  if eaCzyNieskonczonaPetla (1,6) input /= True then error $ eaWypiszKomorke (1,6) input else return 0
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
    then error "test7.1" else return 0
  if 
    eaArkuszWstawKolumne 1 input /= outputkolumna1 ||
    eaArkuszUsunKolumne  2 input /= outputkolunabez2
    then error "test7.2" else return 0
  if 
    eaArkuszWstawWiersz 2 input /= outputwiersz2 ||
    eaArkuszUsunWiersz  0 input /= outputwierszbez0
    then error "test7.3" else return 0
  print "STOP  test7"

main = do
  test1
  test25
  test2
  test3
  test4
  test5
  test6
  test7

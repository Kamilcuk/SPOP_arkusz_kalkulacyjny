import EdycjaArkusza
import System.IO
import Text.ParserCombinators.Parsec
import Data.Either.Utils
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

test3 = do
  print "START test3 - wstawianie wartosci"
  let input = [ [Liczba 1],[Liczba 2] ]
  let output = 
        [ [Iloczyn Pusty,Pusta],
        [Liczba 2,Pusta],
        [Pusta,Liczba 3] ]
  let result = eaWstawWartosc (Liczba 3) (1,2) $ eaWstawWartosc (Iloczyn Pusty) (0,0) input
  -- print input
  -- print output
  -- print result
  if output /= result then
    error "test3"
  else
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
  print $ eaObliczWartosc (1,3) input -}
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
        [Liczba 4, Srednia (Zakres (0,0,0,3) (Komorka (1,2) Pusty)) ]
        ]
  -- print $ eaObliczWartosc (1,1) input
  -- print $ eaObliczWartosc (1,2) input
  -- print $ eaObliczWartosc (1,3) input
  -- print $ eaObliczWartosc (1,4) input
  if
    eaObliczWartosc (1,1) input /= 6   ||
    eaObliczWartosc (1,2) input /= 144 ||
    eaObliczWartosc (1,3) input /= 2.5 ||
    eaObliczWartosc (1,4) input /= 38.5 
  then
    error "test5"
  else
    return 0
  print "STOP  test5"


main = do
  test1
  test2
  test3
  test4
  test5
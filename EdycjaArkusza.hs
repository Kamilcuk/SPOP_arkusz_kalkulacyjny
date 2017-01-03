module EdycjaArkusza where
import Prelude
import Data.Foldable
import Data.Sequence

----------------------- convetion -------------------------
{-
  Wszystkie eksportowane funkcje zaczynają się od ea ( EditArkusz / EdytujArkusz )
-}
{- ---------------------- global help funcitons ---------------------------------- -}

-- resizeX
resizeX :: [a] -> Int -> [a] -> [a]
resizeX _ 0 list = list
resizeX item len [] = item ++ resizeX item (len-1) []
resizeX item len (l:list)= [l] ++ resizeX item (len-1) list

-- resizeXY - resize array in both dimensions
resizeXY :: [a] -> (Int, Int) -> [[a]] -> [[a]]
resizeXY e (x,y) list = map (resizeX e x) $ resizeX [e] y list

removeHead :: [a] -> [a]
removeHead [] = error "Empty list!"
removeHead (x:xs) = xs

{- ---------------------- global data declarations ---------------------------------- -}

{- 
zawartością komórki może być:
o napis,
o liczba,
o funkcja sumowania – dodaje liczby w podanych komórkach/zakresach komórek,
o funkcja iloczynu – mnoży liczby w podanych komórkach/zakresach komórek,
o funkcja wartości średniej – zwraca średnią arytmetyczną liczb w podanych komórkach/zakresach komórek; 

typ komórki jest pierwszym znakiem w niej
-}

data Przedzial = Komorka (Int,Int) Przedzial | Zakres (Int,Int,Int,Int) Przedzial | Pusty
  deriving (Show, Read, Eq)
data Komorka = Napis String | Liczba Float | Suma Przedzial | Iloczyn Przedzial | Srednia Przedzial | Pusta
  deriving (Show, Read, Eq)
type Arkusz = [[Komorka]]

{- w jaki sposob sie numeruje arkusz? a no taki:

 -------------------------X
| (0,0) | (1,0) | (2,0)
|-----------------------
| (0,1) | (1,1) | (2,1)
|-----------------------
| (0,2) | (1,2) | (2,2)
|-----------------------
|
Y
-}

eaResize :: (Int, Int) -> Arkusz -> Arkusz
eaResize = resizeXY [Pusta]

-- | wstawia wartość w punkt x y w podanej podwojnej tablicy (csv)
eaWstawWartosc :: Komorka -> (Int, Int) -> Arkusz  -> Arkusz 
eaWstawWartosc str (x,y) csv = toList . update y ( toList . update x str $ fromList $ eaResize ( (x+1), (y+1) ) csv !! y ) $ fromList $ eaResize ( (x+1), (y+1) ) csv

eaPobierzKomorke :: (Int, Int) -> Arkusz -> Komorka
eaPobierzKomorke (x,y) csv = csv !! y !! x

eaPobierzStringKomorki :: Komorka -> String
eaPobierzStringKomorki (Napis x) = x

eaPobierzLiczbeKomorki :: Komorka -> Float
eaPobierzLiczbeKomorki (Liczba x) = x

eaPobierzPrzedzialKomorki :: Komorka -> Przedzial
eaPobierzPrzedzialKomorki (Suma x) = x
eaPobierzPrzedzialKomorki (Iloczyn x) = x
eaPobierzPrzedzialKomorki (Srednia x) = x

eaPobierzLiczbe :: (Int, Int) -> Arkusz -> Float
eaPobierzLiczbe (x,y) ark = eaPobierzLiczbeKomorki $ eaPobierzKomorke (x,y) ark

eaIloscElementowDoWartosciPrzedzialu :: Przedzial -> Int
eaIloscElementowDoWartosciPrzedzialu (Pusty) = 0
eaIloscElementowDoWartosciPrzedzialu (Komorka _ p) = 1 + eaIloscElementowDoWartosciPrzedzialu p
eaIloscElementowDoWartosciPrzedzialu (Zakres (a,b,c,d) p) = Prelude.length [ (x,y) | x <- [a..b], y <- [c..d] ] 

-- (b -> a -> b) -> b -> t a -> b
eaObliczWartoscPrzedzialu :: (Float -> Float -> Float) -> Float -> Przedzial -> Arkusz -> Float
eaObliczWartoscPrzedzialu _ n Pusty _ = n
eaObliczWartoscPrzedzialu op n (Komorka c p) ark = op (eaObliczWartosc c ark) (eaObliczWartoscPrzedzialu op n p ark)
eaObliczWartoscPrzedzialu op n (Zakres (a,b,c,d) p) ark = 
    foldl op n 
    (
    (map (flip eaObliczWartosc ark) [ (x,y) | x <- [a..b], y <- [c..d] ] ) ++
    [ (eaObliczWartoscPrzedzialu op n p ark) ]
    )

eaObliczWartoscKomorki :: Komorka -> Arkusz -> Float
eaObliczWartoscKomorki (Liczba x) _ = x
eaObliczWartoscKomorki (Suma p) ark = eaObliczWartoscPrzedzialu (+) 0 p ark
eaObliczWartoscKomorki (Iloczyn p) ark = eaObliczWartoscPrzedzialu (*) 1 p ark
eaObliczWartoscKomorki (Srednia p) ark = (eaObliczWartoscPrzedzialu (+) 0 p ark) / fromIntegral (eaIloscElementowDoWartosciPrzedzialu p)
eaObliczWartoscKomorki (Napis _) _ = error "eaObliczWartoscKomorki na napisie!"

eaObliczWartosc :: (Int, Int) -> Arkusz -> Float
eaObliczWartosc c ark = eaObliczWartoscKomorki ( eaPobierzKomorke c ark ) ark


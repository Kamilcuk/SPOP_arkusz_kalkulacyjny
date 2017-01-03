module EdycjaArkuszaDatatypes where

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
type Wspol = (Int,Int) -- Wspołrzędne (x,y)
type Zakres = (Int,Int,Int,Int)
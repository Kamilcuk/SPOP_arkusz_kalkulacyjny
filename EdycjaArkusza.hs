module EdycjaArkusza (module EdycjaArkusza, module EdycjaArkuszaDatatypes) where
import EdycjaArkuszaInternal
import EdycjaArkuszaDatatypes
import Prelude
import Data.List
import Debug.Trace
import System.IO
import Data.Maybe


----------------------- convetion -------------------------
{-
  Wszystkie eksportowane funkcje zaczynają się od ea ( EditArkusz / EdytujArkusz )
-}
--------------------------------- funkcje lokalne ---------------------------------------

------------------------------------- operacje zmieniajace arkusz ----------------------

eaResize :: (Int, Int) -> Arkusz -> Arkusz
eaResize = resizeXY [Pusta]

-- Zmienia kazdy wiersz postaci: [Pusta, Pusta, ...] na [Pusta]
eaArkuszMinimalizujWiersze :: Arkusz -> Arkusz
eaArkuszMinimalizujWiersze ark = map ( deleteLastsExceptOne (Pusta ==) ) ark

-- Usuwa wszystkie pusteWiersz [Pusta] na koncu arkusza
eaArkuszMinimalizujKolumny :: Arkusz -> Arkusz
eaArkuszMinimalizujKolumny ark = deleteLastsExceptOne ([Pusta] ==) ark


eaWstawWartoscBezMinimalizacji :: Komorka -> (Int, Int) -> Arkusz  -> Arkusz 
eaWstawWartoscBezMinimalizacji str (x,y) ark = replaceXY (x,y) str $ eaResize ( (x+1), (y+1) ) ark

eaArkuszMinimalizuj :: Arkusz -> Arkusz
eaArkuszMinimalizuj ark = eaArkuszMinimalizujKolumny $ eaArkuszMinimalizujWiersze ark

------------------------------------- eaCzyNieskonczonaPetla ----------------------

-- TUTAJ NIE PATRZ TO SĄ MOJE FUNKCJE LOKALNE 
eaCzyNieskonczonaPetla_intrace :: [(Int,Int)] -> Przedzial -> Arkusz -> Bool
eaCzyNieskonczonaPetla_intrace przebadane przedzial ark =
  if Prelude.null $ (trace ("Intersect: " ++ show przebadane ++ show zbiorWPrzedziale) (intersect przebadane zbiorWPrzedziale)) then
    foldl (||) False $ 
      map (flip ( trace ("eaCzyNieskonczonaPetla_intrace: ") (eaCzyNieskonczonaPetla_intrace nastepnePrzebadane) ) ark ) $
        trace ("mapa: zbiorWPrzedziale -> zbiorPrzedzialow " ++ show ( map (flip eaPobierzPrzedzial ark) zbiorWPrzedziale) ) 
          (map (flip eaPobierzPrzedzial ark) zbiorWPrzedziale)
  else
    True
  where
    zbiorWPrzedziale = trace ("zbiorWPrzedziale: " ++ show przedzial ++ " -> " ++ show ( eaPrzedzialDoZbiorWspol przedzial ) ) (eaPrzedzialDoZbiorWspol przedzial)
    nastepnePrzebadane = trace ("unnion: " ++ show przebadane ++ show zbiorWPrzedziale) ( Prelude.filter (flip eaCzyZPrzedzialem ark) ( union przebadane zbiorWPrzedziale ) )

eaCzyNieskonczonaPetla_in :: [(Int,Int)] -> Przedzial -> Arkusz -> Bool
eaCzyNieskonczonaPetla_in przebadane przedzial ark =
  if Prelude.null $ intersect przebadane zbiorWPrzedziale then
    foldl (||) False $ 
      map (flip (eaCzyNieskonczonaPetla_in nastepnePrzebadane) ark ) $
          map (flip eaPobierzPrzedzial ark) zbiorWPrzedziale
  else
    True
  where
    zbiorWPrzedziale = eaPrzedzialDoZbiorWspol przedzial
    nastepnePrzebadane = Prelude.filter (flip eaCzyZPrzedzialem ark) (union przebadane zbiorWPrzedziale )

-- Sprawdza czy przy obliczaniu danej współrzędnej wewnątrzn arkusza wystepuje wieczna pętla
eaCzyNieskonczonaPetla :: (Int,Int) -> Arkusz -> Bool
eaCzyNieskonczonaPetla w ark = eaCzyNieskonczonaPetla_in wBezLiczb (eaPobierzPrzedzial w ark) ark
  where wBezLiczb = Prelude.filter (flip eaCzyZPrzedzialem ark) [w]

eaCzyDoPrzedzialuSaSameLiczby :: (Int, Int) -> Arkusz -> Bool
eaCzyDoPrzedzialuSaSameLiczby w ark = 
  if eaCzyZPrzedzialem w ark then
    foldl (&&) True $ map (flip eaCzyDoPrzedzialuSaSameLiczby ark) (eaPrzedzialDoZbiorWspol (eaPobierzPrzedzial w ark))
  else
    eaCzyKomorkaObliczalna (eaPobierzKomorke w ark) w ark

eaCzyPrzedzialObliczalny :: Wspol -> Arkusz -> Bool
eaCzyPrzedzialObliczalny w ark = (not $ eaCzyNieskonczonaPetla w ark) && (eaCzyDoPrzedzialuSaSameLiczby w ark)

eaCzyKomorkaObliczalna :: Komorka -> Wspol -> Arkusz -> Bool
eaCzyKomorkaObliczalna (Suma _) w ark = eaCzyPrzedzialObliczalny w ark 
eaCzyKomorkaObliczalna (Iloczyn _) w ark = eaCzyPrzedzialObliczalny w ark
eaCzyKomorkaObliczalna (Srednia _) w ark = eaCzyPrzedzialObliczalny w ark
eaCzyKomorkaObliczalna (Liczba _) _ _ = True
eaCzyKomorkaObliczalna (Napis _) _ _ = False
eaCzyKomorkaObliczalna (Pusta) _ _  = True

---------------------------- obliczanie wartosci komorek + wyswietlanie -------------------------------- 

eaZakresDoZbioruWspol :: (Int,Int,Int,Int) -> [(Int,Int)]
eaZakresDoZbioruWspol (a,b,c,d) = [ (x,y) | x <- [a..c], y <- [b..d] ]

eaIloscElementowDoWartosciPrzedzialu :: Przedzial -> Int
eaIloscElementowDoWartosciPrzedzialu (Pusty) = 0
eaIloscElementowDoWartosciPrzedzialu (Komorka _ p) = 1 + eaIloscElementowDoWartosciPrzedzialu p
eaIloscElementowDoWartosciPrzedzialu (Zakres z p) = length ( eaZakresDoZbioruWspol z ) + eaIloscElementowDoWartosciPrzedzialu p

eaPrzedzialDoZbiorWspol :: Przedzial -> [(Int,Int)]
eaPrzedzialDoZbiorWspol (Pusty) = []
eaPrzedzialDoZbiorWspol (Komorka w p)= w : eaPrzedzialDoZbiorWspol p
eaPrzedzialDoZbiorWspol (Zakres z p) = eaZakresDoZbioruWspol z ++ eaPrzedzialDoZbiorWspol p

eaObliczWartoscKomorkiUnsafe :: Komorka -> Arkusz -> Float
eaObliczWartoscKomorkiUnsafe (Napis _) _ = error "eaObliczWartoscKomorkiUnsafe na napisie!"
eaObliczWartoscKomorkiUnsafe k ark = fromJust $ eaObliczWartoscKomorkiMaybe k ark

eaObliczWartoscUnsafe :: (Int, Int) -> Arkusz -> Float
eaObliczWartoscUnsafe w ark = eaObliczWartoscKomorkiUnsafe ( eaPobierzKomorke w ark ) ark


--------------------- dodatkowe funkcje - maybe  ---------------------

-- Zwaraca wartosc Liczba x dla danej Komorki  danego arkusza
-- lub zwraca nothing jesli nie da się obliczyć
eaObliczWartoscKomorkiMaybe :: Komorka -> Arkusz -> Maybe Float
eaObliczWartoscKomorkiMaybe (Liczba x) _ = Just $ x
eaObliczWartoscKomorkiMaybe (Suma p) ark = Just $ foldl (+) 0 $ map (flip eaObliczWartosc ark) (eaPrzedzialDoZbiorWspol p) 
eaObliczWartoscKomorkiMaybe (Iloczyn p) ark = Just $ foldl (*) 1 $ map (flip eaObliczWartosc ark) (eaPrzedzialDoZbiorWspol p)
eaObliczWartoscKomorkiMaybe (Srednia p) ark = Just $ eaObliczWartoscKomorkiUnsafe (Suma p) ark / fromIntegral (eaIloscElementowDoWartosciPrzedzialu p)
eaObliczWartoscKomorkiMaybe (Pusta) _ = Just $ 0
eaObliczWartoscKomorkiMaybe (Napis _) _ = Nothing


-- Zmienna Maybe Float jest zamieniana na Maybe Liczba x
eaConvertMaybeFloatToKomorkaLiczba :: Maybe Float -> Maybe Komorka
eaConvertMaybeFloatToKomorkaLiczba (Just x) = Just (Liczba x)
eaConvertMaybeFloatToKomorkaLiczba Nothing = Nothing

-- oblicza wartosc danej komorki, jesli jest to mozliwe
eaPobierzObliczonaKomorkeMaybe :: (Int, Int) -> Arkusz -> Maybe Komorka
eaPobierzObliczonaKomorkeMaybe w ark = 
  if eaCzyLiczbaLubZPrzedzialem w ark then
    eaConvertMaybeFloatToKomorkaLiczba $ eaObliczWartoscMaybe w ark
  else
    Just $ eaPobierzKomorke w ark


eaArkuszaObliczWartoscWszystkichKomorek_in3 :: Arkusz -> (Int, Int) -> Komorka -> Maybe Komorka
eaArkuszaObliczWartoscWszystkichKomorek_in3 ark (x,y) k = eaPobierzObliczonaKomorkeMaybe (x,y) ark
eaArkuszaObliczWartoscWszystkichKomorek_in2 :: Arkusz -> Int -> (Int, Komorka) -> Maybe Komorka
eaArkuszaObliczWartoscWszystkichKomorek_in2 ark x ks = eaArkuszaObliczWartoscWszystkichKomorek_in3 ark (fst ks,x) (snd ks)
eaArkuszaObliczWartoscWszystkichKomorek_in1 :: Arkusz -> (Int, [(Int, Komorka)]) -> [Maybe Komorka]
eaArkuszaObliczWartoscWszystkichKomorek_in1 ark (x, ks) = map (eaArkuszaObliczWartoscWszystkichKomorek_in2 ark x) ks


-------------------------- NIE KORZYSTAJ Z TEGO CO POWYZEJ (no chyba ze musisz) --------------
-------------------------- POWYZEJ SA MOJE FUNKCJE LOKALNE --------------------------------
-------------------------- CZYTAJ OD TEGO MIEJSCA! ----------------------------------------
{- ---------------------- public data declarations ---------------------------------- -}

{- 

w jaki sposob sie numeruje arkusz? Taki sam jak numeruje się piksele na ekranie:
a no taki:

T-------T-------T---------> X
| (0,0) | (1,0) | (2,0)
|-------X-------X-------
| (0,1) | (1,1) | (2,1)
|-------X-------X-------
| (0,2) | (1,2) | (2,2)
|-------X-------X-------
|
v Y

let arkusz = [
	[Napisz "Komorka (0,0)", Napis "kolumna 1, wiersz 0"],
	[Napisz "Komorka (0,1)", Napis "kolumna 1, wiersz 1"]
]

ponadto: X - kolumny, Y - wiersze -> czyli -> (x,y) = (kolumny,wiersze)


Zalozenia:
pustyArkusz = [[Pusta]]
pustyWiersz = [Pusta]
Arksuz z dwoma pustymi wierszami: [[Pusta],[Pusta]]
minimalizacja - zmniejszanie rozmiaru arkusza przy zachowaniu 100% informacji w arkuszu.
pustyWiersz, ale nie zminimalizowany = [Pusta,Pusta]
pustyWiersz, ale nie zminimalizowany = [Pusta,Pusta,Pusta,Pusta,...]

-}

------------------------------------- operacje zmieniajace arkusz ----------------------

eaWstawWartosc :: Komorka -> Wspol -> Arkusz -> Arkusz
eaWstawWartosc str w ark = eaArkuszMinimalizuj $ eaWstawWartoscBezMinimalizacji str w ark

eaWypiszKomorke :: (Int,Int) -> Arkusz -> String
eaWypiszKomorke (x,y) ark = show $ eaPobierzKomorke (x,y) ark

eaArkuszPobierzIloscKolumn :: [[Komorka]] -> Int
eaArkuszPobierzIloscKolumn ark = maximum $ map length ark

eaArkuszPobierzIloscWierszy :: Arkusz -> Int
eaArkuszPobierzIloscWierszy ark = length ark

eaArkuszWstawKolumne :: Int -> Arkusz -> Arkusz
eaArkuszWstawKolumne x ark = 
  map (uncurry3 (++)) $ zip3
    ( map (take x) ark )
    ( eaResize (0, eaArkuszPobierzIloscWierszy ark) [[Pusta]] )
    ( map (drop x) ark )

eaArkuszWstawWiersz :: Int -> Arkusz -> Arkusz
eaArkuszWstawWiersz x ark = 
    ( take x ark ) ++ 
    ( eaResize (eaArkuszPobierzIloscKolumn ark, 0) [[Pusta]] ) ++
    ( drop x ark )

eaArkuszUsunKolumne :: Int -> Arkusz -> Arkusz
eaArkuszUsunKolumne x ark = 
  if eaArkuszPobierzIloscKolumn ark > 1 then
    map (uncurry (++)) $ zip
      ( map (take (x)) ark )
      ( map (drop (x+1)  ) ark )
  else
    ark

eaArkuszUsunWiersz :: Int -> Arkusz -> Arkusz
eaArkuszUsunWiersz x ark = 
  if eaArkuszPobierzIloscWierszy ark > 1 then
    ( take (x) ark ) ++ 
    ( drop (x+1) ark )
  else
    ark

-------------------- eaPobierz*  zamienia współrzędne i arkusz na coś innego ----------------------

eaPobierzKomorke :: (Int, Int) -> Arkusz -> Komorka
eaPobierzKomorke (x,y) ark = 
  if length wiersz > x then wiersz !! x else Pusta
  where wiersz = if length ark > y then ark !! y else [Pusta]

eaPobierzPrzedzial :: (Int,Int) -> Arkusz -> Przedzial
eaPobierzPrzedzial w ark = eaPobierzPrzedzialKomorki $ eaPobierzKomorke w ark

eaPobierzNapis :: (Int, Int) -> Arkusz -> String
eaPobierzNapis (x,y) ark = eaPobierzNapisKomorki $ eaPobierzKomorke (x,y) ark

eaPobierzLiczbe :: (Int, Int) -> Arkusz -> Float
eaPobierzLiczbe (x,y) ark = eaPobierzLiczbeKomorki $ eaPobierzKomorke (x,y) ark

eaPobierzDoZbiorWspol :: (Int,Int) -> Arkusz -> [(Int,Int)]
eaPobierzDoZbiorWspol w ark = eaPrzedzialDoZbiorWspol $ eaPobierzPrzedzial w ark

eaPobierzNapisKomorki :: Komorka -> String
eaPobierzNapisKomorki (Napis x) = x
eaPobierzNapisKomorki _ = error "Komorka to nie napis!"

eaPobierzLiczbeKomorki :: Komorka -> Float
eaPobierzLiczbeKomorki (Liczba x) = x
eaPobierzLiczbeKomorki _ = error "Komorka to nie Liczba"

eaPobierzPrzedzialKomorki :: Komorka -> Przedzial
eaPobierzPrzedzialKomorki (Suma x) = x
eaPobierzPrzedzialKomorki (Iloczyn x) = x
eaPobierzPrzedzialKomorki (Srednia x) = x
eaPobierzPrzedzialKomorki _ = Pusty

-------------------------- eaCzyKomorka sprawdza typ Komorki -------------------------

eaCzyKomorkaNapis :: Komorka -> Bool
eaCzyKomorkaNapis (Napis _) = True
eaCzyKomorkaNapis _ = False

eaCzyKomorkaLiczba :: Komorka -> Bool
eaCzyKomorkaLiczba (Liczba _) = True
eaCzyKomorkaLiczba _ = False

eaCzyKomorkaZPrzedzialem :: Komorka -> Bool
eaCzyKomorkaZPrzedzialem (Suma _) = True
eaCzyKomorkaZPrzedzialem (Iloczyn _) = True
eaCzyKomorkaZPrzedzialem (Srednia _) = True
eaCzyKomorkaZPrzedzialem _ = False

eaCzyKomorkaLiczbaLubZPrzedzialem :: Komorka -> Bool
eaCzyKomorkaLiczbaLubZPrzedzialem k = eaCzyKomorkaLiczba k || eaCzyKomorkaZPrzedzialem k

eaCzyKomorkaPusta :: Komorka -> Bool
eaCzyKomorkaPusta (Pusta) = True
eaCzyKomorkaPusta _ = False

------------------------ eaCzy* sprawdza czy wpsół w arkusza sa danego typu -------------------

eaCzyZPrzedzialem :: Wspol -> Arkusz -> Bool
eaCzyZPrzedzialem w ark = eaCzyKomorkaZPrzedzialem $ eaPobierzKomorke w ark

eaCzyNapis :: Wspol -> Arkusz -> Bool
eaCzyNapis w ark = eaCzyKomorkaNapis $ eaPobierzKomorke w ark

eaCzyLiczba :: Wspol -> Arkusz -> Bool
eaCzyLiczba w ark = eaCzyKomorkaLiczba $ eaPobierzKomorke w ark

eaCzyLiczbaLubZPrzedzialem :: Wspol -> Arkusz -> Bool
eaCzyLiczbaLubZPrzedzialem w ark = eaCzyLiczba w ark || eaCzyZPrzedzialem w ark

eaCzyPusta :: Wspol -> Arkusz -> Bool
eaCzyPusta w ark = eaCzyKomorkaPusta $ eaPobierzKomorke w ark

---------------------- eaCzyNieskonczonaPetla ---------------------------------

-- Sprawdza, czy dana współczędną na arkusza mozna obliczyć
-- dana współrzędna musi być Pusta (wtedy wartość jest Liczba 0)
-- lub musi być sumą, średnią iloczynem lub liczbą
-- przy obliczaniu nie może występować pętla wieczna
eaCzyObliczalna :: (Int,Int) -> Arkusz -> Bool
eaCzyObliczalna w ark = eaCzyKomorkaObliczalna (eaPobierzKomorke w ark) w ark

---------------------------- obliczanie wartosci komorek + wyswietlanie -------------------------------- 

-- oblicza wartosc danej komorki postaci:
-- Iloczyn, Suma, Liczba, Srednia
-- i zwraca obliczona wartosc w postaci Float
-- Jesli nie da się obliczyc komorki wtedy error
eaObliczWartosc :: (Int, Int) -> Arkusz -> Float
eaObliczWartosc w ark = fromJust $ eaObliczWartoscMaybe w ark

-- Zwaraca wartosc Liczba x dla danej współrzędnej wewnątrzn danego arkusza
-- lub zwraca nothing jesli nie da się obliczyć
eaObliczWartoscMaybe :: (Int, Int) -> Arkusz -> Maybe Float
eaObliczWartoscMaybe w ark = 
  if eaCzyObliczalna w ark then
    eaObliczWartoscKomorkiMaybe (eaPobierzKomorke w ark) ark
  else
    Nothing

--------------------- zapis wczytaj z pliku ------------------

-- zapisuje arkusz do pliku o podanej nazwie
eaArkuszDoPliku :: String -> Arkusz -> IO ()
eaArkuszDoPliku nazwaPliku arkusz = writeFile nazwaPliku $ show arkusz

-- wczytuje arkusza z pliku
-- UWAGA:  aby otrzymac arkusza nalezy zrobić:
-- arkusz <- eaArkuszZPliku
eaArkuszZPliku :: String -> IO (Arkusz)
eaArkuszZPliku nazwaPliku = do
  filetxt <- readFile nazwaPliku
  let arkusz = read filetxt :: Arkusz
  return arkusz

--------------------- dodatkowe funkcje - maybe  ---------------------

-- oblicza wszystkie wartosci w arkusz, to znaczy
-- Suma x , Iloczyn x , Liczba x , Srednia x sa zamieniane na Liczba x
-- Pusta pozostaje Pusta
-- Napisa x pozostaje Napis x
-- Jesli jakas komorke nie da sie obliczyc to ona zostaje Nothing
eaArkuszaObliczWartoscWszystkichKomorekMaybe :: Arkusz -> [[Maybe Komorka]]
eaArkuszaObliczWartoscWszystkichKomorekMaybe ark =
  map (eaArkuszaObliczWartoscWszystkichKomorek_in1 ark) arkw
  where arkw = zip [0..] $ map (zip [0..]) ark

-- oblicza wszystkie wartosci w arkusz, to znaczy
-- Suma x , Iloczyn x , Liczba x , Srednia x sa zamieniane na Liczba x
-- Pusta pozostaje Pusta
-- Napisa x pozostaje Napis x
-- Jesli jakas komorke nie da sie obliczyc wtedy wyrzycany jest error 
eaArkuszaObliczWartoscWszystkichKomorek :: Arkusz -> Arkusz
eaArkuszaObliczWartoscWszystkichKomorek ark = eaMaybeArkuszToArkusz $ eaArkuszaObliczWartoscWszystkichKomorekMaybe ark

-- Wybiera z komorki z całego maybe arkusza, ale wyswietli error jesli cokolwiek bedzie nothing
eaMaybeArkuszToArkusz :: [[Maybe Komorka]] -> Arkusz
eaMaybeArkuszToArkusz mark = map (map fromJust) mark

-- Dla podanego Arkuszu z Maybe Komorkami, zwraca czy caly arkusz jest obliczalny, to znaczy
-- czy kazdą komórkę danego arkusza można obliczyć, czy przypadkiem jakaś nie jest równa Nothing
eaCzyMoznaMaybeArkuszToArkusz :: [[Maybe Komorka]] -> Bool
eaCzyMoznaMaybeArkuszToArkusz mark = foldl (&&) True $ map (foldl (&&) True . map isJust) mark
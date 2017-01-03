module EdycjaArkusza (module EdycjaArkusza, module EdycjaArkuszaDatatypes) where
import EdycjaArkuszaInternal
import EdycjaArkuszaDatatypes
import Prelude
import Data.List
import Debug.Trace

----------------------- convetion -------------------------
{-
  Wszystkie eksportowane funkcje zaczynają się od ea ( EditArkusz / EdytujArkusz )
-}
--------------------------------- funkcje lokalne ---------------------------------------


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

ponadto: X - kolumny, Y - wiersze -> czyli -> (x,y) = (kolumny,wiersze)

-}

------------------------------------- operacje zmieniajace arkusz ----------------------

eaResize :: (Int, Int) -> Arkusz -> Arkusz
eaResize = resizeXY [Pusta]

eaArkuszMinimalizujWiersze :: Arkusz -> Arkusz
eaArkuszMinimalizujWiersze ark = 
  map snd $
  deleteLasts ( (True ==) . fst ) $ 
  zip (map (all (Pusta ==)) ark) ark

eaArkuszMinimalizuj :: Arkusz -> Arkusz
eaArkuszMinimalizuj ark = filter (not . null) $ map ( deleteLasts (Pusta ==) ) ark

eaWstawWartoscBezMinimalizacji :: Komorka -> (Int, Int) -> Arkusz  -> Arkusz 
eaWstawWartoscBezMinimalizacji str (x,y) ark = 
  replaceXY (x,y) str resizedArk
  where resizedArk = eaResize ( (x+1), (y+1) ) ark

{-

import Data.Foldable (toList)
import Data.Sequence (fromList, update)
  poprzednia implementacja:
  toList . update y 
    ( toList . update x str $ fromList $ duzyArkusz !! y ) $ 
  fromList $ duzyArkusz
  where duzyArkusz = eaResize ( (x+1), (y+1) ) ark
-}

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
  map (uncurry (++)) $ zip
    ( map (take (x)) ark )
    ( map (drop (x+1)  ) ark )

eaArkuszUsunWiersz :: Int -> Arkusz -> Arkusz
eaArkuszUsunWiersz x ark = 
  ( take (x) ark ) ++ 
  ( drop (x+1) ark )

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

eaCzyKomorkaZZakresem :: Komorka -> Bool
eaCzyKomorkaZZakresem (Suma _) = True
eaCzyKomorkaZZakresem (Iloczyn _) = True
eaCzyKomorkaZZakresem (Srednia _) = True
eaCzyKomorkaZZakresem _ = False

------------------------ eaCzy* sprawdza czy wpsół w arkusza sa danego typu -------------------

eaCzyZZakresem :: Wspol -> Arkusz -> Bool
eaCzyZZakresem w ark = eaCzyKomorkaZZakresem $ eaPobierzKomorke w ark

eaCzyNapis :: Wspol -> Arkusz -> Bool
eaCzyNapis w ark = eaCzyKomorkaNapis $ eaPobierzKomorke w ark

eaCzyLiczba :: Wspol -> Arkusz -> Bool
eaCzyLiczba w ark = eaCzyKomorkaLiczba $ eaPobierzKomorke w ark

---------------------- eaCzyNieskonczonaPetla ---------------------------------

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
    nastepnePrzebadane = trace ("unnion: " ++ show przebadane ++ show zbiorWPrzedziale) ( Prelude.filter (flip eaCzyZZakresem ark) ( union przebadane zbiorWPrzedziale ) )

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
    nastepnePrzebadane = Prelude.filter (flip eaCzyZZakresem ark) (union przebadane zbiorWPrzedziale )

eaCzyNieskonczonaPetla :: (Int,Int) -> Arkusz -> Bool
eaCzyNieskonczonaPetla w ark = eaCzyNieskonczonaPetla_in wBezLiczb (eaPobierzPrzedzial w ark) ark
  where wBezLiczb = Prelude.filter (flip eaCzyZZakresem ark) [w]

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

eaObliczWartoscKomorki :: Komorka -> Arkusz -> Float
eaObliczWartoscKomorki (Liczba x) _ = x
eaObliczWartoscKomorki (Suma p) ark = foldl (+) 0 $ map (flip eaObliczWartosc ark) (eaPrzedzialDoZbiorWspol p) 
eaObliczWartoscKomorki (Iloczyn p) ark = foldl (*) 1 $ map (flip eaObliczWartosc ark) (eaPrzedzialDoZbiorWspol p)
eaObliczWartoscKomorki (Srednia p) ark = eaObliczWartoscKomorki (Suma p) ark / fromIntegral (eaIloscElementowDoWartosciPrzedzialu p)
eaObliczWartoscKomorki (Napis _) _ = error "eaObliczWartoscKomorki na napisie!"
eaObliczWartoscKomorki (Pusta) _ = error "eaObliczWartoscKomorki na pustym elemencie!"

eaObliczWartoscUnsafe :: (Int, Int) -> Arkusz -> Float
eaObliczWartoscUnsafe w ark = eaObliczWartoscKomorki ( eaPobierzKomorke w ark ) ark

eaObliczWartosc :: (Int, Int) -> Arkusz -> Float
eaObliczWartosc w ark = 
  if not $ eaCzyNieskonczonaPetla w ark then
    eaObliczWartoscUnsafe w ark
  else
    error $ "Nieskonczona petla w " ++ show w ++ " - nie da sie obliczyc wartosci!"

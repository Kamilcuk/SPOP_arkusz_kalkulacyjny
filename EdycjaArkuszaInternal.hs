module EdycjaArkuszaInternal where
import EdycjaArkuszaDatatypes
import Debug.Trace

{- ---------------------- global help funcitons ---------------------------------- -}

-- resize
resize :: [a] -> Int -> [a] -> [a]
resize _ 0 list = list
resize item len [] = item ++ resize item (len-1) []
resize item len (l:list)= [l] ++ resize item (len-1) list

-- resizeXY - resize array in both dimensions
resizeXY :: [a] -> (Int, Int) -> [[a]] -> [[a]]
resizeXY e (x,y) list = map (resize e x) $ resize [e] y list

uncurry3 :: (a -> a -> a) -> (a, a, a) -> a
uncurry3 op (x,y,z) = op x (op y z)

deleteFirsts :: (a -> Bool) -> [a] -> [a]
deleteFirsts op [] = [] 
deleteFirsts op (b:bc) | op b == True = deleteFirsts op bc
                       | otherwise    = b : bc

deleteLasts :: (a -> Bool) -> [a] -> [a]
deleteLasts op xs = reverse $ deleteFirsts op $ reverse xs

deleteFirstsExceptOne :: (a -> Bool) -> [a] -> [a]
deleteFirstsExceptOne op [b,a]  | op b == True  = [a]
                                | otherwise     = [b,a]
deleteFirstsExceptOne op (b:bc) | op b == True  = deleteFirsts op bc
                                | otherwise     = b : bc

deleteLastsExceptOne :: (a -> Bool) -> [a] -> [a]
deleteLastsExceptOne op xs = reverse $ deleteFirstsExceptOne op $ reverse xs


replaceNth :: Show a => Int -> a -> [a] -> [a]
replaceNth 0 newVal (x:xs) = newVal : xs
replaceNth n newVal (x:xs) = x : replaceNth (n-1) newVal xs

replaceXY :: Show a => (Int,Int) -> a -> [[a]] -> [[a]]
replaceXY (0,a) newVal (x:xs) = replaceNth a newVal x : xs
replaceXY (b,a) newVal (x:xs) = x : replaceXY ((a-1), b) newVal xs

------------------------- internal functions --------------------------------------

eaResize :: (Int, Int) -> Arkusz -> Arkusz
eaResize = resizeXY [Pusta]

-- Zmienia kazdy wiersz postaci: [Pusta, Pusta, ...] na [Pusta]
eaArkuszMinimalizujWiersze :: Arkusz -> Arkusz
eaArkuszMinimalizujWiersze ark = filter (not . null) $ map ( deleteLastsExceptOne (Pusta ==) ) ark

-- Usuwa wszystkie pusteWiersz [Pusta] na koncu arkusza
eaArkuszMinimalizujKolumny :: Arkusz -> Arkusz
eaArkuszMinimalizujKolumny ark = deleteLastsExceptOne ([Pusta] ==) ark

eaPobierzKomorkeUnsafe :: (Int, Int) -> Arkusz -> Komorka
eaPobierzKomorkeUnsafe (x,y) ark = ark !! y !! x


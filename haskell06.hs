--Pratica 6 de Haskell
--Nome: Augusto Pagnossim Frigo

ends :: [Int] -> [Int]
ends (x:xs) = x: (last xs):[]

deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = (2 * x) : deduzame (xs)

deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if (x) > 2
  then x : deduzame2 (xs) 
  else deduzame2 (xs)

geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = []
geraTabela n = (n, n^2) : geraTabela (n-1)

contido :: Char -> [Char] -> Bool
contido character [] = False
contido character (x:xs)
  |x == character = True
  |otherwise = contido character xs

translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate (x:xs) = (fst x + 2, snd x + 2) : translate xs 

countLongs :: [String] -> Integer 
countLongs [] = 0
countLongs (x:xs)
  |length x > 5 = 1 + countLongs xs
  |otherwise = 0 + countLongs xs

onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs)
  |length x > 5 = x : onlyLongs xs
  |otherwise = onlyLongs xs
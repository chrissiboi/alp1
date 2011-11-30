--Aufgabe 33: Die Funktion filter p l wählt aus einer Liste l alle Elemente aus, die das Prädikat p erfüllen. Definieren
--sie eine solche Funktion

--33a: rekursiv

filt:: (a -> Bool) -> [a] -> [a]
filt _ [] = []
filt p (x:xs) 
		| p x = x: filt p xs
		| otherwise = filt p xs

--33b: Listendurchlauf 

filt_2:: (a -> Bool) -> [a] -> [a]
filt_2 p list = [x| x <- list, p x]

--33c: Schreiben sie mit Hilfe von filter eine FUnktion, die alle Leerzeichen aus einer Zeichenkette entfernt
filter_3:: [[Char]] -> [[Char]]
filter_3 [] = []
filter_3 (x:xs) = filter (' '/=) x:xs

{--
--Aufgabe 34: Die Funktion foldr und fold sind folgendermaßen definiert:
--foldr:: (a -> b -> b) -> b -> [a] -> b
--foldl:: (b -> a -> b) -> b -> [a] -> b
--foldr f z [] = z
--foldr f z (x:xs) = f x (foldr f z xs)
--foldl f a [] = a
--foldl f a (x:xs) = fodl f (f a x ) xs

!!!lazy:

foldr x+2*y 0 [1,2,3] = x+2*y 1 (foldr (x+2*y) 0 [2,3])
                      = x+2*y 1 ( x+2*y 2 (foldr (x+2*y) 0 [3]))
				  = x+2*y 1 ( x+2*y 2 ( x+2*y 3 (foldr (x+2*y) 0 [])))
				  = x+2*y 1 ( x+2*y 2 ( x+2*y 3 ( 0 ))))
				  = x+2*y 1 ( x+2*y 2 ( 3+2*0))
				  = x+2*y 1 ( x+2*y 2 (	3))
				  = x+2*y 1 ( 2+2*3)
				  = x+2*y 1 ( 8 )
				  = 1+2*8
				  = 17

foldl x+2*y 0 [1,2,3] = foldl x+2*y (x+2*y 0 1) [2,3]
				  = foldl x+2*y (foldl x+2*y (x+2*y 0 1) 2) [3]
				  = foldl x+2*y (foldl x+2*y (foldl x+2*y (x+2*y 0 1) 2) 3) []
				  = foldl x+2*y (foldl x+2*y (foldl x+2*y (x+2*y 0 1) 2) 3) 0
				  = foldl x+2*y (foldl x+2*y (foldl x+2*y (0+2*1)     2) 3) 0
				  = foldl x+2*y (foldl x+2*y (x+2*y  2 2) 3) 0
				  = foldl x+2*y (foldl x+2*y (2+2*2)      3) 0
				  = foldl x+2*y (x+2*y 6 3) 0
				  = foldl x+2*y (6+2*3) 0
				  = x+2*y 12 0
				  = 12+2*0
				  = 12

!!!strict:

foldr x+2*y 0 [1,2,3] = x+2*y 1 (foldr (x+2*y) 0 [2,3])
				  = 1+2*    (foldr (x+2*y) 0 [2,3])
				  = 1+2*    (x+2*y 2 (foldr (x+2*y)) [3])
				  = 1+2*    (2+2* (x+2*y 3 (foldr x+2*y 0 [])))
				  = 1+2*    (2+2* (3+2* ( 0 )))
				  = 1+2*    (2+2* (3+2* (0)
				  = 1+2*    (2+2* (3+0))
				  = 1+2*    (2+2* (3))
				  = 1+2*    (2+6)
				  = 1+2*    (8)
				  = 1+16
				  = 17

foldl x+2*y 0 [1,2,3] = foldl x+2*y (x+2*y 0 1) [2,3]
				  = foldl x+2*y (0+2*1)	   [2,3]
				  = foldl x+2*y (x+2*y 2 2) [3]
				  = foldl x+2*y (2+2*2)	   [3]
				  = foldl x+2*y (6)		   [3]
				  = foldl x+2*y (x+2*y 6 3) []
				  = foldl x+2*y (6+2*3)     []
				  = foldl x+2*y (12)        []
				  = foldl x+2*y 12 0 
				  =       12+2*0
				  = 12
--} 		               

--Aufgabe 35
foldl':: (b -> a -> b) -> b -> [a] ->  b
foldl' f a [] = a
foldl' f a x = f (foldl' f a (init' x)) (last' x)
last':: [a] -> a
last' [x] = x
last' (_:xs) = last' xs
init':: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs 

{--
träge? bin mir nicht sicher, ob das die träge oder strikte ist. 

foldl' (x+2*y) 0 [1,2,3] = (x+2*y) (foldl' (x+2*y) 0 (init' [1,2,3])) (last' [1,2,3])
					= (x+2*y) (foldl' (x+2*y) 0 ([1,2])        ) (3)
					= (x+2*y) (x+2*y (foldl' (x+2*y) 0 (init [1,2])) (last' [1,2])) (3)
					= (x+2*y) (x+2*y (foldl' (x+2*y) 0 (       [1])) (          2)) (3)
					= (x+2*y) (x+2*y (x+2*y (foldl' (x+2*y) 0 (init' [1])) (last' [1]) (2)) (3)
					= (x+2*y) (x+2*y (x+2*y (foldl' (x+2*y) 0 ([])       ) (1)) (2)) (3)
					= (x+2*y) (x+2*y (x+2*y (0) (1)) (2)) (3))
					= (x+2*y) (x+2*y (0+2*1)
					= (x+2*y) (x+2*y (2) (2)) (3)
					= (x+2*y) (2+2*2) (3)
					= (x+2*y) (6) (3)
					= (6+2*3)
					= (12)               

träge/strikte fehlt noch

--}
		
--Aufgabe 37
doppel n a = a+a 					--Streng im Parameter a (n wird nicht ausgewertet)
f1 n a = if n == 0 then a + 1 else a-n  --Streng im Parameter n und a
f2 n a = if n == 0 then a + 1 else n	--Streng im Parameter n ( a bei x /= 0 nicht ausgewertet)

--Aufgabe 38

mult:: (Num a) => a -> a -> a
mult 0 _ = 0
mult x y = x*y

--Aufgabe 39

bild = "\\     +-->\n \\    |\n  +---+\n" --Bild was vertikal und horizontal gespiegelt werden soll

flipV, restV, ersteH:: [Char] -> [Char]
flipV_ausgabe, flipH_ausgabe:: [Char] -> IO()
max_2:: [Char] -> Int
--flipV und restV sorgen für die vertikale Spiegelung des Bildes, außerdem greifen Funktionen von der horizontalen
--Spiegelung auf rest V zu/ flipV_ausgabe dient zur ausgabe des gespiegelten Bildes
flipV_ausgabe img = putStr (flipV img)
flipV [] = []
flipV (x:xs) = foldr (:) (takeWhile (/='\n') (x:xs) ++ "\n") (flipV(restV(x:xs)))    --teilt bild in listen auf
restV (x:xs) = drop (length(takeWhile (/='\n') (x:xs) ++ "\n")) (x:xs)			
--Diese Fkt wird im Prelude aufgerufen und an sie wird das zu spiegelnde Bild übergeben 
flipH_ausgabe img =  putStr (flipH img)										
			where 																
				 flipH, test:: [Char] -> [Char]
				 flipH [] = []
				 flipH (x:xs) = test(ersteH (x:xs)) ++ flipH (restV(x:xs))
				 test (y:ys) 
						| max_2 img == length(y:ys) = (y:ys)
						| otherwise = test(' ':y:ys)

ersteH (x:xs) = reverse(takeWhile (/= '\n') (x:xs)) ++ "\n"
max_2 (x:xs) = maximum (max_ (x:xs) )										
			where 
				max_:: [Char] -> [Int]
				max_ [] = []
				max_ (x:xs) =  [(length(ersteH (x:xs)))] ++ (max_ (restV(x:xs)))

--Aufgabe 40

fib1:: (Ord a, Num a, Num a1) => [a] -> [a1]
fib1 list = [fib n| n <- list]
		  where
			  fib 0 = 0
			  fib 1 = 1		
			 
			  fib n 
				  | n > 0 = fib(n-1) + fib(n-2)
				  | otherwise = fib(n+2) - fib (n+1)


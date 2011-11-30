--Aufgabe 33: Die Funktion filter p l wählt aus einer Liste l alle Elemente aus, die das Prädikat p erfüllen. Definieren
--sie eine solche Funktion

--33a: rekursiv

filt _ [] = []
filt p (x:xs) 
		| p x = x: filt p xs
		| otherwise = filt p xs

--33b: Listendurchlauf 

filt_2 p list = [x| x <- list, p x]

--33c: Schreiben sie mit Hilfe von filter eine FUnktion, die alle Leerzeichen aus einer Zeichenkette entfernt

filter_3 [] = []
filter_3 (x:xs) = filter (' '/=) x:xs

--Aufgabe 34: Die Funktion foldr und fold sind folgendermaßen definiert:
--foldr:: (a -> b -> b) -> b -> [a] -> b
--foldl:: (b -> a -> b) -> b -> [a] -> b
--foldr f z [] = z
--foldr f z (x:xs) = f x (foldr f z xs)
--foldl f a [] = a
--foldl f a (x:xs) = fodl f (f a x ) xs
--
--Vollziehen sie nach, wie die Ausdrücke foldr g 0 [1,2,3] und foldl g 0 [1,2,3] mit der Funktion g x y = x+2*y Schritt 
--für Schritt ausgewertet werden, und zwar bei (a) strenger Auswertung von innen nach außen und (b) träger Auswertung 
--(bedarfsauswertung) wie in Haskell
{--
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
träge??

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



--}
testi = foldl' (\x y -> x+2*y) 0 [1,2,3]
		
--Aufgabe 37
doppel n a = a+a 					--Streng im Parameter a
f1 n a = if n == 0 then a + 1 else a-n  --Streng im Parameter n
f2 n a = if n == 0 then a + 1 else n	--Streng im Parameter n

--Aufgabe 38

mult 0 _ = 0
mult x y = x*y

--Aufgabe 39

bild = "\\     +-->\n \\    |\n  +---+\n"

flipH [] = []
flipH (x:xs) = foldr (:) (takeWhile (/='\n') (x:xs) ++ "\n") (flipH(restH(x:xs)))
restH (x:xs) = drop (length(takeWhile (/='\n') (x:xs) ++ "\n")) (x:xs)

ausgabe img =  putStr (flipV img)
			where 
				 flipV [] = []
				 flipV (x:xs) = test(ersteV (x:xs)) ++ flipV (restH(x:xs))
				 test (y:ys) 
						| max_2 img == length(y:ys) = (y:ys)
						| otherwise = test(' ':y:ys)

ersteV (x:xs) = reverse(takeWhile (/= '\n') (x:xs)) ++ "\n"
max_2 (x:xs) = maximum (max_ (x:xs) )
			where 
				max_ [] = []
				max_ (x:xs) =  [(length(ersteV (x:xs)))] ++ (max_ (restH(x:xs)))

--Aufgabe 40



fib1 list = [fib n| n <- list]
		  where
			  fib 0 = 0
			  fib 1 = 1		
			 
			  fib n 
				  | n > 0 = fib(n-1) + fib(n-2)
				  | otherwise = fib(n+2) - fib (n+1)


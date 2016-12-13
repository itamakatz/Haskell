doubleMe x = x + x

-- doubleUs x y = x*2 + y*2 

-- we could define like this
doubleUs x y = doubleMe x + doubleMe y

-- if statment. else is mandatory
doubleSmallNumber x = if x > 100  
						then x  
						else x*2  

-- if in oneliner
-- ' is a valid character in func names 
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1  

-- func that dosent take parameters. called definition
conanO'Brien = "It's a-me, Conan O'Brien!"					

-- list comprehension
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x, x > 5]

-- a defenition using two variables from lists
aa = [ x*y | x <- [2,5,10], y <- [8,10,11]]  

-- using _ and writing our own length functions
length' xs = sum [1 | _ <- xs]   

-- remove lower cases
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

-- using nested list comprehension!!!
[ [ x | x <- xs, even x ] | xs <- xxs]  

-- find all pitagoras natural legths of triangles
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

-- nice example of a guard
max' :: (Ord a) => a -> a -> a  
max' a b   
	| a > b	 = a  
	| otherwise = b 

-- guard and also defining infix 
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
	| a > b	 = GT  
	| a == b	= EQ  
	| otherwise = LT  

-- legal way of writing "where"
where bmi = weight / height ^ 2  
	  (skinny, normal, fat) = (18.5, 25.0, 30.0) 

-- defining a function inside a "where"
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
	where bmi weight height = weight / height ^ 2   

-- ising let
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
	let sideArea = 2 * pi * r * h  
		topArea = pi * r ^2  
	in  sideArea + 2 * topArea  

-- defining function in a local way
[let square x = x * x in (square 5, square 3, square 2)]  

-- let without the "in" due to list comprehension. IT IS NOT A CONDITION
--      BUT A FUNCTION for the output
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- case in haskell
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result 

-- exelent example for max
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs
-- the last part can also be written like so
maximum' (x:xs) = max x (maximum' xs)  

-- we CAN define infinit non ending recursion!
repeat' :: a -> [a]  
repeat' x = x:repeat' x  

-- nice and simple quicksort
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

-- THIS TWO EXAMPLES ARE IDENTICAL!!! AMAZING!!!!
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x  

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100  

-- getting a function as a parameter
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

-- implementing "flip". cool shit
flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x  


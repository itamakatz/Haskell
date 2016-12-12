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

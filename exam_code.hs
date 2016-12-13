

-- ========= 1 ========= -- 
-- Comments: in my sol, with input of [1..100] it would throgh an exception

-- My sol:
-- myLast :: [a] -> a
-- myLast (_:[xx]) = xx

-- myLast :: [a] -> a
-- myLast (xs ++ [x]) = x

-- school sol:
-- myLast :: [a] -> a
-- myLast [] = error "No end for empty lists!"
-- myLast [x] = x
-- myLast (_:xs) = myLast xs

--

-- ========= 2 ========= -- 
-- Comments: what does . mean ????

-- My sol:
-- myButLast :: [a] -> a
-- myButLast [] = error "sup"
-- myButLast (x:xs) = if y == 2 then x else myButLast xs 
-- 	where
		-- y = length (x:xs)

-- school sol:
-- myButLast :: [a] -> a
-- myButLast = last . init

--

-- ========= 3 ========= -- 
-- Comments:

-- My sol:

-- school sol:


-- ========= 3 ========= -- 
-- Comments:

-- My sol:

-- school sol:



-- tell :: (Int a) => [a] -> a
-- tell [] = 0
-- tell (x:[]) = x
-- tell (x:y:[]) = x
-- tell (x:y:_) = x

-- factors :: Int → [Int] 
-- factors 


-- solution to ch5 slide 20 :) beautiful!!!!!
-- perfects :: Int -> [Int]
-- perfects x = [ a | a <- [1..(x - 1)], perfect a == True]
-- 	where perfect n = if sum ([x | x <- [1..(n - 1)], n `mod` x == 0]) == n then True else False

la :: [a] -> Int -> a
la [] n = error "blabla"
la (x:xs) n
	| n < 0 = error "blabla"
	| n == 0 = x
	| otherwise = la xs (n - 1)


-- ========= 1 ========= -- 
-- Comments: in my sol, with input of [1..100] it would throgh an exception

-- My sol:
-- myLast :: [a] -> a
-- myLast (_:[xx]) = xx

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

tell :: (Int a) => [a] -> Int
tell [] = 0
tell (x:[]) = 1
tell (x:y:[]) = 2
tell (x:y:_) = 3


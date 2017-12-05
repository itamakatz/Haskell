import System.Random (randomRIO)
import System.IO (hFlush, stdout, getLine, putStrLn)

prompt :: String -> IO String
prompt s = do
    putStr s
    hFlush stdout
    getLine

data Cell = R1_C1 | R1_C2
data Tile = Empty| O | X

instance Eq Tile where 
    Empty == Empty = True 
    _ == _ = False

instance Show Cell where
    show R1_C1 = "R1_C1"
    show R1_C2 = "2"

num_to_cell :: (Integral a) => a -> Cell
num_to_cell 1 = R1_C1
num_to_cell 2 = R1_C2

returnTile :: Cell -> Tile
returnTile _ = O

lala :: Cell -> Cell -> Cell
lala b pos
	| t == Empty = pos
	| otherwise = randomEmptyTile b
	where 
    	t = returnTile pos

getRand :: Int -> Int -> IO Int
getRand a b = 
	randomRIO(a,b)

randomEmptyTile :: Cell -> Cell
randomEmptyTile b =
    	posIO <- randomRIO (1,9)
    	let	pos = num_to_cell (read posIO)
    	in lala b pos


-- randomEmptyTile :: Cell -> Cell
-- randomEmptyTile b = do
--     posIO <- randomRIO (1,9) 
--     let pos = num_to_cell (read posIO)
--     let t = returnTile pos
--     if t == Empty
--         then do
--             return pos
--         else
--             randomEmptyTile b

main = do
    playermove <- prompt "Choose a number from 1 to 9: "
    let b = num_to_cell (read playermove)
    print b
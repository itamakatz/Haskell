-- Tic Tac Toe Haskell program project for
-- William Wang (17237158)
-- Yue (Johnson) Sun  (39828116)
-- Eric Chi Xiang Chou (26505140)

import System.Random (randomRIO)
import System.IO (hFlush, stdout, getLine)

-- Tile can be of 3 states: Empty, X, or O
data Tile = Empty| O | X
data Player = P1 | P2 
data Cell = R1_C1 | R1_C2 | R1_C3 | R2_C1 | R2_C2 | R2_C3 | R3_C1 | R3_C2 | R3_C3

instance Show Tile where
    show Empty = " "
    show O     = "O"
    show X     = "X"

instance Eq Tile where 
    Empty == Empty = True 
    _ == _ = False

-- Declares Board as a series of 9 Tiles
type Board = (Tile, Tile, Tile, Tile, Tile, Tile, Tile, Tile, Tile)

num_to_cell :: (Integral a) => a -> Cell
num_to_cell 1 = R1_C1
num_to_cell 2 = R1_C2
num_to_cell 3 = R1_C3
num_to_cell 4 = R2_C1
num_to_cell 5 = R2_C2
num_to_cell 6 = R2_C3
num_to_cell 7 = R3_C1
num_to_cell 8 = R3_C2
num_to_cell 9 = R3_C3

-- emptyBoard is a Board of empty Tiles
emptyBoard :: Board
emptyBoard = (Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty)

-- Place marker in empty Tile
makeMove :: Board -> Tile -> Cell -> Maybe Board
makeMove (a,b,c,d,e,f,Empty,h,i) t R1_C1 = Just (a,b,c,d,e,f,t,h,i)
makeMove (a,b,c,d,e,f,g,Empty,i) t R1_C2 = Just (a,b,c,d,e,f,g,t,i)
makeMove (a,b,c,d,e,f,g,h,Empty) t R1_C3 = Just (a,b,c,d,e,f,g,h,t)
makeMove (a,b,c,Empty,e,f,g,h,i) t R2_C1 = Just (a,b,c,t,e,f,g,h,i)
makeMove (a,b,c,d,Empty,f,g,h,i) t R2_C2 = Just (a,b,c,d,t,f,g,h,i)
makeMove (a,b,c,d,e,Empty,g,h,i) t R2_C3 = Just (a,b,c,d,e,t,g,h,i)
makeMove (Empty,b,c,d,e,f,g,h,i) t R3_C1 = Just (t,b,c,d,e,f,g,h,i)
makeMove (a,Empty,c,d,e,f,g,h,i) t R3_C2 = Just (a,t,c,d,e,f,g,h,i)
makeMove (a,b,Empty,d,e,f,g,h,i) t R3_C3 = Just (a,b,t,d,e,f,g,h,i)
makeMove _ _ _ = Nothing

-- All winning conditions
determineWin :: Board -> Maybe Player
determineWin (O,O,O,_,_,_,_,_,_) = Just P2
determineWin (_,_,_,O,O,O,_,_,_) = Just P2
determineWin (_,_,_,_,_,_,O,O,O) = Just P2
determineWin (O,_,_,O,_,_,O,_,_) = Just P2
determineWin (_,O,_,_,O,_,_,O,_) = Just P2
determineWin (_,_,O,_,_,O,_,_,O) = Just P2
determineWin (O,_,_,_,O,_,_,_,O) = Just P2
determineWin (_,_,O,_,O,_,O,_,_) = Just P2
determineWin (X,X,X,_,_,_,_,_,_) = Just P1
determineWin (_,_,_,X,X,X,_,_,_) = Just P1
determineWin (_,_,_,_,_,_,X,X,X) = Just P1
determineWin (X,_,_,X,_,_,X,_,_) = Just P1
determineWin (_,X,_,_,X,_,_,X,_) = Just P1
determineWin (_,_,X,_,_,X,_,_,X) = Just P1
determineWin (X,_,_,_,X,_,_,_,X) = Just P1
determineWin (_,_,X,_,X,_,X,_,_) = Just P1

determineWin _ = Nothing

-- Checks for no winner/tie
determineTie :: Board -> Bool
determineTie (Empty,_,_,_,_,_,_,_,_) = False
determineTie (_,Empty,_,_,_,_,_,_,_) = False
determineTie (_,_,Empty,_,_,_,_,_,_) = False
determineTie (_,_,_,Empty,_,_,_,_,_) = False
determineTie (_,_,_,_,Empty,_,_,_,_) = False
determineTie (_,_,_,_,_,Empty,_,_,_) = False
determineTie (_,_,_,_,_,_,Empty,_,_) = False
determineTie (_,_,_,_,_,_,_,Empty,_) = False
determineTie (_,_,_,_,_,_,_,_,Empty) = False
determineTie _= True

-- returns Tile at specified Int
returnTile :: Board -> Cell -> Tile
returnTile (a,b,c,d,e,f,g,h,i) R1_C1 = g
returnTile (a,b,c,d,e,f,g,h,i) R1_C2 = h
returnTile (a,b,c,d,e,f,g,h,i) R1_C3 = i
returnTile (a,b,c,d,e,f,g,h,i) R2_C1 = d
returnTile (a,b,c,d,e,f,g,h,i) R2_C2 = e
returnTile (a,b,c,d,e,f,g,h,i) R2_C3 = f
returnTile (a,b,c,d,e,f,g,h,i) R3_C1 = a
returnTile (a,b,c,d,e,f,g,h,i) R3_C2 = b
returnTile (a,b,c,d,e,f,g,h,i) R3_C3 = c

-- Computer attempts to make a winning move
chooseCompMove :: Board -> Maybe Cell
chooseCompMove (Empty,O,O,_,_,_,_,_,_) = Just R3_C1
chooseCompMove (O,Empty,O,_,_,_,_,_,_) = Just R3_C2
chooseCompMove (O,O,Empty,_,_,_,_,_,_) = Just R3_C3
chooseCompMove (_,_,_,Empty,O,O,_,_,_) = Just R2_C1
chooseCompMove (_,_,_,O,Empty,O,_,_,_) = Just R2_C2
chooseCompMove (_,_,_,O,O,Empty,_,_,_) = Just R2_C3
chooseCompMove (_,_,_,_,_,_,Empty,O,O) = Just R1_C1
chooseCompMove (_,_,_,_,_,_,O,Empty,O) = Just R1_C2
chooseCompMove (_,_,_,_,_,_,O,O,Empty) = Just R1_C3
chooseCompMove (Empty,_,_,O,_,_,O,_,_) = Just R3_C1
chooseCompMove (O,_,_,Empty,_,_,O,_,_) = Just R2_C1
chooseCompMove (O,_,_,O,_,_,Empty,_,_) = Just R1_C1
chooseCompMove (_,Empty,_,_,O,_,_,O,_) = Just R3_C2
chooseCompMove (_,O,_,_,Empty,_,_,O,_) = Just R2_C2
chooseCompMove (_,O,_,_,O,_,_,Empty,_) = Just R1_C2
chooseCompMove (_,_,Empty,_,_,O,_,_,O) = Just R3_C3
chooseCompMove (_,_,O,_,_,Empty,_,_,O) = Just R2_C3
chooseCompMove (_,_,O,_,_,O,_,_,Empty) = Just R1_C3
chooseCompMove (Empty,_,_,_,O,_,_,_,O) = Just R3_C1
chooseCompMove (O,_,_,_,Empty,_,_,_,O) = Just R2_C2
chooseCompMove (O,_,_,_,O,_,_,_,Empty) = Just R1_C3
chooseCompMove (_,_,Empty,_,O,_,O,_,_) = Just R3_C3
chooseCompMove (_,_,O,_,Empty,_,O,_,_) = Just R2_C2
chooseCompMove (_,_,O,_,O,_,Empty,_,_) = Just R1_C1

-- Computer attempts to block off the player's winning move
chooseCompMove (Empty,X,X,_,_,_,_,_,_) = Just R3_C1
chooseCompMove (X,Empty,X,_,_,_,_,_,_) = Just R3_C2
chooseCompMove (X,X,Empty,_,_,_,_,_,_) = Just R3_C3
chooseCompMove (_,_,_,Empty,X,X,_,_,_) = Just R2_C1
chooseCompMove (_,_,_,X,Empty,X,_,_,_) = Just R2_C2
chooseCompMove (_,_,_,X,X,Empty,_,_,_) = Just R2_C3
chooseCompMove (_,_,_,_,_,_,Empty,X,X) = Just R1_C1
chooseCompMove (_,_,_,_,_,_,X,Empty,X) = Just R1_C2
chooseCompMove (_,_,_,_,_,_,X,X,Empty) = Just R1_C3
chooseCompMove (Empty,_,_,X,_,_,X,_,_) = Just R3_C1
chooseCompMove (X,_,_,Empty,_,_,X,_,_) = Just R2_C1
chooseCompMove (X,_,_,X,_,_,Empty,_,_) = Just R1_C1
chooseCompMove (_,Empty,_,_,X,_,_,X,_) = Just R3_C2
chooseCompMove (_,X,_,_,Empty,_,_,X,_) = Just R2_C2
chooseCompMove (_,X,_,_,X,_,_,Empty,_) = Just R1_C2
chooseCompMove (_,_,Empty,_,_,X,_,_,X) = Just R3_C3
chooseCompMove (_,_,X,_,_,Empty,_,_,X) = Just R2_C3
chooseCompMove (_,_,X,_,_,X,_,_,Empty) = Just R1_C3
chooseCompMove (Empty,_,_,_,X,_,_,_,X) = Just R3_C1
chooseCompMove (X,_,_,_,Empty,_,_,_,X) = Just R2_C2
chooseCompMove (X,_,_,_,X,_,_,_,Empty) = Just R1_C3
chooseCompMove (_,_,Empty,_,X,_,X,_,_) = Just R3_C3
chooseCompMove (_,_,X,_,Empty,_,X,_,_) = Just R2_C2
chooseCompMove (_,_,X,_,X,_,Empty,_,_) = Just R1_C1

chooseCompMove (_,_,_,_,_,_,_,_,_) = Nothing

-- Determines the most optimal move for the computer
-- attempts to win/ block off player's winning move 
-- or else it randomly places a mark in an empty tile
computerMove :: Board -> IO (Board)
computerMove b = do
    case chooseCompMove b of
        Just pos -> do
            let (Just b') = makeMove b O pos
            return b'
        Nothing -> do
            let pos1 = randomEmptyTile b
            let (Just b') = makeMove b O pos1
            return b'

-- helper function for computerMove
randomEmptyTile :: Board -> Cell
randomEmptyTile b = do
    posIO <- randomRIO (1,9) 
    let pos = num_to_cell (read posIO)
    -- let t = returnTile b (num_to_cell (read pos))
    let t = returnTile b pos
    if t == Empty
        then do
            return pos
        else
            randomEmptyTile b

-- Shows the player the current board state
showBoard :: Board -> IO ()
showBoard (a,b,c,d,e,f,g,h,i) = do
    putStrLn ("|" ++ show a ++ "|" ++ show b ++ "|" ++ show c ++ "|")
    putStrLn ("|" ++ show d ++ "|" ++ show e ++ "|" ++ show f ++ "|")
    putStrLn ("|" ++ show g ++ "|" ++ show h ++ "|" ++ show i ++ "|")

-- Shows the player which squares correspond with which numbers
showTiles :: IO ()
showTiles  = do
    putStrLn "|7|8|9|"
    putStrLn "|4|5|6|"
    putStrLn "|1|2|3|"
    putStrLn ""


prompt :: String -> IO String
prompt s = do
    putStr s
    hFlush stdout
    getLine




startGame :: Board -> IO ()
startGame a1 = do
    playermove <- prompt "Choose a number from 1 to 9: "
    let get_cell = num_to_cell (read playermove)
    let newboardstate = makeMove a1 X get_cell
    case newboardstate of
        Nothing -> do
                    putStrLn "Not a valid move."
                    startGame a1
        Just b2 ->
                    case determineWin b2 of
                        Just P1 -> do
                                    putStrLn "Player win"
                                    showBoard b2
                        _            -> if determineTie b2 
                                            then do
                                                putStrLn "No winner"
                                                showBoard b2
                                            else do
                                                c3 <- computerMove b2
                                                showBoard c3
                                                case determineWin c3 of
                                                    Just P2 -> putStrLn "Computer win"
                                                    _            -> if determineTie c3 
                                                                        then do
                                                                            putStrLn "No winner"
                                                                            showBoard c3
                                                                        else
                                                                            startGame c3

main = do
    putStrLn "Welcome to Tic Tac Toe Haskell!"
    putStrLn "Please indicate where you would like to place your next move"
    putStrLn "This is the game board"
    showTiles
    showBoard emptyBoard
    startGame emptyBoard
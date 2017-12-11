import System.Random (randomRIO)
import System.IO 
import Data.Maybe

data Symbol = X | O | Empty

instance Show Symbol where
    show X = "X"
    show O = "O"
    show Empty = " "

(&&&&) :: Symbol -> Symbol -> Bool
(&&&&) Empty _ = False
(&&&&) _ Empty = False
(&&&&) _ _ = True

(&&&) :: Symbol -> Bool -> Bool
(&&&) Empty _ = False
(&&&) _ False = False
(&&&) _ _ = True

type Board = (Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol)

-- data Player = P1 | P2 

changePlayer :: Symbol -> Symbol
changePlayer X = O
changePlayer O = X

-- Building an empty board
emptyBoard :: Board
emptyBoard = (Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty)

getAtIndex :: Board -> Int -> Symbol
getAtIndex (x,_,_,_,_,_,_,_,_) 7 = x 
getAtIndex (_,x,_,_,_,_,_,_,_) 8 = x 
getAtIndex (_,_,x,_,_,_,_,_,_) 9 = x 
getAtIndex (_,_,_,x,_,_,_,_,_) 4 = x 
getAtIndex (_,_,_,_,x,_,_,_,_) 5 = x 
getAtIndex (_,_,_,_,_,x,_,_,_) 6 = x 
getAtIndex (_,_,_,_,_,_,x,_,_) 1 = x 
getAtIndex (_,_,_,_,_,_,_,x,_) 2 = x 
getAtIndex (_,_,_,_,_,_,_,_,x) 3 = x 

setAtIndex :: Board -> Symbol -> Int -> Board
setAtIndex (_,b,c,d,e,f,g,h,i) symbol 7 = (symbol,b,c,d,e,f,g,h,i)
setAtIndex (a,_,c,d,e,f,g,h,i) symbol 8 = (a,symbol,c,d,e,f,g,h,i)
setAtIndex (a,b,_,d,e,f,g,h,i) symbol 9 = (a,b,symbol,d,e,f,g,h,i)
setAtIndex (a,b,c,_,e,f,g,h,i) symbol 4 = (a,b,c,symbol,e,f,g,h,i)
setAtIndex (a,b,c,d,_,f,g,h,i) symbol 5 = (a,b,c,d,symbol,f,g,h,i)
setAtIndex (a,b,c,d,e,_,g,h,i) symbol 6 = (a,b,c,d,e,symbol,g,h,i)
setAtIndex (a,b,c,d,e,f,_,h,i) symbol 1 = (a,b,c,d,e,f,symbol,h,i)
setAtIndex (a,b,c,d,e,f,g,_,i) symbol 2 = (a,b,c,d,e,f,g,symbol,i)
setAtIndex (a,b,c,d,e,f,g,h,_) symbol 3 = (a,b,c,d,e,f,g,h,symbol)
 
-- Place marker in empty Symbol
makeMove :: Board -> Symbol -> Int -> Maybe Board
makeMove board symbol index =
    case getAtIndex board index of 
        Empty -> Just $ setAtIndex board symbol index
        _ -> Nothing

-- All winning conditions
determineWin :: Board -> Maybe Symbol
determineWin (O,O,O,_,_,_,_,_,_) = Just O
determineWin (_,_,_,O,O,O,_,_,_) = Just O
determineWin (_,_,_,_,_,_,O,O,O) = Just O
determineWin (O,_,_,O,_,_,O,_,_) = Just O
determineWin (_,O,_,_,O,_,_,O,_) = Just O
determineWin (_,_,O,_,_,O,_,_,O) = Just O
determineWin (O,_,_,_,O,_,_,_,O) = Just O
determineWin (_,_,O,_,O,_,O,_,_) = Just O
determineWin (X,X,X,_,_,_,_,_,_) = Just X
determineWin (_,_,_,X,X,X,_,_,_) = Just X
determineWin (_,_,_,_,_,_,X,X,X) = Just X
determineWin (X,_,_,X,_,_,X,_,_) = Just X
determineWin (_,X,_,_,X,_,_,X,_) = Just X
determineWin (_,_,X,_,_,X,_,_,X) = Just X
determineWin (X,_,_,_,X,_,_,_,X) = Just X
determineWin (_,_,X,_,X,_,X,_,_) = Just X

determineWin _ = Nothing

-- Checks for no winner/tie
determineTie :: Board -> Bool
determineTie (a,b,c,d,e,f,g,h,i) = (&&&) a $ (&&&) b  $ (&&&) c $ (&&&) d $ (&&&) e $ (&&&) f $ (&&&) g $ (&&&&) h i

-- Computer attempts to make a winning move
chooseCompMove :: Board -> Maybe Int
chooseCompMove (Empty,O,O,_,_,_,_,_,_) = Just 7
chooseCompMove (O,Empty,O,_,_,_,_,_,_) = Just 8
chooseCompMove (O,O,Empty,_,_,_,_,_,_) = Just 9
chooseCompMove (_,_,_,Empty,O,O,_,_,_) = Just 4
chooseCompMove (_,_,_,O,Empty,O,_,_,_) = Just 5
chooseCompMove (_,_,_,O,O,Empty,_,_,_) = Just 6
chooseCompMove (_,_,_,_,_,_,Empty,O,O) = Just 1
chooseCompMove (_,_,_,_,_,_,O,Empty,O) = Just 2
chooseCompMove (_,_,_,_,_,_,O,O,Empty) = Just 3
chooseCompMove (Empty,_,_,O,_,_,O,_,_) = Just 7
chooseCompMove (O,_,_,Empty,_,_,O,_,_) = Just 4
chooseCompMove (O,_,_,O,_,_,Empty,_,_) = Just 1
chooseCompMove (_,Empty,_,_,O,_,_,O,_) = Just 8
chooseCompMove (_,O,_,_,Empty,_,_,O,_) = Just 5
chooseCompMove (_,O,_,_,O,_,_,Empty,_) = Just 2
chooseCompMove (_,_,Empty,_,_,O,_,_,O) = Just 9
chooseCompMove (_,_,O,_,_,Empty,_,_,O) = Just 6
chooseCompMove (_,_,O,_,_,O,_,_,Empty) = Just 3
chooseCompMove (Empty,_,_,_,O,_,_,_,O) = Just 7
chooseCompMove (O,_,_,_,Empty,_,_,_,O) = Just 5
chooseCompMove (O,_,_,_,O,_,_,_,Empty) = Just 3
chooseCompMove (_,_,Empty,_,O,_,O,_,_) = Just 9
chooseCompMove (_,_,O,_,Empty,_,O,_,_) = Just 5
chooseCompMove (_,_,O,_,O,_,Empty,_,_) = Just 1

-- Computer attempts to block off the player's winning move
chooseCompMove (Empty,X,X,_,_,_,_,_,_) = Just 7
chooseCompMove (X,Empty,X,_,_,_,_,_,_) = Just 8
chooseCompMove (X,X,Empty,_,_,_,_,_,_) = Just 9
chooseCompMove (_,_,_,Empty,X,X,_,_,_) = Just 4
chooseCompMove (_,_,_,X,Empty,X,_,_,_) = Just 5
chooseCompMove (_,_,_,X,X,Empty,_,_,_) = Just 6
chooseCompMove (_,_,_,_,_,_,Empty,X,X) = Just 1
chooseCompMove (_,_,_,_,_,_,X,Empty,X) = Just 2
chooseCompMove (_,_,_,_,_,_,X,X,Empty) = Just 3
chooseCompMove (Empty,_,_,X,_,_,X,_,_) = Just 7
chooseCompMove (X,_,_,Empty,_,_,X,_,_) = Just 4
chooseCompMove (X,_,_,X,_,_,Empty,_,_) = Just 1
chooseCompMove (_,Empty,_,_,X,_,_,X,_) = Just 8
chooseCompMove (_,X,_,_,Empty,_,_,X,_) = Just 5
chooseCompMove (_,X,_,_,X,_,_,Empty,_) = Just 2
chooseCompMove (_,_,Empty,_,_,X,_,_,X) = Just 9
chooseCompMove (_,_,X,_,_,Empty,_,_,X) = Just 6
chooseCompMove (_,_,X,_,_,X,_,_,Empty) = Just 3
chooseCompMove (Empty,_,_,_,X,_,_,_,X) = Just 7
chooseCompMove (X,_,_,_,Empty,_,_,_,X) = Just 5
chooseCompMove (X,_,_,_,X,_,_,_,Empty) = Just 3
chooseCompMove (_,_,Empty,_,X,_,X,_,_) = Just 9
chooseCompMove (_,_,X,_,Empty,_,X,_,_) = Just 5
chooseCompMove (_,_,X,_,X,_,Empty,_,_) = Just 1

chooseCompMove (_,_,_,_,_,_,_,_,_) = Nothing

-- Determines the most optimal move for the computer
-- attempts to win/ block off player's winning move 
-- or else it randomly places a mark in an empty Symbol
computerMove :: Board -> IO (Board)
computerMove b = 
    case pos of
        Nothing -> do
            pos1 <- randomEmptySymbol b
            let (Just b') = makeMove b O pos1
            return b'
        _       -> do
            let (Just b') = makeMove b O $ head $ catMaybes [pos]
            return b'
    where pos = chooseCompMove b

-- helper function for computerMove
randomEmptySymbol :: Board -> IO Int
randomEmptySymbol b = do
    pos <- randomRIO(1,9) 
    case getAtIndex b pos of
        Empty -> return pos
        _     -> randomEmptySymbol b

-- Shows the player the current board state
showBoard :: Board -> IO ()
showBoard (a,b,c,d,e,f,g,h,i) = do
    putStrLn ("|" ++ show a ++ "|" ++ show b ++ "|" ++ show c ++ "|")
    putStrLn ("|" ++ show d ++ "|" ++ show e ++ "|" ++ show f ++ "|")
    putStrLn ("|" ++ show g ++ "|" ++ show h ++ "|" ++ show i ++ "|")

prompt :: String -> IO String
prompt s = do
    putStrLn s
    hFlush stdout
    getLine

startGameForOne :: Board -> IO ()
startGameForOne a1 = do
    playermove <- prompt "Choose a number from 1 to 9: "
    let newboardstate = makeMove a1 X (read playermove)
    case newboardstate of
        Nothing -> do
                    putStrLn "Not a valid move."
                    startGameForOne a1
        Just b2 ->
                    case determineWin b2 of
                        Just X -> do
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
                                                    Just O -> putStrLn "Computer win"
                                                    _            -> if determineTie c3 
                                                                        then do
                                                                            putStrLn "No winner"
                                                                            showBoard c3
                                                                        else
                                                                            startGameForOne c3

startGameForTwo :: Board -> Symbol -> IO ()
startGameForTwo a1 symbol = do
    playermove <- prompt " Player 1 - Choose a number from 1 to 9: "
    let newboardstate = makeMove a1 symbol (read playermove)
    case newboardstate of
        Nothing -> do
            putStrLn "Not a valid move."
            startGameForTwo a1 symbol
        Just b2 -> do
            showBoard b2
            case determineWin b2 of
                Just X -> do
                    putStrLn "Player 1 wins"    
                Just O -> do
                    putStrLn "Player 2 wins"
                _      -> if determineTie b2 
                            then do
                                putStrLn "No winner"
                            else do
                                startGameForTwo b2 $ changePlayer symbol



welcome :: IO()                                   
welcome = do
    putStrLn "|7|8|9| \n|4|5|6|\n|1|2|3|\n"
    showBoard emptyBoard

main = do
    putStrLn "Tic Tac Toe"
    playermove <- prompt "Choose 1 for two players, or 2 for player Vs computer"
    case (read playermove) of
        1 -> do
            putStrLn "You have chosen two players"
            welcome
            startGameForTwo emptyBoard X
        2 -> do
            putStrLn "You have chosen to play against the computer"
            welcome
            startGameForOne emptyBoard


    
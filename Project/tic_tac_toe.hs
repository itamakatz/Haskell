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


changePlayer :: Symbol -> Symbol
changePlayer X = O
changePlayer O = X


emptyBoard :: Board
emptyBoard = (Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty)


-- get symbol of the board for a given index
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


-- set symbol in the board for a given index
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
 

-- update board due to a player move
makeMove :: Board -> Symbol -> Int -> Maybe Board
makeMove board symbol index =
    case getAtIndex board index of 
        Empty -> Just $ setAtIndex board symbol index
        _ -> Nothing


-- Check when a player wins
checkWin :: Board -> Maybe Symbol
checkWin (O,O,O,_,_,_,_,_,_) = Just O
checkWin (_,_,_,O,O,O,_,_,_) = Just O
checkWin (_,_,_,_,_,_,O,O,O) = Just O
checkWin (O,_,_,O,_,_,O,_,_) = Just O
checkWin (_,O,_,_,O,_,_,O,_) = Just O
checkWin (_,_,O,_,_,O,_,_,O) = Just O
checkWin (O,_,_,_,O,_,_,_,O) = Just O
checkWin (_,_,O,_,O,_,O,_,_) = Just O
checkWin (X,X,X,_,_,_,_,_,_) = Just X
checkWin (_,_,_,X,X,X,_,_,_) = Just X
checkWin (_,_,_,_,_,_,X,X,X) = Just X
checkWin (X,_,_,X,_,_,X,_,_) = Just X
checkWin (_,X,_,_,X,_,_,X,_) = Just X
checkWin (_,_,X,_,_,X,_,_,X) = Just X
checkWin (X,_,_,_,X,_,_,_,X) = Just X
checkWin (_,_,X,_,X,_,X,_,_) = Just X

checkWin _ = Nothing

-- Checks for a tie
isTie :: Board -> Bool
isTie (a,b,c,d,e,f,g,h,i) = (&&&) a $ (&&&) b  $ (&&&) c $ (&&&) d $ (&&&) e $ (&&&) f $ (&&&) g $ (&&&&) h i


-- Computer attempts to make a winning move
deterministicMove :: Board -> Maybe Int
deterministicMove (Empty,O,O,_,_,_,_,_,_) = Just 7
deterministicMove (O,Empty,O,_,_,_,_,_,_) = Just 8
deterministicMove (O,O,Empty,_,_,_,_,_,_) = Just 9
deterministicMove (_,_,_,Empty,O,O,_,_,_) = Just 4
deterministicMove (_,_,_,O,Empty,O,_,_,_) = Just 5
deterministicMove (_,_,_,O,O,Empty,_,_,_) = Just 6
deterministicMove (_,_,_,_,_,_,Empty,O,O) = Just 1
deterministicMove (_,_,_,_,_,_,O,Empty,O) = Just 2
deterministicMove (_,_,_,_,_,_,O,O,Empty) = Just 3
deterministicMove (Empty,_,_,O,_,_,O,_,_) = Just 7
deterministicMove (O,_,_,Empty,_,_,O,_,_) = Just 4
deterministicMove (O,_,_,O,_,_,Empty,_,_) = Just 1
deterministicMove (_,Empty,_,_,O,_,_,O,_) = Just 8
deterministicMove (_,O,_,_,Empty,_,_,O,_) = Just 5
deterministicMove (_,O,_,_,O,_,_,Empty,_) = Just 2
deterministicMove (_,_,Empty,_,_,O,_,_,O) = Just 9
deterministicMove (_,_,O,_,_,Empty,_,_,O) = Just 6
deterministicMove (_,_,O,_,_,O,_,_,Empty) = Just 3
deterministicMove (Empty,_,_,_,O,_,_,_,O) = Just 7
deterministicMove (O,_,_,_,Empty,_,_,_,O) = Just 5
deterministicMove (O,_,_,_,O,_,_,_,Empty) = Just 3
deterministicMove (_,_,Empty,_,O,_,O,_,_) = Just 9
deterministicMove (_,_,O,_,Empty,_,O,_,_) = Just 5
deterministicMove (_,_,O,_,O,_,Empty,_,_) = Just 1

-- Computer attempts to block off the player's winning move
deterministicMove (Empty,X,X,_,_,_,_,_,_) = Just 7
deterministicMove (X,Empty,X,_,_,_,_,_,_) = Just 8
deterministicMove (X,X,Empty,_,_,_,_,_,_) = Just 9
deterministicMove (_,_,_,Empty,X,X,_,_,_) = Just 4
deterministicMove (_,_,_,X,Empty,X,_,_,_) = Just 5
deterministicMove (_,_,_,X,X,Empty,_,_,_) = Just 6
deterministicMove (_,_,_,_,_,_,Empty,X,X) = Just 1
deterministicMove (_,_,_,_,_,_,X,Empty,X) = Just 2
deterministicMove (_,_,_,_,_,_,X,X,Empty) = Just 3
deterministicMove (Empty,_,_,X,_,_,X,_,_) = Just 7
deterministicMove (X,_,_,Empty,_,_,X,_,_) = Just 4
deterministicMove (X,_,_,X,_,_,Empty,_,_) = Just 1
deterministicMove (_,Empty,_,_,X,_,_,X,_) = Just 8
deterministicMove (_,X,_,_,Empty,_,_,X,_) = Just 5
deterministicMove (_,X,_,_,X,_,_,Empty,_) = Just 2
deterministicMove (_,_,Empty,_,_,X,_,_,X) = Just 9
deterministicMove (_,_,X,_,_,Empty,_,_,X) = Just 6
deterministicMove (_,_,X,_,_,X,_,_,Empty) = Just 3
deterministicMove (Empty,_,_,_,X,_,_,_,X) = Just 7
deterministicMove (X,_,_,_,Empty,_,_,_,X) = Just 5
deterministicMove (X,_,_,_,X,_,_,_,Empty) = Just 3
deterministicMove (_,_,Empty,_,X,_,X,_,_) = Just 9
deterministicMove (_,_,X,_,Empty,_,X,_,_) = Just 5
deterministicMove (_,_,X,_,X,_,Empty,_,_) = Just 1

deterministicMove (_,_,_,_,_,_,_,_,_) = Nothing


-- Makes a random move
randomMove :: Board -> IO Int
randomMove b = do
    pos <- randomRIO(1,9) 
    case getAtIndex b pos of
        Empty -> return pos
        _     -> randomMove b


computerMove :: Board -> IO (Board)
computerMove b = 
    case pos of
        Nothing -> do
            pos1 <- randomMove b
            let (Just b') = makeMove b O pos1
            return b'
        _       -> do
            let (Just b') = makeMove b O $ head $ catMaybes [pos]
            return b'
    where pos = deterministicMove b


showBoard :: Board -> IO ()
showBoard (a,b,c,d,e,f,g,h,i) = do
    putStrLn ("|" ++ show a ++ "|" ++ show b ++ "|" ++ show c ++ "|")
    putStrLn ("|" ++ show d ++ "|" ++ show e ++ "|" ++ show f ++ "|")
    putStrLn ("|" ++ show g ++ "|" ++ show h ++ "|" ++ show i ++ "|")


userInput :: String -> IO String
userInput s = do
    putStrLn s
    hFlush stdout
    getLine

-- computer agent
runComputer :: Board -> IO()
runComputer newBoard = do
    computerBoard <- computerMove newBoard
    showBoard computerBoard
    case checkWin computerBoard of
        Just O -> putStrLn "Computer win"
        _      -> startGameForTwo computerBoard False X  

-- Two players
startGameForTwo :: Board -> Bool -> Symbol -> IO ()
startGameForTwo board multiplePlayers symbol = do
    playermove <- userInput " Player 1 - Choose a number from 1 to 9: "
    let newBoard = makeMove board symbol $ read playermove
    case newBoard of
        Nothing -> do
            putStrLn "Not a valid move."
            startGameForTwo board multiplePlayers symbol
        Just newBoard -> do
            if multiplePlayers
                then do
                    showBoard newBoard
                else do
                    putStr ""
            case checkWin newBoard of
                Just X -> do
                    if multiplePlayers
                        then do
                            putStrLn "Player 1 wins"
                        else do
                            showBoard newBoard
                            putStrLn "You win"    
                Just O -> do
                    putStrLn "Player 2 wins"
                _      -> if isTie newBoard 
                            then do
                                if multiplePlayers
                                    then do
                                        putStr ""
                                    else do
                                        showBoard newBoard
                                putStrLn "No winner"
                            else do
                                if multiplePlayers 
                                    then do 
                                        startGameForTwo newBoard multiplePlayers $ changePlayer symbol
                                    else do 
                                        runComputer newBoard



-- Welcoming messages
welcome :: IO()                                   
welcome = do
    putStrLn "|7|8|9| \n|4|5|6|\n|1|2|3|\n"
    showBoard emptyBoard


main = do
    putStrLn "Tic Tac Toe"
    playermove <- userInput "Choose 1 for two players, or 2 for player Vs computer"
    case (read playermove) of
        1 -> do
            putStrLn "You have chosen two players"
            welcome
            startGameForTwo emptyBoard True X
        2 -> do
            putStrLn "You have chosen to play against the computer"
            welcome
            startGameForTwo emptyBoard False X
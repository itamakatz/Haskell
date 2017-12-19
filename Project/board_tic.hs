import System.Random (randomRIO)
import System.IO 
import Data.Maybe
import Text.Read (readMaybe)
import Control.Applicative
import Control.Monad (liftM, ap)

------------------------ Symbol ------------------------ 

data Symbol = X | O | Empty

instance Show Symbol where
    show X = "X"
    show O = "O"
    show Empty = " "

-- bool operator for two Symbols
(&&&&) :: Symbol -> Symbol -> Bool
(&&&&) Empty _ = False
(&&&&) _ Empty = False
(&&&&) _ _ = True

-- bool operator between a Symbol and a Bool
(&&&) :: Symbol -> Bool -> Bool
(&&&) Empty _ = False
(&&&) _ False = False
(&&&) _ _ = True

-- switch current player (for multiplayer mode)
changePlayer :: Symbol -> Symbol
changePlayer X = O
changePlayer O = X

------------------------ Board ------------------------ 

-- type Board = (Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol)
data Board Symbol = Board Symbol Symbol Symbol Symbol Symbol Symbol Symbol Symbol Symbol
-- data Board a = Board a a a a a a a a a
-- deriving (Show)

-- data MyTree a = MyEmptyNode
--               | MyFilledNode a (MyTree a) (MyTree a)
--               deriving (Show)

-- instance Functor MyTree where
--   fmap = liftM

-- instance Applicative MyTree where
--   pure  = return
--   (<*>) = ap

-- instance Monad MyTree where
--    return x = MyFilledNode x MyEmptyNode MyEmptyNode
--    (MyFilledNode x y z) >>= f = f x
--    MyEmptyNode >>= _ = MyEmptyNode

emptyBoard :: (Symbol a) => Board a
emptyBoard = Board Empty Empty Empty Empty Empty Empty Empty Empty Empty

-- print the current state of the game
-- printBoard :: Board -> IO ()
-- printBoard (Board a b c d e f g h i) = do
--     putStrLn ("")
--     putStrLn ("|" ++ show a ++ "|" ++ show b ++ "|" ++ show c ++ "|")
--     putStrLn ("|" ++ show d ++ "|" ++ show e ++ "|" ++ show f ++ "|")
--     putStrLn ("|" ++ show g ++ "|" ++ show h ++ "|" ++ show i ++ "|")
--     putStrLn ("")

-- -- ------------------ General Functions ------------------ 

-- -- print instructions and get users input
-- userInput :: String -> IO String
-- userInput s = do
--     putStrLn s
--     hFlush stdout
--     getLine

-- -- checks that user input is an Int
-- readMaybeInt :: String -> Maybe Int
-- readMaybeInt = readMaybe

-- -- get symbol of the board for a given index
-- getAtIndex :: Board -> Int -> Symbol
-- getAtIndex (Board x _ _ _ _ _ _ _ _) 7 = x 
-- getAtIndex (Board _ x _ _ _ _ _ _ _) 8 = x 
-- getAtIndex (Board _ _ x _ _ _ _ _ _) 9 = x 
-- getAtIndex (Board _ _ _ x _ _ _ _ _) 4 = x 
-- getAtIndex (Board _ _ _ _ x _ _ _ _) 5 = x 
-- getAtIndex (Board _ _ _ _ _ x _ _ _) 6 = x 
-- getAtIndex (Board _ _ _ _ _ _ x _ _) 1 = x 
-- getAtIndex (Board _ _ _ _ _ _ _ x _) 2 = x 
-- getAtIndex (Board _ _ _ _ _ _ _ _ x) 3 = x 


-- -- set symbol in the board for a given index
-- setAtIndex :: Board -> Symbol -> Int -> Board
-- setAtIndex (Board _ b c d e f g h i) symbol 7 = (Board symbol b c d e f g h i)
-- setAtIndex (Board a _ c d e f g h i) symbol 8 = (Board a symbol c d e f g h i)
-- setAtIndex (Board a b _ d e f g h i) symbol 9 = (Board a b symbol d e f g h i)
-- setAtIndex (Board a b c _ e f g h i) symbol 4 = (Board a b c symbol e f g h i)
-- setAtIndex (Board a b c d _ f g h i) symbol 5 = (Board a b c d symbol f g h i)
-- setAtIndex (Board a b c d e _ g h i) symbol 6 = (Board a b c d e symbol g h i)
-- setAtIndex (Board a b c d e f _ h i) symbol 1 = (Board a b c d e f symbol h i)
-- setAtIndex (Board a b c d e f g _ i) symbol 2 = (Board a b c d e f g symbol i)
-- setAtIndex (Board a b c d e f g h _) symbol 3 = (Board a b c d e f g h symbol)

-- -- Check when a player wins
-- checkWin :: Board -> Maybe Symbol
-- checkWin (Board O O O _ _ _ _ _ _) = Just O
-- checkWin (Board _ _ _ O O O _ _ _) = Just O
-- checkWin (Board _ _ _ _ _ _ O O O) = Just O
-- checkWin (Board O _ _ O _ _ O _ _) = Just O
-- checkWin (Board _ O _ _ O _ _ O _) = Just O
-- checkWin (Board _ _ O _ _ O _ _ O) = Just O
-- checkWin (Board O _ _ _ O _ _ _ O) = Just O
-- checkWin (Board _ _ O _ O _ O _ _) = Just O
-- checkWin (Board X X X _ _ _ _ _ _) = Just X
-- checkWin (Board _ _ _ X X X _ _ _) = Just X
-- checkWin (Board _ _ _ _ _ _ X X X) = Just X
-- checkWin (Board X _ _ X _ _ X _ _) = Just X
-- checkWin (Board _ X _ _ X _ _ X _) = Just X
-- checkWin (Board _ _ X _ _ X _ _ X) = Just X
-- checkWin (Board X _ _ _ X _ _ _ X) = Just X
-- checkWin (Board _ _ X _ X _ X _ _) = Just X

-- checkWin _ = Nothing

-- -- Checks for a tie
-- isTie :: Board -> Bool
-- isTie (Board a b c d e f g h i) = (&&&) a $ (&&&) b  $ (&&&) c $ (&&&) d $ (&&&) e $ (&&&) f $ (&&&) g $ (&&&&) h i

-- -- ------------------ Computer Functions ------------------

-- -- Computer attempts to make a winning move
-- deterministicMove :: Board -> Maybe Int
-- deterministicMove (Board Empty O O _ _ _ _ _ _) = Just 7
-- deterministicMove (Board O Empty O _ _ _ _ _ _) = Just 8
-- deterministicMove (Board O O Empty _ _ _ _ _ _) = Just 9
-- deterministicMove (Board _ _ _ Empty O O _ _ _) = Just 4
-- deterministicMove (Board _ _ _ O Empty O _ _ _) = Just 5
-- deterministicMove (Board _ _ _ O O Empty _ _ _) = Just 6
-- deterministicMove (Board _ _ _ _ _ _ Empty O O) = Just 1
-- deterministicMove (Board _ _ _ _ _ _ O Empty O) = Just 2
-- deterministicMove (Board _ _ _ _ _ _ O O Empty) = Just 3
-- deterministicMove (Board Empty _ _ O _ _ O _ _) = Just 7
-- deterministicMove (Board O _ _ Empty _ _ O _ _) = Just 4
-- deterministicMove (Board O _ _ O _ _ Empty _ _) = Just 1
-- deterministicMove (Board _ Empty _ _ O _ _ O _) = Just 8
-- deterministicMove (Board _ O _ _ Empty _ _ O _) = Just 5
-- deterministicMove (Board _ O _ _ O _ _ Empty _) = Just 2
-- deterministicMove (Board _ _ Empty _ _ O _ _ O) = Just 9
-- deterministicMove (Board _ _ O _ _ Empty _ _ O) = Just 6
-- deterministicMove (Board _ _ O _ _ O _ _ Empty) = Just 3
-- deterministicMove (Board Empty _ _ _ O _ _ _ O) = Just 7
-- deterministicMove (Board O _ _ _ Empty _ _ _ O) = Just 5
-- deterministicMove (Board O _ _ _ O _ _ _ Empty) = Just 3
-- deterministicMove (Board _ _ Empty _ O _ O _ _) = Just 9
-- deterministicMove (Board _ _ O _ Empty _ O _ _) = Just 5
-- deterministicMove (Board _ _ O _ O _ Empty _ _) = Just 1

-- -- Computer attempts to block off the player's winning move
-- deterministicMove (Board Empty X X _ _ _ _ _ _) = Just 7
-- deterministicMove (Board X Empty X _ _ _ _ _ _) = Just 8
-- deterministicMove (Board X X Empty _ _ _ _ _ _) = Just 9
-- deterministicMove (Board _ _ _ Empty X X _ _ _) = Just 4
-- deterministicMove (Board _ _ _ X Empty X _ _ _) = Just 5
-- deterministicMove (Board _ _ _ X X Empty _ _ _) = Just 6
-- deterministicMove (Board _ _ _ _ _ _ Empty X X) = Just 1
-- deterministicMove (Board _ _ _ _ _ _ X Empty X) = Just 2
-- deterministicMove (Board _ _ _ _ _ _ X X Empty) = Just 3
-- deterministicMove (Board Empty _ _ X _ _ X _ _) = Just 7
-- deterministicMove (Board X _ _ Empty _ _ X _ _) = Just 4
-- deterministicMove (Board X _ _ X _ _ Empty _ _) = Just 1
-- deterministicMove (Board _ Empty _ _ X _ _ X _) = Just 8
-- deterministicMove (Board _ X _ _ Empty _ _ X _) = Just 5
-- deterministicMove (Board _ X _ _ X _ _ Empty _) = Just 2
-- deterministicMove (Board _ _ Empty _ _ X _ _ X) = Just 9
-- deterministicMove (Board _ _ X _ _ Empty _ _ X) = Just 6
-- deterministicMove (Board _ _ X _ _ X _ _ Empty) = Just 3
-- deterministicMove (Board Empty _ _ _ X _ _ _ X) = Just 7
-- deterministicMove (Board X _ _ _ Empty _ _ _ X) = Just 5
-- deterministicMove (Board X _ _ _ X _ _ _ Empty) = Just 3
-- deterministicMove (Board _ _ Empty _ X _ X _ _) = Just 9
-- deterministicMove (Board _ _ X _ Empty _ X _ _) = Just 5
-- deterministicMove (Board _ _ X _ X _ Empty _ _) = Just 1

-- deterministicMove (Board _ _ _ _ _ _ _ _ _) = Nothing

-- -- Makes a random move in case there is no deterministic move
-- randomMove :: Board -> IO Int
-- randomMove b = do
--     pos <- randomRIO(1,9) 
--     case getAtIndex b pos of
--         Empty -> return pos
--         _     -> randomMove b

-- -- prints board after computers move and checks if the computer won
-- finishComputerMove :: Board -> IO()
-- finishComputerMove computerBoard = do 
--     printBoard computerBoard
--     case checkWin computerBoard of
--         Just O -> putStrLn " ----- Computer wins -----\n"
--         _      -> startGame computerBoard False X

-- -- run computers agent
-- runComputer :: Board -> IO ()
-- runComputer b = do
--     let pos = deterministicMove b
--     case pos of
--         Nothing -> do
--             pos1 <- randomMove b
--             let (Just computerBoard) = makeMove b O pos1
--             finishComputerMove computerBoard
--         _       -> do
--             let (Just computerBoard) = makeMove b O $ head $ catMaybes [pos]
--             finishComputerMove computerBoard 

-- ------------------ Workflow Functions ------------------

-- -- update board after some players move
-- makeMove :: Board -> Symbol -> Int -> Maybe Board
-- makeMove board symbol index =
--     case getAtIndex board index of 
--         Empty -> Just $ setAtIndex board symbol index
--         _ -> Nothing

-- -- main recursive function of the game
-- startGame :: Board -> Bool -> Symbol -> IO ()
-- startGame board multiplePlayers symbol = do
--     playermove <- userInput "Player 1 - Choose a number from 1 to 9:"

-- -- parse user input

--     case readMaybeInt playermove of
--         Nothing -> do
--             putStrLn "\n*** Not a valid character. Please Choose a number ***\n"
--             startGame board multiplePlayers symbol
--         Just move -> do
--             if move > 9 || move < 1
--                 then do
--                     putStrLn "\n*** Not a valid number. Please keep input between 1 to 9 ***\n"
--                     startGame board multiplePlayers symbol
--                 else do
--                     let newBoard = makeMove board symbol $ move
--                     case newBoard of
--                         Nothing -> do
--                             putStrLn "\n*** Not a valid move. Please Choose an empty cell ***\n"
--                             startGame board multiplePlayers symbol

-- -- update board, check the state of the game and continue to next move
                        
--                         Just newBoard -> do
--                             if multiplePlayers
--                                 then do
--                                     printBoard newBoard
--                                 else do
--                                     putStr ""
--                             case checkWin newBoard of
--                                 Just X -> do
--                                     if multiplePlayers
--                                         then do
--                                             putStrLn "----- Player 1 wins -----\n"
--                                         else do
--                                             printBoard newBoard
--                                             putStrLn "----- You win -----\n"    
--                                 Just O -> do
--                                     putStrLn "----- Player 2 wins -----\n"
--                                 _      -> if isTie newBoard 
--                                             then do
--                                                 if multiplePlayers
--                                                     then do
--                                                         putStr ""
--                                                     else do
--                                                         printBoard newBoard
--                                                 putStrLn "----- No winner -----\n"
--                                             else do
--                                                 if multiplePlayers 
--                                                     then do 
--                                                         startGame newBoard multiplePlayers $ changePlayer symbol
--                                                     else do 
--                                                         runComputer newBoard

-- -- Welcoming messages
-- welcome :: IO ()                                   
-- welcome = do
--     putStrLn "\n|7|8|9| \n|4|5|6|\n|1|2|3|"
--     printBoard emptyBoard

-- -- user enterd a invalid character
-- invalidInput :: IO ()
-- invalidInput = do
--     putStrLn "\n*** You have enterd an invalid character. Please choose 1 or 2. ***\n"
--     chooseGameType    

-- -- chooses a game and checks user input is correct 
-- chooseGameType :: IO ()
-- chooseGameType = do
--     playermove <- userInput "Choose 1 for two players, or 2 for player Vs computer"
--     case readMaybeInt playermove of
--         Just 1 -> do
--             putStrLn "\n----- You have chosen two players -----"
--             welcome
--             startGame emptyBoard True X
--         Just 2 -> do
--             putStrLn "\n----- You have chosen to play against the computer -----"
--             welcome
--             startGame emptyBoard False X
--         Nothing -> do
--             invalidInput
--         Just _ -> do
--             invalidInput

main = do
    putStrLn "\n-----------------------"
    putStrLn "----- Tic Tac Toe -----"
    putStrLn "-----------------------\n"
--     chooseGameType
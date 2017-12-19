module Main where
import Control.Applicative
import Control.Monad (liftM, ap)

newtype State s a = State { runState :: s -> (a, s) } deriving (Show)

instance Functor (State s) where
    fmap = liftM	

instance Applicative (State s) where
    pure  = return
    (<*>) = ap

instance Monad (State s) where
	return a = State $ \s -> (a, s)
	m >>= k = State $ \s -> let (a, s') = runState m s
	                         in runState (k a) s'

main :: IO ()
main  = do 
    let s1 = State 1 1
    print(s1)
    -- print (tree >>= (\x     

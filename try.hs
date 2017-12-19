module Main where
 
import Control.Applicative
import Control.Monad (liftM, ap)

data MyTree a = MyEmptyNode
              | MyFilledNode a (MyTree a) (MyTree a)
              deriving (Show)
 
instance Functor MyTree where
  fmap = liftM

instance Applicative MyTree where
  pure  = return
  (<*>) = ap
 
instance Monad MyTree where
   return x = MyFilledNode x MyEmptyNode MyEmptyNode
   (MyFilledNode x y z) >>= f = f x
   MyEmptyNode >>= _ = MyEmptyNode
 
main :: IO ()
main  =
	do
		putStrLn "Program begins."

		putStrLn "Tests that prove that MyTree behaves as a type constructor."

		let tree1 = MyFilledNode 5 (MyFilledNode 3 MyEmptyNode MyEmptyNode) (MyFilledNode 2 MyEmptyNode MyEmptyNode)
		print tree1

		putStrLn ""
		putStrLn ""

		let tree2 = MyFilledNode "ABC" (MyFilledNode "AB" MyEmptyNode MyEmptyNode) (MyFilledNode "ABCDEF" MyEmptyNode MyEmptyNode)
		print tree2

		putStrLn ""
		putStrLn ""

		putStrLn "Tests that prove that MyTree behaves as a Functor."

		putStrLn ""
		putStrLn ""

		print (fmap (*2) tree1)
		print (fmap length tree2)

		putStrLn ""
		putStrLn ""

		putStrLn "Tests that prove that MyTree behaves as an Applicative."

		putStrLn ""
		putStrLn ""

		print ((MyFilledNode (*2) MyEmptyNode MyEmptyNode) <*> tree1)
		print ((MyFilledNode init MyEmptyNode MyEmptyNode) <*> tree2)

		putStrLn ""
		putStrLn ""

		putStrLn "Tests that prove that MyTree behaves as a Monad."

		putStrLn ""
		putStrLn ""

		print (tree1 >>= (\x -> MyFilledNode (x+200) MyEmptyNode MyEmptyNode))
		print (tree2 >>= (\x -> MyFilledNode (tail x) MyEmptyNode MyEmptyNode))

		putStrLn ""
		putStrLn ""

		putStrLn "Program ends."
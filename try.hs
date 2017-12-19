module Main where
import Control.Applicative
import Control.Monad (liftM, ap)


data MyTree a = EmptyNode | Tree a (MyTree a) (MyTree a)
                deriving (Show)
 
instance Functor MyTree where
  fmap = liftM

instance Applicative MyTree where
  pure  = return
  (<*>) = ap

instance Monad MyTree where
   return x = Tree x EmptyNode EmptyNode
   -- (Tree x EmptyNode EmptyNode) >>= f = f x
   (Tree x y z) >>= f = let x' y' z' = 
   -- (Tree x y EmptyNode) >>= f = f y
   EmptyNode >>= _ = EmptyNode
 
main :: IO ()
main  =
   do 
      let tree = Tree 5 (Tree 1 EmptyNode EmptyNode) (EmptyNode)
      print(tree)
      print (tree >>= (\x -> Tree (x*x) EmptyNode EmptyNode))
      -- print (tree >>= (\x -> Tree (x*x) EmptyNode EmptyNode))
 
import Prelude

myLast :: [a] -> a
myLast (xs:x) = x

main = do
	arg <- getLine
	print(myLast arg)

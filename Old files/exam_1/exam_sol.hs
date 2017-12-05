--  :cd c:\Users\michael\Desktop\Haskell

length':: [a]->Int
length'  = foldr (\_ x -> x+1) 0

map':: (a->b)->[a]-> [b]
map' f = foldr ((:).f) []

data Mt = Val Int | M1 Mt | M2 Mt Mt

t = M2 (M2 (Val 4) (M1 (Val 5))) (M1 (M1 (Val 3)))

ev:: Mt->Int
ev (Val x) = x
ev (M1 x) = 0 - (ev x)
ev (M2 x y) = (ev x)-(ev y)


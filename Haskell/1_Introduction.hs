module Introduction where

{-
The first equation states that the sum of the empty list is zero, 
while the second states that the sum of any non-empty list comprising a first number 
n and a remaining list of numbers ns is given by adding n and the sum of ns. 
-}
sm [] = 0
sm (n:ns) = n + sm ns

rv [] = []
rv(n:ns) = rv ns ++ [n]

{-
In this definition, ++ is an operator that appends two lists together; 
for example, [1,2,3] ++ [4,5] “ [1,2,3,4,5]. In turn, where is a keyword that 
introduces local definitions, in this case a list smaller comprising all elements 
a from the list xs that are less than or equal to x, together with a list larger 
comprising all elements b from xs that are greater than x
-}
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x] 
        larger = [b | b<-xs, b>x]


{-
seqn [getChar,getChar,getChar]
for any monadic type m, of which IO is just one example, seqn maps a list of actions of 
type m a into a single action that returns a list of values of type a.
-}
seqn [] = return [] 
seqn (act:acts) = do x <- act 
                     xs <- seqn acts 
                     return (x:xs)
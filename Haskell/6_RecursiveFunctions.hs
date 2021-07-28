module RecursiveFunctions where

facDef n = product [1..n]

fac 0 = 1
fac n = n * fac (n-1)

-- m*0=0
-- m * n = m + (m * (n-1))

product2 [] = 1
product2 (n:ns) = n * product2 ns

-- Recall that lists in Haskell are actually constructed one element at a time using the 
-- cons operator. Hence, [2,3,4] is just an abbreviation for 2:(3:(4:[]))

-- the reverse of the empty list is simply the empty list, and the reverse of 
-- any non-empty list is given by appending the reverse of its tail and a singleton 
-- list comprising the head of the list
reverse2 [] = []
reverse2 (n:ns) = reverse2 ns ++ [n]

-- reverse [1,2,3]
-- reverse [2,3] ++ [1] 
-- (reverse [3] ++ [2]) ++ [1] 
-- ((reverse [] ++ [3]) ++ [2]) ++ [1] 
-- (([] ++ [3]) ++ [2]) ++ [1]
-- [3,2,1]

-- [] ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)


-- if x <= y then the new element x is simply prepended to the start of the list, 
-- otherwise the head y becomes the first element of the resulting list, and we then 
--     proceed to insert the new element into the tail of the given list
insert x [] = [x]
insert x (y:ys) 
    | x <= y = x : y : ys
    | otherwise = y : insert x ys

-- using insert we can now define a function that implements insertion sort, 
-- in which the empty list is already sorted, and any non-empty list is sorted by 
-- inserting its head into the list that results from sorting its tail:
isort [] = []
isort (x:xs) = insert x (isort xs)


-- Multiple arguments
zip2 _ [] = []
zip2 [] _ = []
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys

drop2 0 xs = xs
drop2 _ [] = []
drop2 n (_:xs) = drop2 (n-1) xs

-- Multiple recursion

-- Functions can also be defined using multiple recursion, in which a function is applied 
-- more than once in its own definition. 

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- the empty list is already sorted, and any non-empty list can be sorted by placing its head 
-- between the two lists that result from sorting those elements of its tail that are smaller 
-- and larger than the head
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [ a | a <- xs, a <= x ] 
        larger = [ b | b <- xs, b > x ]


-- Mutual recursion

even2 0 = True
even2 n = odd1 (n-1)

odd1 0 = False
odd1 n = even2 (n-1)

-- functions that select the elements from a list at all even and odd positions (counting from zero) 
-- can be defined as follows
evens [] = []
evens (x:xs) = x : odds xs

odds [] = []
odds (_:xs) = evens xs



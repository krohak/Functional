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


init2 [_] = []
init2 (x:xs) = x : init2 xs

{-
Exercises
-}

-- How does the recursive version of the factorial function behave if applied to a negative argument, such as (-1)? 
-- Modify the definition to prohibit negative arguments by adding a guard to the recursive case.
fac2 0 = 1
fac2 n | n > 0 = n * fac2 (n-1)


-- Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative integers from a given value 
-- down to zero. For example, sumdown 3 should return the result 3+2+1+0 = 6.
sumdown 1 = 1
sumdown n | n > 0 = n + sumdown (n-1)


-- Define the exponentiation operator ^ for non-negative integers using the same pattern of recursion as the multiplication 
-- operator *, and show how the expression 2 ^ 3 is evaluated using your definition.
a^0 = 1
a^b = a * (a RecursiveFunctions.^(b-1))

-- Define a recursive function euclid :: Int -> Int -> Int that implements Euclid’s algorithm for calculating the greatest common 
-- divisor of two non- negative integers: if the two numbers are equal, this number is the result; otherwise, the smaller number is 
-- subtracted from the larger, and the same process is then repeated. For example:
euclid m n
    | m==n = m
    | otherwise = euclid (max m n - min m n) (min m n)



-- Without looking at the definitions from the standard prelude, define the following library functions on lists using recursion.
-- a. Decide if all logical values in a list are True: and :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs)
    | not x = False
    | otherwise = allTrue xs

-- b. Concatenate a list of lists:
-- concat :: [[a]] -> [a]   
conc [xs] = xs
conc (x:y:xs) = conc ((x++y): xs)

-- c. Produce a list with n identical elements:
-- replicate :: Int -> a -> [a] 
replicate2 0 a = []
replicate2 n a = a : replicate2 (n-1) a

-- d. Select the nth element of a list:
-- (!!) :: [a] -> Int -> a
select (x:xs) 0 = x
select (x:xs) n = select xs (n-1)

-- e. Decide if a value is an element of a list:
-- elem :: Eq a => a -> [a] -> Bool
elem2 a [] = False
elem2 a (x:xs)
    | a==x = True 
    | otherwise = elem2 a xs


-- Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted lists to give a single sorted list. For example:
-- > merge [2,5,6] [1,3,4] 
-- [1,2,3,4,5,6]
-- Note: your definition should not use other functions on sorted lists such as insert or isort, but should be defined using explicit recursion.
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys


-- Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort, in which the empty list and singleton lists are already sorted, 
-- and any other list is sorted by merging together the two lists that result from sorting the two halves of the list separately.
msort [] = []
msort [x] = [x]
msort xs = merge (msort firstHalf) (msort secondHalf)
    where 
        firstHalf = take half xs
        secondHalf = drop half xs
        half = length xs `div` 2

-- Using the five-step process, construct the library functions that:
-- a. calculate the sum of a list of numbers;
sum2 [xs] = xs
sum2 (x:xs) = x+sum2(xs)

-- b. take a given number of elements from the start of a list;
take2 0 _ = []
take2 n (x:xs) = x : take2 (n-1) xs

-- c. select the last element of a non-empty list.
selectLast [x] = x
selectLast (x:xs) = selectLast xs

module HigherOrderFunctions where
import Data.Char
import Data.List

-- Formally speaking, a function that takes a function as an argument 
-- or returns a function as a result is called a higher-order function

-- add :: Int -> Int -> Int 
-- add x y = x + y
-- means
-- add :: Int -> (Int -> Int) 
-- add = \x -> (\y -> x + y)

twice f x = f (f x)
t1 = twice (*2) 3
-- 12
t2 = twice reverse [1,2,3]
-- [1,2,3]

map1 f xs = [f x | x <- xs]
m1 = map1 (+1) [1,3,5,7]
-- [2,4,6,8]
m2 = map reverse ["abc","def","ghi"]
-- ["cba","fed","ihg"]

m3 = map (map (+1)) [[1,2,3],[4,5]]
-- [[2,3,4],[5,6]]

map2 _ [] = []
map2 f (x:xs) = f x : map2 f xs

filter1 p xs = [x | x <- xs, p x]

f1 = filter (> 5) [1..10]
-- [6,7,8,9,10]
f2 = filter (/= ' ') "abc def ghi"
-- "abcdefghi"

filter2 f [] = []
filter2 f (x:xs)
    | f x = x: filter2 f xs
    | otherwise = filter2 f xs

sumsqreven ns = sum (map (^2) (filter even ns))

a1 = all even [2,4,6,8]
-- True
a2 = any odd [2,4,6,8]
-- False
t3 = takeWhile even [2,4,6,7,8]
-- [2,4,6]
d1 = dropWhile odd [1,3,5,6,7]
-- [6,7]

-- foldr function

-- Many functions that take a list as their argument can be defined using the
-- following simple pattern of recursion on lists:
-- f [] = v
-- f (x:xs) = x # f xs
-- That is, the function maps the empty list to a value v, and any non-empty list 
-- to an operator # applied to the head of the list and the result of recursively 
-- processing the tail.
sum1 [] = 0
sum1(x:xs) = x+sum1 xs
or1 [] = False
or1 (x:xs) = x || or1 xs
and1 [] = True
and1 (x:xs) = x && and1 xs

-- foldr (abbreviating fold right) encapsulates this pattern of recursion for defining 
-- functions on lists, with the operator # and the value v as arguments
a sum2 = foldr (+) 0
or2 :: [Bool] -> Bool
or2 = foldr (||) False
and2 :: [Bool] -> Bool
and2 = foldr (&&) True

-- These new definitions could also include explicit list arguments, as in
sum3 xs = foldr (+) 0 xs
or3 xs = foldr (||) False xs


foldr2 f v [] = v
foldr2 f v (x:xs) = f x (foldr2 f v xs)

-- That is, the function foldr f v maps the empty list to the value v, and any 
-- non-empty list to the function f applied to the head of the list and the 
-- recursively processed tail. In practice, however, it is best to think of the 
-- behaviour of foldr f v in a non-recursive manner, as simply replacing each cons 
-- operator in a list by the function f, and the empty list at the end by the value v.
-- For example, applying the function foldr (+) 0 to the list
-- 1 : (2 : (3 : []))
-- gives the result
-- 1 + (2 + (3 + 0))

-- alculating the length of a list amounts to replacing each cons by the function that adds 
-- one to its second argument, and the empty list by zero.
length1 :: [a] -> Int
length1 = foldr (\_ n -> 1+n) 0

reverse1 :: [a] -> [a]
reverse1 = foldr (\x xs -> xs ++ [x]) []


-- foldr (#) v [x0,x1,...,xn] = x0 # (x1 # (... (xn # v) ...))


-- foldl

-- he function sum can be redefined in this manner by using an auxiliary function sum’ that takes 
-- an extra argument v that is used to accumulate the final result:
sum4 = sum' 0
    where
        sum' v [] = v
        sum' v (x:xs) = sum' (v+x) xs


-- many functions on lists can be defined using the following simple pattern of recursion:
-- f v [] = v
-- f v (x:xs) = f (v # x) xs


-- That is, the function maps the empty list to the accumulator value v, and any non-empty list to the 
-- result of recursively processing the tail using a new accu- mulator value obtained by applying an operator 
-- # to the current value and the head of the list

sum5 :: Num a => [a] -> a
sum5 = foldl (+) 0

reverse3 :: [a] -> [a]
reverse3 = foldl (\xs x -> x:xs) []

foldl2 f v [] = v
foldl2 f v (x:xs) = foldl2 f (f v x) xs

-- foldl (#) v [x0,x1,...,xn] = (... ((v # x0) # x1) ...) # xn

-- The composition operator
-- f . g = \x -> f (g x)

odd2 = not . even
twice2 f = f . f
sumsqreven2 = sum . map (^2) . filter even

-- id = \x -> x
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id


-- Binary string transmitter

type Bit = Int

bin2int bits = sum [w*b | (w,b) <- zip weights bits]
    where weights = iterate (*2) 1

bin2int2 :: [Bit] -> Int
bin2int2 = foldr (\x y -> x + 2*y) 0

int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 bits = take 8 (bits ++ repeat 0)

-- We can now define a function that encodes a string of characters as a list of bits by converting each character 
-- into a Unicode number, converting each such number into an eight-bit binary number, and concatenating each of these 
-- numbers together to produce a list of bits. Using the higher-order functions map and composition, this conversion 
-- can be implemented as follows

encode :: String -> [Bit]
encode = concatMap (make8 . int2bin . ord)


chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)
decode = map (chr . bin2int) . chop8



transmit = decode . channel . encode
channel = id

t = transmit "higher-order functions are easy"
-- "higher-order functions are easy"


-- Voting algorithms
-- First past the post
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count x = length . filter (== x)
c = count "Red" votes
-- 2

rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

r = rmdups votes
-- ["Red", "Blue", "Green"]

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

b = winner votes
-- "Blue"


-- Alternative vote

ballots = [["Red", "Green"],
            ["Blue"],
            ["Green", "Red", "Blue"],
            ["Blue", "Green", "Red"],
            ["Green"]
    ]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim x = map (filter (/= x))



rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

r3 = rank ballots
-- ["Red", "Blue", "Green"]


-- We first remove empty ballots, then rank the remaining 1st-choice can- didates in increasing order of votes. If only one such 
-- candidate remains, they are the winner, otherwise we eliminate the candidate with the smallest number of 1st-choice votes and 
-- repeat the process.
winner' bs = case rank (rmempty bs) of
            [c] -> c
            (c:cs) -> winner' (elim c bs)

w1 = winner' ballots
-- "Green"


{-
Exercises
-}


-- Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using the higher-order functions map and filter.

mapFilterComp :: (t -> a) -> (t -> Bool) -> [t] -> [a]
mapFilterComp f p xs = [f x | x <- xs, p x]
mapFilter :: (t -> a) -> (t -> Bool) -> [t] -> [a]
mapFilter f p = map f . filter p

-- 2. Without looking at the definitions from the standard prelude, define the following higher-order library functions on lists.
-- a. Decide if all elements of a list satisfy a predicate:
-- all2 :: (a -> Bool) -> [Bool] -> Bool
all2 f = foldr (\x xs -> f x && xs) True
all3 p = and . map p

-- b. Decide if any element of a list satisfies a predicate:
-- any :: (a -> Bool) -> [Bool] -> Bool
any2 f = foldr (\x xs -> f x || xs) False
any3 p = or . map p

-- c. Select elements from a list while they satisfy a predicate:
-- takeWhile :: (a -> Bool) -> [a] -> [a]

takeWhile1 f [] = []
takeWhile1 f (x:xs)
    | f x = x: takeWhile1 f xs
    | otherwise = []

-- d. Remove elements from a list while they satisfy a predicate:
-- dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile1 f [] = []
dropWhile1 f (x:xs)
    | f x = dropWhile1 f xs
    | otherwise = x:xs

-- Note: in the prelude the first two of these functions are generic functions rather than being specific to the type of lists.


-- Redefine the functions map f and filter p using foldr.

map3 f = foldr (\x xs -> f x: xs) []

filter3 p = foldr (\x xs -> ([x | p x]) ++ xs) []


-- Using foldl, define a function dec2int :: [Int] -> Int that converts a
-- decimal number into an integer. For example:
-- dec2int [2,3,4,5] 
-- 2345

dec2int :: [Int] -> Int
dec2int = foldl (\xs x -> (*10)xs + x) 0


-- 5. Without looking at the definitions from the standard prelude, define the higher-order library function curry that converts a function 
-- on pairs into a curried function, and, conversely, the function uncurry that converts a curried function with two arguments into a function on pairs.

-- curry1 :: ((a0, b0) -> c0) -> a0 -> b0 -> c0
-- \x.\y = x+y
-- \ x y = x+y

curry1 :: ((a0, b0) -> c0) -> a0 -> b0 -> c0
curry1 f x y = f (x,y)
uncurry1 :: (a0 -> b0 -> c0) -> (a0, b0) -> c0
uncurry1 f(x,y) = f x y

-- A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be defined as follows:
unfold p h t x 
    | p x = []
    | otherwise = h x : unfold p h t (t x)
-- That is, the function unfold p h t produces the empty list if the predicate p is true of the argument value, and otherwise produces a non-empty list by applying 
-- the function h to this value to give the head, and the function t to generate another argument that is recursively processed in the same way to produce the tail 
-- of the list. For example, the function int2bin can be rewritten more compactly using unfold as follows:
int2bin1 = unfold (== 0) (`mod` 2) (`div` 2)
-- Redefine the functions chop8, map f and iterate f using unfold.
chop8two = unfold null (take 8) (drop 8)
map4 f = unfold null (f . head) tail
iterate2 f = unfold null f f


-- Modify the binary string transmitter example to detect simple transmission errors using the concept of parity bits. That is, each eight-bit binary number produced during 
-- encoding is extended with a parity bit, set to one if the number contains an odd number of ones, and to zero otherwise. In turn, each resulting nine-bit binary number 
-- consumed during decoding is checked to ensure that its parity bit is correct, with the parity bit being discarded if this is the case, and a parity error being reported 
-- otherwise. Hint: the library function error :: String -> a displays the given string as an error message and terminates the program; the polymorphic result type ensures 
-- that error can be used in any context.

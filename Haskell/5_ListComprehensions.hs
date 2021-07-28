module ListComprehensions where
import Data.Char
-- list comprehension syntax
-- The symbol | is read as such that, <- is read as is drawn from, 
-- and the expression x <- [1..5] is called a generator
l1 = [x^2 | x <- [1..5]]
-- [1,4,9,16,25]

l2 = [(x,y) | x <- [1,2,3], y <- [4,5]]
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]


l3 = [(x,y) | y <- [4,5], x <- [1,2,3]]
-- [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]

l4 = [(x,y) | x <- [1..3], y <- [x..3]]
-- [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]

-- concat xss = [x | xs <- xss, x <- xs]
l5 = concat [[1,2,3],[4,5,6]]


firsts ps = [x | (x,_) <- ps]
l6 = firsts [(1,2),(4,5)]

lengthX xs = sum [1 | _ <- xs]

-- Guards
l7 = [x | x <- [1..10], even x]


-- Note that deciding that a number such as 15 is not prime does not require 
-- the function prime to produce all of its factors, because under lazy evaluation 
-- the result False is returned as soon as any factor other than one or the number 
-- itself is produced, which for this example is given by the factor 3.
factors n = [x | x <- [1..n], n `mod` x == 0]
prime n = factors n == [1,n]
primes n = [x | x <- [2..n], prime x]

-- function find that returns the list of all values that are associated with a given 
-- key in a table can be defined as follows
find k t = [v | (k',v) <- t, k == k']
vals = find 'b' [('a',1),('b',2),('c',3),('b',4)]

-- zip

l8 = zip ['a','b','c'] [1,2,3,4]
-- [(’a’,1),(’b’,2),(’c’,3)]

-- function that returns the list of all pairs of adjacent elements from a list as follows
pairs xs = zip xs (tail xs)
ps = pairs [1,2,3,4]
-- [(1,2),(2,3),(3,4)]
sorted xs = and [x <= y | (x,y) <- pairs xs]
s1 = sorted [1,3,2,4]
-- False

-- function that returns the list of all positions at which a value occurs in a list, by pairing 
-- each element with its position, and selecting those positions at which the desired value occurs:
positions x xs = [i | (x',i) <- zip xs [0..], x == x']
l10 = positions False [True, False, True, False]
-- [1,3]

-- Within the definition for positions, the expression [0..] produces the list of indices [0,1,2,3,...]. 
-- This list is notionally infinite, but under lazy evaluation only as many elements of the list as required 
-- by the context in which it is used, in this case zipping with the input list xs, will actually be produced. 


-- String comprehensions

z1 = zip "abc" [1,2,3,4]
-- [(’a’,1),(’b’,2),(’c’,3)]


lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']
count x xs = sum [1 | x' <- xs, x == x']


-- The Caesar cipher 

-- For simplicity, we will only encode the lower-case letters within a string, leaving other characters 
-- such as upper-case letters and punctuation unchanged. 

-- We begin by defining a function let2int that converts a lower-case letter between ’a’ and ’z’ into 
-- the corresponding integer between 0 and 25, together with a function int2let that performs the opposite 
-- conversion

let2int c = ord c - ord 'a'
int2let n = chr (ord 'a' + n)

-- function shift that applies a shift fac- tor to a lower-case letter by converting the letter into the 
-- corresponding integer, adding on the shift factor and taking the remainder when divided by twenty- six 
-- (thereby wrapping around at the end of the alphabet), and converting the resulting integer back into a 
-- lower-case letter

shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode n xs = [shift n x | x <- xs]

en = encode 3 "haskell is fun"
-- "kdvnhoo lv ixq"
de = encode (-3) en
-- "haskell is fun"

-- frequency table 
-- By analysing a large volume of such text, one can derive the following table of approximate percentage 
-- frequencies of the twenty-six letters of alphabet
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
  0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0,
  2.8, 1.0, 2.4, 0.2, 2.0, 0.1]


-- The library function fromIntegral :: Int -> Float converts an integer into a floating-point number
percent n m = (fromIntegral n / fromIntegral m) * 100

-- Using percent within a list comprehension, together with the functions lowers and count from the previous section, 
-- we can now define a function that returns a frequency table for any given string
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
            where n = lowers xs

f = freqs "abbcccddddeeeee"
-- [6.666667, 13.333334, 20.0, 26.666668, ..., 0.0]

chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]
rotate n xs = drop n xs ++ take n xs

-- Now suppose that we are given an encoded string, but not the shift factor that was used to encode it, and wish to determine 
-- this number in order that we can decode the string. This can usually be achieved by producing the frequency table of the 
-- encoded string, calculating the chi-square statistic for each possible rotation of this table with respect to the table of 
-- expected frequencies, and using the position of the minimum chi-square value as the shift factor

crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs

c = crack "vscd mywzboroxcsyxc kbo ecopev"
-- "list comprehensions are useful"

{-
Exercises
-}


-- Using a list comprehension, give an expression that calculates the sum 1**2 + 2**2 + . . . 100**2 of the first one hundred 
-- integer squares.
sumSq = sum [ x**2 | x <- [1..100] ]


-- Suppose that a coordinate grid of size m x n is given by the list of all pairs (x, y) of integers such that 0 <= x <= m and 
-- 0 <= y <= n. Using a list comprehension, define a function grid :: Int -> Int -> [(Int,Int)] that returns a coordinate grid 
-- of a given size. For example:
-- grid 1 2 
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
grid m n = [ (x,y) | x<-[0..m], y<-[0..n] ]


-- Using a list comprehension and the function grid above, define a function square :: Int -> [(Int,Int)] that returns a coordinate 
-- square of size n, excluding the diagonal from (0, 0) to (n, n). For example:
squareGrid n = filter (uncurry (/=)) (grid n n)


-- In a similar way to the function length, show how the library function replicate :: Int -> a -> [a] that produces a list of 
-- identical elements can be defined using a list comprehension. For example:
-- replicate 3 True 
-- [True,True,True]
replicate n d = [ d | x<-[0..n-1] ]


-- A triple (x, y, z) of positive integers is Pythagorean if it satisfies the equation x**2 + y**2 = z**2. Using a list 
-- comprehension with three generators, define a function pyths :: Int -> [(Int,Int,Int)] that returns the list of all such triples 
-- whose components are at most a given limit. For example:
-- pyths 10 
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
pyths n = [ (x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x**2+y**2==z**2 ]


-- A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself. Using a list comprehension 
-- and the function factors, define a function perfects :: Int -> [Int] that returns the list of all perfect numbers up to a given 
-- limit. For example:
-- perfects 500 
-- [6,28,496]
perfects n = [ x | x<- [1..n], sum(factors x)-x == x ]


-- Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two generators can be re-expressed using two comprehensions 
-- with single gen- erators. Hint: nest one comprehension within the other and make use of the library function 
-- concat :: [[a]] -> [a].
-- [(1,3),(1,4),(2,3),(2,4)]
kat = concat [ [ (x, y) | y <- [3,4] ] | x <- [1,2] ]

-- Redefine the function positions using the function find.
-- positions x xs = [i | (x',i) <- zip xs [0..], x == x']
-- find k t = [v | (k',v) <- t, k == k']
positions2 x xs = find x (zip xs [0..])


-- The scalar product of two lists of integers xs and ys of length n is given by
-- the sum of the products of corresponding integers:
-- n-1
-- sum (xsi  * ysi)
-- i=0
-- In a similar manner to chisqr, show how a list comprehension can be used to define a function scalarproduct :: 
-- [Int] -> [Int] -> Int that returns the scalar product of two lists. For example:
-- scalarproduct [1,2,3] [4,5,6] 
-- 32
scalarproduct xs ys = sum [ x*y | (x,y) <- zip xs ys ]


-- Modify the Caesar cipher program to also handle upper-case letters.
crackUpper xs = upperAcc (encode (-factor) lower) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs
    lower = map toLower xs
    upperAcc low orig = [ if isUpper o then toUpper l else l | (l,o)<-zip low orig ]

cu = crackUpper "Vscd mywzBoroxcsyxc kbo ecoPev"
-- "List compRehensions are useFul"
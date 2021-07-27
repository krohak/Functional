module DefiningFunctions where

even n = n `mod` 2 == 0

splitAt1 n xs = (take n xs, drop n xs)

recip n = 1 / n

-- conditional expression
abs n = if n >= 0 then n else - n

-- nested conditional expression
signum n =
  if n < 0
    then -1
    else if n == 0 then 0 else 1

-- guards
-- The symbol | is read as such that, and the guard
-- otherwise is defined in the standard prelude simply by
-- otherwise = True
abs2 n
  | n >= 0 = n
  | otherwise = - n

signum2 n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

-- Pattern matching
-- wildcard pattern
True && True = True
_ && _ = False
b && c
  | b == c = b
  | otherwise = False

-- Tuple patterns
fst (x, _) = x

snd (_, y) = y

-- List patterns
-- decides if a list contains precisely three characters
-- beginning with the letter ’a’ can be defined as follows
test ['a', _, _] = True
test _ = False

-- cons
l = 1 : (2 : (3 : []))

test1 ('a' : _) = True
test1 _ = False

-- Note that cons patterns must be parenthesised, because function application
-- has higher priority than all other operators in the language. For example,
-- the definition head x:_ = x without parentheses means (head x):_ = x
head1 (x : _) = x

tail1 (_ : xs) = xs

-- Lambda expressions
-- The symbol \ represents the Greek letter lambda
double = (\x -> x + x) 2

add1 x y = x + y

add2 = \x -> (\y -> x + y)

const1 x _ = x

const2 x = \_ -> x

-- The library function map applies a function to all elements of a list
odds1 n = map f [0 .. n -1]
  where
    f x = x * 2 + 1

odds2 n = map (\x -> x * 2 + 1) [0 .. n -1]

-- Operator sections
-- (#) = \x -> (\y -> x#y)
-- (x#) = \y -> x#y
-- (#y) = \x -> x#y

-- (+) is the addition function \x -> (\y -> x+y) 
-- (1+) is the successor function \y -> 1+y
-- (1/) is the reciprocation function \y -> 1/y 
-- (*2) is the doubling function \x -> x*2
-- (/2) is the halving function \x -> x/2


{-
Exercises
-}


-- Using library functions, define a function halve :: [a] -> ([a],[a]) 
-- that splits an even-lengthed list into two halves. For example:
-- > halve [1,2,3,4,5,6] ([1,2,3],[4,5,6])

halve xs = splitAt n xs
            where n = length xs `div` 2



-- Define a function third :: [a] -> a that returns the third element in a list 
-- that contains at least this many elements using:
-- a. head and tail 
-- b. list indexing !!
-- c. pattern matching.

thirdA xs = head (tail (tail xs))
thirdB xs = xs !! 2
thirdC (_:_:x:_) = x


--  Consider a function safetail :: [a] -> [a] that behaves in the same way as tail except 
--  that it maps the empty list to itself rather than producing an error. Using tail and the 
--  function null :: [a] -> Bool that decides if a list is empty or not, define safetail using:
-- a. a conditional expression
-- b. guarded equations
-- c. pattern matching

safetailA xs = if not (null xs) then tail xs else xs
safetailB xs
  | not (null xs) = tail xs
  | otherwise = xs
safetailC (_:x) = x
safetailC _ = []

-- In a similar way to && in section 4.4, show how the disjunction operator 
-- || can be defined in four different ways using pattern matching.
False || False = False
_ || _ = True

b || c
  | b==c = b
  | otherwise = True

-- The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a digit, 
-- and proceeds as follows:
-- ‚ consider each digit as a separate number;
-- ‚ moving left, double every other number from the second last;
-- ‚ subtract 9 from each number that is now greater than 9;
-- ‚ add all the resulting numbers together;
-- ‚ if the total is divisible by 10, the card number is valid.
-- Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9 if the result is 
-- greater than 9. For example:
-- > luhnDouble 3 
-- 6
-- > luhnDouble 6 
-- 3

luhnDouble x = if (* 2) x > 9 then (* 2) x -9 else  (* 2) x

-- Using luhnDouble and the integer remainder function mod, define a function luhn :: Int -> Int -> Int -> Int -> Bool 
-- that decides if a four-digit bank card number is valid. For example:
-- > luhn 1 7 8 4 
-- True
-- > luhn 4 7 8 3 
-- False
 
luhn _ x y z = sum (map luhnDouble [x,y,z]) `mod` 10 == 0
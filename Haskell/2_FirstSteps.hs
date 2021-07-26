module FirstSteps where

-- Select the first element of a non-empty list
x = head [1,2,3,4,5]

-- Remove the first element from a non-empty list
xs = tail [1,2,3,4,5]

-- Select the nth element of list (counting from zero)
x2 = [1,2,3,4,5] !! 2

-- Select the first n elements of a list
xs2 = take 3 [1,2,3,4,5]

-- Remove the first n elements from a list
xs3 = drop 3 [1,2,3,4,5]


-- Calculate the length of a list: 
l = length [1,2,3,4,5]

-- ‚ Calculate the sum of a list of numbers: > 
s = sum [1,2,3,4,5]

-- ‚ Calculate the product of a list of numbers: > 
p = product [1,2,3,4,5]

-- ‚ Append two lists:
l1 = [1,2,3] ++ [4,5]

-- ‚ Reverse a list:
r = reverse [1,2,3,4,5]


double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns



a = b+c 
    where
     b = 1
     c = 2

o = m + n where {m = 1; n = 2}
{- CSC324 Winter 2018 Lab 9 -}

module Lab9 where


-- Task 1: Fitting some generic functions
-- For each of the functions below, give an implementation
-- of them so that they are *total* (always terminate and don't raise
-- and error), and compile with the given type signatures.
--
-- Feel free to change the function and parameter names
-- to be more descriptive!

roundX :: Float->Int
roundX x = round x

-- Note that (_,_) is the Haskell *tuple* type, which is also generically polymorphic.
f0 :: a -> (a, a)
f0 x = (x,x)
-- f0 2

f1 :: (a -> b) -> a -> b
f1 x y = x y
-- f1 roundX 2.999

f2 :: (b -> c) -> (a -> b) -> a -> c
f2 g h x = g (h x)
-- f2 f0 (roundX 2.999)

-- What's special about this one?
f3 :: (a, b) -> (c -> b)
f3 (x, y) = \c -> y
-- ((f3 ("a", 2.3333)) 4)

-- Task 2: One new generic type, `Maybe`

-- For your reference, here's the definition of the `Maybe` type
-- built into Haskell:
-- data Maybe = Nothing | Just a

getMaybeData :: Maybe a -> a
getMaybeData (Just x) = x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead l = Just (head l)
-- safeHead [1,2,3,4,5,6]

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail l = Just (tail l)
-- safeTail [1,2,3,4,5,6]

onlyWhen :: (a -> Bool) -> a -> Maybe a
onlyWhen cond x = if (cond x) then Just x else Nothing
-- onlywhen (\x->(x==3)) 3

try :: (a -> b) -> Maybe a -> Maybe b
try fun (Nothing) = Nothing
try fun (Just x) = Just (fun x) 
--try roundX (Nothing)
--try roundX (Just 2.33)

-- Task 3: Introduction to typeclasses

data Shape = Circle Float            -- representing the circle radius
           | Rectangle Float Float   -- representing width and height
           | Square Float            -- representing side length

shapeEqual :: Shape -> Shape -> Bool
shapeEqual (Circle x) (Circle y) = (x == y)
shapeEqual (Square x) (Square y) = (x == y)
shapeEqual (Square x) (Rectangle y z) = (x == y) && (x==z)
shapeEqual (Rectangle a b) (Rectangle x y) = (a == x) && (b == y)
shapeEqual _ _ = False


--shapeEqual (Circle 12) (Circle 21)
--shapeEqual (Circle 12) (Circle 12)






















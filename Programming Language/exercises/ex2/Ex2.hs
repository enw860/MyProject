{- CSC324 Winter 2018: Exercise 2

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}

-- The module definition line, including exports. Don't change this!
module Ex2 (List(Empty, Cons), numEvensList, mapList) where

-- | Imports used for testing purposes only.
import Control.Monad (liftM2)
import Test.QuickCheck (Property, quickCheck, (==>), oneof, sized, Arbitrary(..))


{- Recursive definition of a List type. (Don't change this!)
    See handout for details. -}
data List = Empty
            | Cons Int List
            deriving (Show, Eq)


-- | numEvensList returns the number of even items in a list.
-- Here you're using pattern-matching on the different forms of a List
-- ("Empty" or "Cons), because we don't have an explicit "is-empty?" function
-- for this datatype.
numEvensList :: List -> Int
numEvensList Empty =
    -- In this case, the list is empty.
    0
numEvensList (Cons first rest) =
    -- In this case, `first` is an Int representing the first item in the list,
    -- and `rest` is a List representing the other items in the list.
    if(even first) 
        then 1+numEvensList(rest)
        else numEvensList(rest)

-- | mapList behaves the same as `map` for built-in lists in both Racket and Haskell.
mapList :: (Int -> Int) -> List -> List
mapList _ Empty = (Empty)
mapList f (Cons first rest) = 
    let result_first = f first
        result_rest = mapList f (rest)
    in (Cons result_first result_rest)
-------------------------------------------------------------------------------

-- | For testing, we first need to define how to generate "random" Lists.
-- You can safely ignore this code for this exercise.
instance Arbitrary List where
    arbitrary = sized list'
        where
            list' 0 = return Empty
            list' n = oneof [return Empty, liftM2 Cons arbitrary arbitrary]


-- | What does this property mean?
prop_numEvensFirstOdd :: List -> Bool
prop_numEvensFirstOdd nums =
    numEvensList nums == numEvensList (Cons 1 nums)


-- | What does this property mean? (Hint: `id` is the identity function.)
prop_mapIdentity :: List -> Bool
prop_mapIdentity nums =
    -- Note: the reason we can use (==) here to compare Lists
    -- is because of the "deriving Eq" in the type definition.
    -- More on this later in the course.
    mapList id nums == nums


-- | What does this property mean?
prop_mapAdd2 :: List -> Bool
prop_mapAdd2 nums =
    let newNums = mapList (\x -> x + 2) nums
    in
        numEvensList nums == numEvensList newNums


-------------------------------------------------------------------------------

-- | This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation much later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
main :: IO ()
main = do
    quickCheck prop_numEvensFirstOdd
    quickCheck prop_mapIdentity
    quickCheck prop_mapAdd2

{- CSC324 Winter 2018 Lab 10 -}

module Lab10 where

--arr = [(Just 2.8),(Just 9.1),(Just 4.2),(Just 8.8)]

-- Task 1
helper1:: (a->b) -> Maybe a -> Maybe b
helper1 _ Nothing = Nothing
helper1 fun (Just x) = Just (fun x)

mapMaybes :: (a -> b) -> [Maybe a] -> [Maybe b]
mapMaybes fun arrA = fmap (\x->(helper1 fun x)) arrA

--testfun1 :: Double -> Int
--testfun1 x = round x

--testResult1 = mapMaybes testfun1 arr

helper2:: Maybe a -> a
helper2 (Just x) = x 

composeMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
composeMaybe fun1 fun2 = \x -> fun2(helper2 (fun1 x))

--testfun2 :: Double -> Maybe Int
--testfun2 0 = Nothing
--testfun2 x = Just(round x)

--testfun3 :: Int -> Maybe String
--testfun3 0 = Nothing
--testfun3 x = Just(show x)

--testResult2 = ((composeMaybe testfun2 testfun3) 2.3)

foldMaybe :: (b -> a -> Maybe b) -> b -> [a] -> Maybe b
foldMaybe fun arg arr= foldl (\(Just x) y->(fun x y)) (Just arg) arr

--testfun4:: String -> Int -> Maybe String
--testfun4 str num = Just (str ++ (show num))

--testResult3 = foldMaybe testfun4 "NumberStr: " [1..6]

applyBinaryMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
applyBinaryMaybe fun (Just x) (Just y) = Just(fun x y)

--testfun5:: Int -> Double -> String
--testfun5 x y = show (x + (round y))

--testResult4 = applyBinaryMaybe testfun5 (Just 92) (Just 8.29)

collectMaybes :: [Maybe a] -> Maybe [a]
collectMaybes arr = Just(fmap (\(Just x)->x) arr)

--testResult5 = collectMaybes arr

-- Task 2
mapF :: Functor f => (a -> b) -> [f a] -> [f b]
mapF = undefined

composeM :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
composeM = undefined

foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM = undefined

applyBinaryM :: Monad m => (a -> b -> c) -> m a -> m b -> m c
applyBinaryM = undefined

collectM :: Monad m => [m a] -> m [a]
collectM arr = foldr fun (return []) arr
    where
        fun acc val = 
            acc >>= \a-> 
            val >>= \b->
                return(a:b)

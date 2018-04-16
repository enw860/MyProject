import Control.Monad (ap)

data BTree a = Empty | Node a Int (BTree a) (BTree a) deriving Show

sampleTree :: BTree String
sampleTree =
    Node "A" 0
      (Node "B" 0
        (Node "D" 0
          (Node "H" 0 Empty Empty)
          (Node "I" 0 Empty Empty))
        (Node "E" 0
          (Node "J" 0 Empty Empty)
          Empty))
      (Node "C" 0
        (Node "F" 0 Empty Empty)
        (Node "G" 0 Empty Empty))

postOrder :: BTree a -> [(a, Int)]
postOrder Empty = []
postOrder (Node x y left right) =
    postOrder left ++ postOrder right ++ [(x, y)]


data State s a = State (s -> (a, s))

-- "Unwrap" the state
runState :: State s a -> (s -> (a, s))
runState (State f) = f

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put x = State (\s -> ((), x))

example :: State Int String
example = State (\s ->
  let
    (_, s1)  = runState (put 10) s
    (x, s2)  = runState get s1
    (_, s3)  = runState (put (x*2)) s2
    (x', s4) = runState get s3
  in
    (show (x'+1), s4)
  )

example' :: State Int String
example' =
  put 10 >>= \_ ->
  get >>= \x ->
  put (x*2) >>= \_ ->
  get >>= \x' ->
  return (show (x' + 1))

example'' :: State Int String
example'' = do
  _ <- put 10
  x <- get
  _ <- put (x*2)
  x' <- get
  return (show (x' + 1))


instance Monad (State s) where
  -- return :: a -> State s a
  return x = State (\s -> (x, s))

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) op1 opMaker = State (\s ->
    let (x, s1) = runState op1 s
        op2 = opMaker x
        (y, s2) = runState op2 s1
    in
      (y, s2))


-- These are technically required to make a type an instance of Monad.
-- Note that we *aren't* covering Applicative in this course.
instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f op = State (\s ->
    let (x, s1) = runState op s
    in
        (f x, s1))

instance Applicative (State s) where
  pure = return
  (<*>) = ap


postOrderLabel :: BTree a -> BTree a
postOrderLabel tree = fst (runState (postOrderLabelHelper tree) 0)

postOrderLabelHelper :: BTree a -> State Int (BTree a)
postOrderLabelHelper Empty = State (\s -> (Empty, s))
postOrderLabelHelper (Node x _ left right) = State (\s ->
  let (newLeft, s1) = runState (postOrderLabelHelper left) s
      (newRight, s2) = runState (postOrderLabelHelper right) s1
      (i, s3) = runState get s2 -- Note: s3 == s2, and in fact i == s2
      (_, s4) = runState (put (i+1)) s3
  in
      (Node x i newLeft newRight, s4))


-- With Monad operations
postOrderLabel' :: BTree a -> BTree a
postOrderLabel' tree = fst (runState (postOrderLabelHelper' tree) 0)

postOrderLabelHelper' :: BTree a -> State Int (BTree a)
postOrderLabelHelper' Empty = return Empty
postOrderLabelHelper' (Node x _ left right) = do
  newLeft <- postOrderLabelHelper' left
  newRight <- postOrderLabelHelper' right
  i <- get
  _ <- put (i+1)
  return (Node x i newLeft newRight)

{- CSC324 Winter 2018: Lab 11 -}

import Control.Monad (ap)

-- This is the stack code from lecture (same as posted version).
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
-------------------------------------------------------------------------------
-- Task 1: Primitive stack operations
-------------------------------------------------------------------------------

-- We use a list to represent a stack, where the front of the list
-- represents the top of the stack.
type Stack a = [a]


-- | A stateful operation representing a "pop" onto the stack.
-- Can raise an error if the stack is empty.
pop :: State (Stack a) a
pop = State(\(x:xs)->(x, xs))

-- | A stateful operation representing a "push" onto the stack.
push :: a -> State (Stack a) ()
push val = State(\x->((), (val:x)))


-- | A stateful operation that returns whether the stack is empty.
-- Note that this function actually leaves the stack unchanged.
isEmpty :: State (Stack a) Bool
isEmpty = State(\s->((length s)==0, s))


-- | A stateful operation that corresponds to the following imperative-style
-- code snippet, where s is a mutable stack:
--
-- s.push(1)
-- x = s.pop()
-- if (s.is_empty()):
--   s.push(x)
-- else:
--   s.push(x + 1)
--
-- The `if` is easier to deal with first if you're using explicit (>>=) rather
-- than do notation!
-- Also note
play :: State (Stack Int) ()
play = push 2


-------------------------------------------------------------------------------
-- Task 2: Recursing on stacks
-------------------------------------------------------------------------------
-- Note: for the functions below, implement them only in terms of the primitive
-- stack operations found in the previous section.

-- | Remove and return the item that is second from the top of the stack.
-- Assume that the stack has at least 2 items.
removeSecond :: State (Stack a) a
removeSecond = State(\(a:b:rest)->(b, (a:rest)))

-- | Remove and return the item that is third from the top of the stack.
-- Assume that the stack has at least 3 items.
removeThird :: State (Stack a) a
removeThird = State (\(a:b:c:rest)->(c, (a:b:rest)))

-- | Remove and return the item that is n-th from the top of the stack.
-- Assume that n >= 1, and that the stack has at least n items.
removeNth :: Int -> State (Stack a) a
removeNth n = State(\lst ->
 let (a, b:c) = splitAt (n-1) lst
 in (b, (a ++ c)))

-- | Remove and return the item that is n-th from the top of the stack.
-- Fails (returns Nothing) if the stack is too small.
removeNthMaybe :: Int -> State (Stack a) (Maybe a)
removeNthMaybe n = State(\lst ->
 if (n>(length lst)) 
  then (Nothing, lst)
  else
    let (a, b:c) = splitAt (n-1) lst
    in (Just b, (a ++ c)))


-- | Remove and return the first item in the stack (starting from the top)
-- that satisfies the given predicate. Fails (i.e., returns Nothing) if no
-- items in the stack satisfy the predicate.
findNeedle :: (a -> Bool) -> State (Stack a) (Maybe a)
findNeedle pred = State(\lst->
  if (null lst)
    then (Nothing, lst)
    else 
      let (x:xs) = lst
      in
        if (pred x) 
          then (Just x, xs)
          else 
            if ((length xs) == 0)
              then (Nothing, (x:xs))
              else 
                let (val, s1) = runState (findNeedle pred) xs
                in (val, x:s1))



example2 :: State Int String
example2 = State (\s ->
  let
    (_, s1) = runState (put 10) s
    (_, s2) = runState (put 11) s1
    (_, s3) = runState (put 12) s2
    (x, s4) = runState get s3
  in  (show x, s4)
  )




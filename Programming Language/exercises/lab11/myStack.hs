type Stack = [Integer]

s = [4,3,2,1]

pop :: Stack -> (Integer, Stack)
pop (top:rest) = (top, rest)

push :: Stack -> Integer -> Stack
push s a = (a:s)

switchTopTwo :: Stack -> Stack 
switchTopTwo s =
 let 
  (x, s1) = pop s 
  (y, s2) = pop s1
  s3 = push s2 x
  s4 = push s3 y
 in s4

type StackOP a = Stack -> (a, Stack)

pop2:: StackOP Integer
pop2 (top:rest) = (top, rest)

push2:: Integer -> StackOP ()
push2 a = \s -> ((), a:s)

(>>>) :: StackOP a -> StackOP b -> StackOP b
(op1 >>> op2) s= 
 let (_, s1) = op1 s
 in op2 s1
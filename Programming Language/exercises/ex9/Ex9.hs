{- CSC324 Winter 2018: Exercise 9

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}

-- The module definition line, including exports. Don't change this!
module Ex9 (runTypeCheck,
            Prog(..),
            Expr(..),
            Type(..),
            TypeCheckResult) where

-- This is one of Haskell's built-in analogues of dictionaries.
-- https://hackage.haskell.org/package/containers-0.5.11.0/docs/Data-Map-Strict.html
-- Note that this is a *qualified import*, meaning to access any of the names
-- defined in Data.Map.Strict in this file, you'll need to prefix them with "Map."
-- Similar to doing `import data.strict.map as map` in Python.
import qualified Data.Map.Strict as Map

import Test.QuickCheck (quickCheck)


-- |
-- = Data types
--
-- This section contains the main data types used in this exercise.

-- | These two data types represents a program to type-check.
-- This is similar to the "datum" we get pretty much for free
-- in Racket, but uses different data constructors to provide
-- more structure for our functions. The tradeoff is that
-- it's a bit more work to *construct* values of this type:
-- we can't just "quote" an existing piece of code.
--
-- Please read the inline notes for each constructor, which contains
-- useful information about the representation and language restrictions.
data Prog
    -- A program consisting of a single expression.
    = JustExpr Expr

    -- A program with some definitions, followed by an expression.
    -- (This was used on Assignment 1's "Whisper" language.)
    | WithDefines [(String, Expr)] Expr
    -- The "deriving" clause here automatically makes
    -- this type part of the Show *typeclass*
    -- (useful for debugging).
    deriving Show

data Expr = IntLiteral Int
          | BoolLiteral Bool
          | Identifier String  -- Macro keywords like `if` are not valid identifiers.
          | If Expr Expr Expr  -- condition, then-expr, else-expr
          | Call Expr [Expr]   -- function expression and arguments
          | Lambda [String] Expr
          deriving (Show, Eq)


-- | This type represents the *type* of an expression in our language.
-- There are two primitive types (boolean and integer), a function type,
-- and *type variables* used to indicate generic polymorphism.
data Type
    -- The two primitive types have names that are similar, but *not*
    -- the same, as Haskell's types.
    = Bool_
    | Int_

    -- The function type is *not* curried (unlike Haskell, but like Racket).
    -- The constructor's arguments are the function parameter types and return type.
    | Function [Type] Type

    -- A type variable, e.g. "a" in `id :: a -> a`.
    -- The argument is used as an identifier for the variable.
    | TypeVar String
    deriving (Show, Eq, Ord)


-- | A common practice is Haskell is to use `Maybe` or `Either` to represent the result
-- of a computation that could succeed or fail. You've seen `Maybe`
-- in the lab this week; here's the definition of `Either`.
-- data Either a b = Left a | Right b.

-- It's very general (two type parameters!), but one common use is to
-- think of `Right` as `Just`, and `Left` as `Nothing`, except allowing
-- a string to be passed describing the error.
--
-- The following line uses the `type` keyword to create a *type synonym* for a specific
-- `Either` type that we use to represent the result of a type check (similar to `typedef` in C).
-- The `Left` constructor takes a string representing an *error message* when type-checking fails.
-- The `Right` constructor takes a `Type`, and represents the type of an expression that
-- has no type errors.
type TypeCheckResult = Either String Type

-- | The following are the provided error messages for this exercise.
-- You won't use them standalone, but instead by calling `Left` on one to produce
-- a `TypeCheckResult`.
errorIfBranches = "Type error: the two branches of an `if` must have the same type."
errorIfCondition = "Type error: the condition of an `if` must be boolean."
errorCallNotAFunction = "Type error: first expression in a function call must be a function."
errorCallWrongArgNumber = "Type error: function called with the wrong number of arguments."
errorCallWrongArgType = "Type error: function called with an argument of the wrong type."
errorUnboundIdentifier = "Error: unbound identifier"


-- | Similar to the environment we use for interpreting expressions, except it
-- binds identifiers to *types* rather than values.
type TypeEnv = Map.Map String Type


-- | Type signatures for our built-in functions.
-- This map is "loaded" at the start of our type-checking.
builtins :: TypeEnv
builtins = Map.fromList
  [ ("+", Function [Int_, Int_] Int_)
  , ("-", Function [Int_, Int_] Int_)
  , ("*", Function [Int_, Int_] Int_)
  , ("quotient", Function [Int_, Int_] Int_)
  , ("remainder", Function [Int_, Int_] Int_)

  , ("<", Function [Int_, Int_] Bool_)

  , ("and", Function [Bool_, Bool_] Bool_)
  , ("or", Function [Bool_, Bool_] Bool_)
  , ("not", Function [Bool_] Bool_)
  ]

getCompareResult :: TypeCheckResult->TypeCheckResult->TypeCheckResult
getCompareResult (Right x) (Right y) = 
  if (x==y) then (Right x) else (Left errorIfBranches)
getCompareResult (Left x) (Right y) = Left x
getCompareResult (Right x) (Left y) = Left y

isFun:: TypeCheckResult -> Bool
isFun (Right (Function _ _)) = True
isFun _ = False

getFunRetType:: TypeCheckResult -> TypeCheckResult
getFunRetType (Right (Function _ ret)) = Right ret
getFunRetType _ = Left errorCallNotAFunction

getFunArgType:: TypeCheckResult -> TypeCheckResult
getFunArgType (Right (Function l _)) = Right (head l)
getFunArgType _ = Left errorCallNotAFunction

getFunArgNumb:: TypeCheckResult -> Int
getFunArgNumb (Right (Function l _)) = length l
getFunArgNumb _ = 0

isLeft:: TypeCheckResult -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Entry point to the type-checking. We've implemented this for you (though you can
-- change it if you like). Compare this with `run-interpreter` from A1!
runTypeCheck :: Prog -> TypeCheckResult
runTypeCheck (JustExpr expr) = typeCheck builtins expr
runTypeCheck (WithDefines definitions expr) =
    case buildTypeEnv builtins definitions of
        Left msg -> Left msg
        Right newEnv -> typeCheck newEnv expr

-- | This is the core type-checking function. We've started this for you; please
-- study this carefully.
typeCheck :: TypeEnv -> Expr -> TypeCheckResult
typeCheck _ (IntLiteral _) = Right Int_
typeCheck _ (BoolLiteral _) = Right Bool_
typeCheck env (Identifier s) =
    case Map.lookup s env of
        Nothing -> Left errorUnboundIdentifier
        Just t -> Right t

-- Task 1: Implement this rule (but don't worry about the Function one down below).
typeCheck env (If c t e) = 
  if (validCondition) then compareResult
  else Left errorIfCondition
  where
    validCondition = (typeCheck env c) == Right Bool_
    resultT = typeCheck env t
    resultE = typeCheck env e
    compareResult = getCompareResult resultT resultE

-- Task 2: Implement this rule.
typeCheck env (Call f args) = 
  if (isFunction)
    then 
      if (isNumbArgsMatch)
        then 
          if (isAllSameType) 
            then retType 
            else Left errorCallWrongArgType
        else
          Left errorCallWrongArgNumber
    else 
      Left errorCallNotAFunction
  where
    callType = typeCheck env f
    argType = getFunArgType callType
    retType = getFunRetType callType
    argNum = getFunArgNumb callType
    currentArgsNumb = length args
    typesCheck x = ((typeCheck env x) == argType)
    isAllArgsType = map (typesCheck) args

    isNumbArgsMatch = (argNum == currentArgsNumb)
    isFunction = isFun callType
    isAllSameType = and isAllArgsType

getRight:: TypeCheckResult->Type
getRight (Right x) = x

insertEnv:: Either String TypeEnv->(String, Expr)->Either String TypeEnv
insertEnv (Right env) (str, expr) = 
  if (isLeft exprResult) 
    then Left errorUnboundIdentifier
    else Right (Map.insert str (getRight exprResult) env)
  where 
    exprResult = typeCheck env expr

insertEnv fault _ = fault

-- | (Task 3) This function takes a list of name-expression pairs and a type environment,
-- and adds new name-type bindings to the type environment. You may assume that there
-- are no duplicate names among the input or the ones in `builtins`.
-- This can be done with a single call to `foldl`, with an appropriate helper.
buildTypeEnv :: TypeEnv -> [(String, Expr)] -> Either String TypeEnv
buildTypeEnv env definitions = 
  foldl (insertEnv) (Right env) definitions
 

-- | Sample tests for `If`.
test_IfCorrect =
    runTypeCheck (JustExpr $ If (BoolLiteral True) (IntLiteral 3) (IntLiteral 4)) ==
    Right Int_

test_IfBadCondition =
    runTypeCheck (JustExpr $ If (IntLiteral 10) (IntLiteral 3) (IntLiteral 4)) ==
    Left errorIfCondition

test_IfBadBranches =
    runTypeCheck (JustExpr $ If (BoolLiteral True) (IntLiteral 3) (BoolLiteral False)) ==
    Left errorIfBranches

-- Propagate error upwards.
test_IfSubExprError =
    runTypeCheck (JustExpr $
                    If (BoolLiteral True)
                       (IntLiteral 3)
                       (If (IntLiteral 10) (IntLiteral 3) (IntLiteral 4))) ==
    -- Note that the error comes from the condition `IntLiteral 10` in the inner `If`.
    Left errorIfCondition


-- | Sample tests for `Call`.
test_CallCorrect =
    runTypeCheck (JustExpr $
        Call (Identifier "<") [IntLiteral 10, IntLiteral 20]) ==
    Right Bool_

test_CallNotAFunction =
    runTypeCheck (JustExpr $
        Call (BoolLiteral True) [IntLiteral 10, IntLiteral 20]) ==
    Left errorCallNotAFunction

test_CallWrongArgNumber =
    runTypeCheck (JustExpr $
        Call (Identifier "remainder") [IntLiteral 10]) ==
    Left errorCallWrongArgNumber

test_CallWrongArgType =
    runTypeCheck (JustExpr $
        Call (Identifier "or") [BoolLiteral True, IntLiteral 10]) ==
    Left errorCallWrongArgType

test_DefineOne =
    runTypeCheck (WithDefines
        [("x", BoolLiteral True)]
        (Identifier "x")) ==
    Right Bool_

test_DefineTwo =
    runTypeCheck (WithDefines
        [ ("x", IntLiteral 10)
        , ("y", Call (Identifier "<") [Identifier "x", IntLiteral 3])]
        (If (Identifier "y") (Identifier "x") (IntLiteral 3))) ==
    Right Int_


main :: IO ()
main = do
    quickCheck test_IfCorrect
    quickCheck test_IfBadCondition
    quickCheck test_IfBadBranches
    quickCheck test_IfSubExprError
    quickCheck test_CallCorrect
    quickCheck test_CallNotAFunction
    quickCheck test_CallWrongArgNumber
    quickCheck test_CallWrongArgType
    quickCheck test_DefineOne
    quickCheck test_DefineTwo

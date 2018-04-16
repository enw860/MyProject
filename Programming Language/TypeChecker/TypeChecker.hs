{- CSC324 Winter 2018: Assignment 2

*Before starting, please review the general assignment guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*

Note: See the Exercise 9 starter code for some additional introductory comments.
We omitted them here so that we could draw your attention to the new parts not
found on the exercise.
-}

-- The module definition line, including exports. Don't change this!
module TypeChecker (runTypeCheck,
                    Prog(..),
                    Expr(..),
                    Type(..),
                    TypeCheckResult) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- The following are imported for your convenience. You may or may not
-- find these useful (consult the relevant documentation); if you don't
-- use them, please remove them.
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Either (isLeft)
-------------------------------------------------------------------------------

-- |
-- = Data types
--

data Prog
    = JustExpr Expr
    | WithDefines [(String, Expr)] Expr
    deriving Show

data Expr = IntLiteral Int
          | BoolLiteral Bool
          | Identifier String
          | If Expr Expr Expr
          | Call Expr [Expr]
          -- NEW (not on Exercise 9). The first argument is a list of parameter names,
          -- and the second argument is a body expression.
          | Lambda [String] Expr
          deriving (Show, Eq)

data Type
    = Bool_
    | Int_
    | Function [Type] Type
    | TypeVar String
    deriving (Show, Eq, Ord)

-- | Helper for generating type variables for function parameters.
nameToTypeVar :: String -> Type
nameToTypeVar label = TypeVar $ "tvar_" ++ label

type TypeEnv = Map.Map String Type

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

    -- NEW: built-in polymorphic functions. The type parameters here are given unique
    -- names; see handout for discussion of name collisions among type parameters.
    , ("return-zero", Function [TypeVar "t1"] Int_)
    , ("identity", Function [TypeVar "t2"] (TypeVar "t2"))
    , ("apply", Function [Function [TypeVar "t3"] (TypeVar "t4"), TypeVar "t3"] (TypeVar "t4"))
    , ("return-it", Function [TypeVar "t5"] (Function [TypeVar "t6"] (TypeVar "t5")))
    ]


type TypeCheckResult = Either String Type

-- Expected error messages. DON'T CHANGE THESE!
errorIfBranches = "Type error: the two branches of an `if` must have the same type."
errorIfCondition = "Type error: the condition of an `if` must be boolean."
errorCallNotAFunction = "Type error: first expression in a function call must be a function."
errorCallWrongArgNumber = "Type error: function called with the wrong number of arguments."
errorCallWrongArgType = "Type error: function called with an argument of the wrong type."
errorUnboundIdentifier = "Error: unbound identifier."
errorTypeUnification = "Type error: inconsistent set of type constraints generated during type inference."
---------------------------------------------------------------
--test two types can be potentially equal
typeCompares::(Type,Type)-> Bool
typeCompares ((TypeVar _), _) = True
typeCompares (_, (TypeVar _)) = True
typeCompares ((Function arg1 ret1), (Function arg2 ret2)) = 
  and [isSameLength, isSameRetType, isSameArgType]
  where
    isSameLength = (length arg1)==(length arg2)
    isSameRetType = typeCompares (ret1, ret2)
    isSameArgType = not (elem False (map (\val->(typeCompares val)) $ zip arg1 arg2))
typeCompares ((Function _ ret), t2) = typeCompares(ret,t2)
typeCompares (t1, (Function _ ret)) = typeCompares(t1,ret)
typeCompares (t1, t2) = (t1==t2)

--convert a Either type to a Maybe Type
eitherToMaybe:: TypeCheckResult-> Maybe Type
eitherToMaybe tar = 
  case tar of
    (Right t) -> (Just t)
    (Left _) -> Nothing

--starch out a array of TypeCheckResult and turn it into a Maybe Type array
gatherArgs:: [TypeCheckResult] -> Maybe [Type]
gatherArgs arr = 
  foldl func (Just []) (reverse arr)
  where
    func acc val = 
      acc >>= \a ->
      (eitherToMaybe val) >>= \b ->
        return (b:a)

--remove return value in a set of constrains
removeRet:: TypeConstraints -> TypeConstraints
removeRet cons = 
  Set.filter isRet cons
  where
    isRet ((TypeVar "ret"), _) = False
    isRet _ = True
-------------------------------------------------------------------------------
-- | Entry point to the type-checking. We've implemented this for you (though you will
-- probably need to change over the course of the assignment).
runTypeCheck :: Prog -> TypeCheckResult
runTypeCheck (JustExpr expr) = typeCheck builtins expr
runTypeCheck (WithDefines definitions expr) =
    case buildTypeEnv builtins definitions of
        Left msg -> Left msg
        Right newEnv -> typeCheck newEnv expr

-- | The "core" type-checking function.
typeCheck :: TypeEnv -> Expr -> TypeCheckResult
typeCheck _ (IntLiteral _) = Right Int_
typeCheck _ (BoolLiteral _) = Right Bool_
typeCheck env (Identifier s) =
    case Map.lookup s env of
        Nothing -> Left errorUnboundIdentifier
        Just t -> Right t

typeCheck env (If c t e) = 
  if (not isValidCondition) then Left errorIfCondition else
    if (isLeft tType) then tType else
      if (isLeft eType) then eType else 
        case retValue of
          Nothing -> Left errorIfBranches
          Just t -> Right t 
  where
    isValidCondition = (typeCheck env c) == Right Bool_

    tType = typeCheck env t
    eType = typeCheck env e
  
    retValue = 
      (eitherToMaybe tType) >>= \cond1 ->
      (eitherToMaybe eType) >>= \cond2 ->
      (unify cond1 cond2) >>= \rawCons ->
      (consolidate rawCons) >>= \cons ->
      if(typeCompares ((resolve cons cond1), (resolve cons cond2)))
        then return (resolve cons cond1)
        else Nothing

typeCheck env (Call f args) = 
  if isNotFunction then Left errorCallNotAFunction else
    if isArgNumNotMatch then Left errorCallWrongArgNumber else
      if isUnboundedIdentifier then Left errorUnboundIdentifier else
        if isArgTypeNotMatch then Left errorCallWrongArgType else
          case finalRet of
            (Just x) -> Right x
            Nothing -> Left errorCallWrongArgType
  where
    func = typeCheck env f

    --checking if input f is a valid function
    isFunction (Right (Function _ _)) = True
    isFunction _ = False
    isNotFunction = not (isFunction func)

    --checking if number of arguments match with the exception of input function  
    getFunArgNumb (Right (Function l _)) = length l
    getFunArgNumb _ = 0
    isArgNumNotMatch = not ((getFunArgNumb func) == (length args))

    --checking if all input arguments are bounded identifier 
    getFunArgType (Right (Function args _)) = args
    getFunArgType _ = []
    fArgType = getFunArgType func
    replacedArgs = map (\x->(typeCheck env x)) args
    isUnboundedIdentifier = elem True (map isLeft (func:replacedArgs))

    --checking could input arguments types matching with the expected types of function arguments
    evalutedArgs = gatherArgs replacedArgs
    isArgTypeNotMatch = (Just True) ==
      (evalutedArgs >>= \x ->
        return(elem False (map typeCompares $ zip fArgType x)))
    
    --get expected function return type
    getReturnType (Function _ ret) = ret
    returnType = func >>= \fType -> return(getReturnType fType)

    --make a comparator type to the input function type
    makeType (Function _ ret) newArgs = (Function newArgs ret)
    newType = 
      (eitherToMaybe func) >>= \fType -> 
      evalutedArgs >>= \newargs ->
        return(makeType fType newargs)

    --solve for unknown type and find out the return value
    finalRet = 
      (eitherToMaybe returnType) >>= \target ->
      (eitherToMaybe func) >>= \typeA ->
      newType >>= \typeB ->
      unify typeA typeB >>= \allCons -> 
        (consolidate allCons) >>= \solvedCons -> 
          return (resolve solvedCons target)

typeCheck env (Lambda names body) = 
  if ((length names)== 0) 
    then 
      case typeCheck updateEnv body of
        Left l -> Left l
        Right r -> Right (Function [] r)
    else 
      case finalType of
        Just t -> Right t
        Nothing -> 
          case (body) of
            (Call _ _) -> Left errorCallWrongArgType
            (If _ _ _) -> Left errorIfBranches
            _ -> Left errorUnboundIdentifier
  where
    --convert and add all new variables types as TypeVar to the environment 
    nameType = map nameToTypeVar names
    updateEnv = foldl (\arr x->(Map.insert x (nameToTypeVar x) arr)) env names
    
    --build constrains for body with new environment 
    --which is a Call expression + return value of inner function call
    constructType newEnv (Call f args) = 
      evalutedArgs >>= \convertedArgs -> 
      evalutedRet >>= \func@(Function midArgs retType) ->
      functionSet >>= \funSet-> argsSet >>= \argSet->
      if(compareTypes convertedArgs midArgs) 
        then  unify (Function convertedArgs retType) func >>= \cons1 ->
              unify (TypeVar "ret") retType >>= \cons2 ->
                (consolidate (Set.unions [(removeRet cons1), cons2, (removeRet funSet), argSet]))
        else Nothing --case of wrong type of arguments
      where
        --return types for each arguments
        argsType = map (\x->(typeCheck newEnv x)) args
        evalutedArgs = gatherArgs argsType

        --return type for returning function call
        evalutedRet = eitherToMaybe (typeCheck newEnv f)

        -- checking if all pairs of types in given two arrays can potentially matched
        compareTypes arr1 arr2 = not (elem False (map typeCompares $zip arr1 arr2))

        --array of constrains sets for each arguments
        argsSetArr = map (\x->constructType newEnv x) args
        --constrains set for function call
        functionSet = constructType newEnv f
        
        --union all arguments sets and function set(removed all return value for each set)
        rmRetUnion acc val = 
          acc >>= \a-> 
          val >>= \b-> 
            return(Set.union a (removeRet b))
        argsSet = foldl rmRetUnion (Just (Set.fromList [])) argsSetArr

    --build constrains for body with new environment which is a If expression
    constructType newEnv (If c t e) = 
      returnSet >>= \cons1 ->
      returnType >>= \t ->
      unify (TypeVar "ret") t >>= \cons2 ->
        (consolidate (Set.unions [cons1, cons2]))
      where 
        eExpression = typeCheck newEnv e
        
        returnSet = 
          (constructType newEnv c) >>= \cCons -> 
          (constructType newEnv t) >>= \tCons ->
          (constructType newEnv e) >>= \eCons ->
          unify (resolve cCons (TypeVar "ret")) Bool_ >>= \retRetCons ->
          unify (resolve tCons (TypeVar "ret")) (resolve eCons (TypeVar "ret")) >>= \teCons ->
            (consolidate (Set.unions [(removeRet retRetCons),(removeRet teCons),
              (removeRet cCons),(removeRet eCons),(removeRet tCons)]))
        
        returnType = 
          (eitherToMaybe eExpression) >>= \expr ->
          returnSet >>= \cons -> 
            return(resolve cons expr)


    --build constrains for body with new environment which is a Lambda expression
    constructType newEnv input@(Lambda newNames expr) = 
      (constructType newEnv2 expr) >>= \previousCons ->
      returnType >>= \ret ->
      unify (TypeVar "ret") ret >>= \newCons ->
        (consolidate (Set.unions [newCons, (removeRet previousCons)]))
      where
        --build new TypeEnv
        newNameType = map nameToTypeVar newNames
        newEnv2 = foldl (\acc x->(Map.insert x (nameToTypeVar x) acc)) newEnv newNames
        
        --gather old return type
        newEncap = Function newNameType (TypeVar "ret")
        returnType = 
          (constructType newEnv2 expr) >>= \cons -> 
            return (resolve cons newEncap)
    
    --build constrains for body with new environment which is 
    --Identifier, BoolLiteral, IntLiteral expression
    constructType newEnv input = 
      eitherToMaybe (typeCheck newEnv input) >>= \func -> 
      unify (TypeVar "ret") func >>= \cons1 ->
        (consolidate cons1)
    
    --make a comparator with return type TypeVar 'ret'
    encapType = (Function nameType (TypeVar "ret"))
    
    --Since ret not follow regular nameToTypeVar format, 
    --and also not conflict with TypeVar t1-t6, and each 
    --set of constrain can only contains one ret value. 
    --Thus, ret is unique and representative for the function return Type.
    finalType = 
      (constructType updateEnv body) >>= \consSet ->
        return(resolve consSet encapType)

buildTypeEnv :: TypeEnv -> [(String, Expr)] -> Either String TypeEnv
buildTypeEnv env definitions = 
  foldl (insertEnv) (Right env) definitions
  where 
    --bind and insert a string to an expression into environment only if 
    --expression is valid
    insertEnv (Right env) (str, expr) = 
      if (isLeft exprResult) 
        then Left errorUnboundIdentifier
        else exprResult >>= \er ->
          Right (Map.insert str er env)
      where 
        exprResult = typeCheck env expr
    insertEnv fault _ = fault


-------------------------------------------------------------------------------

{- |
== Type Unification
This section contains more information about the implementation of
type unification required to handle polymorphic types.

This section is organized into four parts:
  - The type definitions.
  - The function `unify` for generating type constraints.
  - The function `consolidate` for taking a set of constraints and processing them,
    including looking for errors.
  - The function `resolve`, used to take a type variable and determine what type
    it should take on, given some type constraints.

The three functions can be used to type-check generic function calls in the following way
(given in pseudocode):
  1. Check that the number of arguments matches the number of params of the function.
  2. `unify` each argument type with its corresponding parameter type, and accumulate constraints.
  3. `consolidate` all constraints (checks for errors).
  4. `resolve` the return type using the consolidated constraints.

As we've discussed in the handout, you may freely change the given types and bits of
implementation provided; just make sure you document your work carefully.
-}

-- | This is a type synonym. The tuple represents an *equivalence* between
-- two types; for example, "a is an Int" is represented as (TypeVar "a", Int_).
-- Recommended invariant: the first type is always a type variable.
-- It'll be up to you to enforce this; the compiler won't catch it!
type TypeConstraint = (Type, Type)
-- | A set of type constraints. Used to accumulate multiple constraints.
type TypeConstraints = Set.Set TypeConstraint

-- You should choose a different name representation for this type, or remove it entirely.
type ConsolidatedConstraints = TypeConstraints

unionConstrains:: Maybe TypeConstraints -> Maybe TypeConstraints -> Maybe TypeConstraints
unionConstrains acc val = 
  acc >>= \a ->
  val >>= \b ->
    return(Set.union a b)

--check if a constrain is valid
isValidConstrain:: TypeConstraint -> Bool
isValidConstrain (TypeVar t1, t2) = True
isValidConstrain (t1, TypeVar t2) = True
isValidConstrain (t1, t2) = (t1==t2)

--filter directed solved type bindings
dFilteSort:: [TypeConstraint] -> TypeConstraint -> [TypeConstraint]
dFilteSort arr (TypeVar _, TypeVar _) = arr
dFilteSort arr t@(TypeVar _, _) = t:arr
dFilteSort arr (t1, TypeVar t2) = (TypeVar t2, t1):arr

--checking if a constrain is an indirected type binding
filteRest:: TypeConstraint -> Bool
filteRest (TypeVar _, TypeVar _) = True
filteRest _ = False

--filter indirected type bindings
rFilteSort::[TypeConstraint] -> TypeConstraint -> [TypeConstraint]
rFilteSort arr t@(TypeVar _, _) = t:arr
rFilteSort arr (t1, TypeVar t2) = (TypeVar t2, t1):arr
rFilteSort arr _ = arr

--replace all possible constrains with solved types
solveTypes:: [TypeConstraint]->TypeConstraint->[TypeConstraint]
solveTypes acc ((TypeVar x), actType) = 
  map (\(a,b)->((replaceType a), (replaceType b))) acc
    where
      replaceType (TypeVar y) = if (y==x) then actType else (TypeVar y)
      replaceType y = y 

-- | This is the main unification function.
-- It takes two types (possibly type variables) and succeeds if the types unify,
-- and fails otherwise.
--
-- Formally, we say two types t1 and t2 *unify* if:
--   - t1 or t2 is a type variable.
--       In this case, return a single type constraint (t1, t2).
--   - t1 and t2 are the same primitive type (Int_ or Bool_).
--       In this case, return no type constraints.
--   - t1 and t2 are both function types that take the same number of parameters,
--     *and* their corresponding parameter types unify, *and* their return types unify.
--       In this case, return all the type constraints from unifying the parameters
--       and return types.
unify :: Type -> Type -> Maybe TypeConstraints
unify (TypeVar t1) t2 = Just (Set.fromList [((TypeVar t1), t2)])
unify t1 (TypeVar t2) = Just (Set.fromList [((TypeVar t2), t1)])
unify (Function paramType1 retType1) (Function paramType2 retType2) = 
  if(not isParamSameLen) then Nothing else mergeUnis
  where
    --checking if two functions' arguments have same length
    isParamSameLen = (length paramType1) == (length paramType2)
    
    --union all constrains that arguments types and return type have
    paramTypeUni = (map (\(x, y)->(unify x y)) $zip paramType1 paramType2)
    retTypesUni = (unify retType1 retType2)
    mergeUnis = foldr unionConstrains retTypesUni paramTypeUni
unify (Function _ _) _ = Nothing
unify _ (Function _ _) = Nothing
unify _ _ = Just (Set.fromList [])

-- | Takes the generated constraints and processs them.
-- Returns `Nothing` if the constraints cannot be satisfied (this is a type error).
-- Note that the returned value will depend on your representation of `ConstraintSets`.
consolidate :: TypeConstraints -> Maybe ConsolidatedConstraints
consolidate constraints = 
  if (and [isdConsValid, isrestSelfContradicted])
    then 
      if(unsolvedCountBefore==unsolvedCountAfter)
        then Just solvedCons --constrain cannot be further solved
        else consolidate solvedCons --continue solving constrains 
    else Nothing
  where
    cons = Set.toList constraints

    --filter out directly type bindings
    dCons = foldl dFilteSort [] cons   

    --checking if a TypeVar binds only one type
    dProduct = [(x,y) | x<-dCons, y<-dCons]
    isdConsValid = foldl (\acc ((a,b),(c,d))-> (if(and [a==c, (not (b==d))]) then False else acc)) True dProduct

    --replace directly value into unsolved constrains
    restTypeReplace = foldl solveTypes cons dCons

    --checking whether a constrain binds two different solved types
    isrestSelfContradicted = not (elem False (map isValidConstrain restTypeReplace))

    --filter out all constrains with TypeVar in restTypeReplace
    rCons = foldl rFilteSort [] restTypeReplace

    --number of unsolved TypeVar(binds two TypeVars) count before and after solve for constrains
    unsolvedCountBefore = (length (filter filteRest cons))
    unsolvedCountAfter = (length (filter filteRest rCons))
    
    --remove duplications
    solvedCons = Set.union (Set.fromList dCons) (Set.fromList rCons)

-- | Takes the consolidated constraints and a type, and returns a
-- new type obtained by replacing any type variables in the input type
-- based on the given constraints.
-- Note: if there are no constraints on a type variable, you should
-- simply return the type variable (it remains generic).
resolve :: ConsolidatedConstraints -> Type -> Type
resolve _ Int_ = Int_
resolve _ Bool_ = Bool_
--search for input Type, if found then return a solved type, otherwise return search type itself
resolve constraints t@(TypeVar x) = foldl search t allCons
  where 
    allCons = Set.toList constraints
    search acc (TypeVar a, b) = if(x==a) then b else acc
--search for input function types, solve as many as solvable types in given function type
resolve constraints (Function params rType) = 
  (Function paraTypes returnType)
  where 
    allcons = Set.toList constraints
    paraTypes = map (\x->resolve constraints x) params
    returnType = resolve constraints rType

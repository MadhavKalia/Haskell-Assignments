{-|
Module      : 1JC3-Assign4.Assign_4.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 4 - McMaster CS 1JC3 2021
-}
module Assign_4 where

import Test.QuickCheck

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Madhav Kalia
-- Date: December 12th, 2021
macid :: String
macid = "kaliam1"

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                (abs (2*X + 1)) ^ 3
 -          can be encoded as
 -                Power 3 (Func1 Abs
 -                              (Func2 Add (Func2 Mult (Coef 2) X)
 -                                         (Coef 1)))
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Func1 UnaryOp (MathExpr a)
  | Func2 BinOp (MathExpr a) (MathExpr a)
  deriving (Eq,Show,Read)

data BinOp = Add | Mult
  deriving (Show,Eq,Read)

data UnaryOp = Cos | Sin | Abs | Power Int
  deriving (Show,Eq,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
 -    eval takes in a MathExpr a and float. It then  evaluates the expression 
      at v, by subbing in v for X
 -}

eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval X v                = v
eval (Coef c) v         = c
eval (Func1 op e) v     =
  case op of
    Power n -> eval e v ^^ n
    Cos     -> cos (eval e v)
    Sin     -> sin (eval e v)
    Abs     -> abs (eval e v)
eval (Func2 op e0 e1) v =
  case op of
    Add  -> eval e0 v + eval e1 v
    Mult -> eval e0 v * eval e1 v


{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    instance Num a takes in Num a and implements the operations
      needed for MathExpr a
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Func2 Add x y
  x * y         = Func2 Mult x y
  negate x      = Func2 Mult x (Coef (-1))
  abs x         = Func1 Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    instance Fractional a implements methods required for MathExpr a
 -}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Func1 (Power (-1)) e
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    instance Floating a implements the methods required for MathExpr a
      which are pi, sin and cos and leaves the rest un-implemented
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin     = Func1 Sin
  cos     = Func1 Cos
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
 -    diff takes in a MathExpr a and differentiates the equation 
      by pattern matching to see what rules need to be followed for 
      each type of expression
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X                    = Coef 1
diff (Coef r)             = Coef 0
diff (Func2 Add u v)      = diff u + diff v
diff (Func2 Mult u v)     = diff u * v + u * diff v
diff (Func1 (Power n) u)  = fromInteger (fromIntegral n) * Func1 (Power (n-1)) u * diff u
diff (Func1 Cos u)        = -1 * Func1 Sin u * diff u
diff (Func1 Sin u)        = Func1 Cos u * diff u
diff (Func1 Abs u)        = u / Func1 Abs u * diff u


{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description:
 -    pretty takes in a MathExpr a and returns the string representation of 
      the expression with subexpressions u and v by pattern matching to see 
      what type of expression it is.
 -}
pretty :: (Show a) => MathExpr a -> String
pretty X                    = "X"
pretty (Coef c)             = show c
pretty (Func2 Add u v)      = "(" ++ pretty u ++ "+" ++ pretty v ++ ")"
pretty (Func2 Mult u v)     = "(" ++ pretty u ++ "*" ++ pretty v ++ ")"
pretty (Func1 (Power u) v)  = "(" ++ pretty v ++ "^^" ++ show u ++ ")"
pretty (Func1 Cos u)        = "cos(" ++ pretty u ++ ")"
pretty (Func1 Sin u)        = "sin(" ++ pretty u ++ ")"
pretty (Func1 Abs u)        = "abs(" ++ pretty u ++ ")"

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}
infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

{- EXAMPLE
 - Function: eval
 - Property: eval (Func2 Add (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}
evalProp0 :: (Float,Float) -> Bool
evalProp0 (x,y) = (x + y) =~ eval (Func2 Add (Coef x) X) y

runEvalProp0 :: IO ()
runEvalProp0 = quickCheck  evalProp0

-- TODO add more quickcheck test cases here

{-
 - Function: eval
 - Property: eval (Func1 Abs (Coef x) ) x is correct for all x
 - Actual Test Result: Pass
 -}
evalProp1 :: Float -> Bool
evalProp1 x = abs x =~ eval (Func1 Abs (Coef x) ) x

runEvalProp1 :: IO ()
runEvalProp1 = quickCheck  evalProp1

{-
 - Function: diff
 - Property: (diff (Func1 Cos X)) x  is correct for all x
 - Actual Test Result: Pass
 -}
diffProp1 :: Float -> Bool
diffProp1 x = -sin x =~ eval (diff (Func1 Cos X)) x

runDiffProp1 :: IO ()
runDiffProp1 = quickCheck diffProp1

{- Test Plan

Function         : eval
Test case Number : 1
Input            : eval (Func2 Add (Coef (1.5)) X) (3.5)
Expected Output  : 5.0
Acutal Output    : 5.0
Function         : eval
Test case Number : 2
Input            : eval (Func2 Mult (Coef (2.5)) X) (2.5)
Expected Output  : 6.25
Acutal Output    : 6.25
Function         : eval
Test case Number : 3
Input            : eval (Func2 Add (Func2 Mult (Coef (1.5)) X) (Func2 Add (Coef (2.4)) X)) (5.0)
Expected Output  : 14.9
Acutal Output    : 14.9

Function         : diff
Test case Number : 1
Input            : diff (Func1 Abs X)    
Expected Output  : Func2 Mult (Func2 Mult X (Func1 (Power (-1)) (Func1 Abs X))) (Coef 1.0)
Acutal Output    : Func2 Mult (Func2 Mult X (Func1 (Power (-1)) (Func1 Abs X))) (Coef 1.0)
Function         : diff
Test case Number : 2
Input            : diff (Func2 Add (Func2 Mult (Coef (1.5)) X) (Func2 Add (Coef (2.4)) X))
Expected Output  : Func2 Add (Func2 Add (Func2 Mult (Coef 0.0) X) (Func2 Mult (Coef 1.5) (Coef 1.0))) (Func2 Add (Coef 0.0) (Coef 1.0))
Acutal Output    : Func2 Add (Func2 Add (Func2 Mult (Coef 0.0) X) (Func2 Mult (Coef 1.5) (Coef 1.0))) (Func2 Add (Coef 0.0) (Coef 1.0))
Function         : diff
Test case Number : 3
Input            : diff (Func1 Abs (Func2 Add (Func2 Mult (Coef 3) X) (Func1 Sin X)))
Expected Output  : Func2 Mult (Func2 Mult (Func2 Add (Func2 Mult (Coef 3.0) X) (Func1 Sin X)) (Func1 (Power (-1)) (Func1 Abs (Func2 Add (Func2 Mult (Coef 3.0) X) (Func1 Sin X))))) (Func2 Add (Func2 Add (Func2 Mult (Coef 0.0) X) (Func2 Mult (Coef 3.0) (Coef 1.0))) (Func2 Mult (Func1 Cos X) (Coef 1.0)))
Acutal Output    : Func2 Mult (Func2 Mult (Func2 Add (Func2 Mult (Coef 3.0) X) (Func1 Sin X)) (Func1 (Power (-1)) (Func1 Abs (Func2 Add (Func2 Mult (Coef 3.0) X) (Func1 Sin X))))) (Func2 Add (Func2 Add (Func2 Mult (Coef 0.0) X) (Func2 Mult (Coef 3.0) (Coef 1.0))) (Func2 Mult (Func1 Cos X) (Coef 1.0)))

Function         : pretty
Test case Number : 1
Input            : pretty (Func2 Add (Coef (1.5)) X) 
Expected Output  : "(1.5+X)"
Acutal Output    : "(1.5+X)"
Function         : prety
Test case Number : 2
Input            : pretty (Func2 Add  (Func1 (Power 3) X) (Func1 Cos X))
Expected Output  : "((X^^3)+cos(X))"
Acutal Output    : "((X^^3)+cos(X))"
Function         : pretty
Test case Number : 3
Input            : pretty (Func2 Mult (Func2 Mult (Coef 3.5) X) (Func1 Abs X)) 
Expected Output  : "((3.5*X)*abs(X))"
Acutal Output    : "((3.5*X)*abs(X))"

-}
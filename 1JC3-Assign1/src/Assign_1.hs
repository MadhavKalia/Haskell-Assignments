{-|
Module      : 1JC3-Assign1.Assign_1.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 1 - McMaster CS 1JC3 2021
-}
module Assign_1 where

import Prelude hiding (sin,cos,tan)

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
macid :: String
macid = "kaliam1"
--Name : Madhav Kalia
--Date : Oct 3, 2021



{- -----------------------------------------------------------------
 - factorial
 - -----------------------------------------------------------------
 - Description:
 -    Computes the factorial of any Integer n
 - -----------------------------------------------------------------
 - |   Input     |                                                 |
 - |      n      | Integer input                                   |
 - -----------------------------------------------------------------
 - |   Output    |                                                 |
 - |      n <= 1 | 1                                               |
 - |      n >  1 | n * (n-1) ...  * 1   while (n-k) > 0            |
 - -----------------------------------------------------------------
 -}
factorial :: Integer -> Integer
factorial n = if n > 0
              then n * factorial (n-1)
              else 1

{- ------------------------------------------------------------------
 - sinTaylor
 - ------------------------------------------------------------------
 - |    Input     |                                                 |
 - |       a      | Double Input                                    |
   |     cos_a    | Double Input                                    |
   |     sin_a    | Double Input                                    |
   |      x       | Double Input                                    |
 - -----------------------------------------------------------------
 - |   Output                                                     |
 - |     double value: the value of sin function                  |
   |     approximation at x using the taylor series formula       |
 - -----------------------------------------------------------------
 - Description:
 -   Computes 4th Taylor polynomial approximation of sin(x) using a, sin(a), cos(a) and x
     in the equation (sin(x) = (sin(a)/0! * (x-a)**0) + (cos(a)/1! * (x-a)**1) + (-sin(a)/2! * (x-a)**2)
     + (-cos(a)/3! * (x-a)**3) + (sin(a)/4!* (x-a)**4)
 -}
sinTaylor :: Double -> Double -> Double -> Double -> Double
sinTaylor a cos_a sin_a x = sin_a/fromIntegral(factorial 0)*(x-a)**0
                            + cos_a/fromIntegral(factorial 1)*(x-a)**1
                            + (-sin_a/fromIntegral(factorial 2))*(x-a)**2
                            + (-cos_a/fromIntegral(factorial 3))*(x-a)**3
                            + sin_a/fromIntegral(factorial 4)*(x-a)**4

{- -----------------------------------------------------------------
 - fmod
 - -----------------------------------------------------------------
-  |    Input    |                                                 |
 - |       x     | Double Input                                    |
   |       y     | Double Input
 - -----------------------------------------------------------------
 - |   Output                                                      |
 - |     remainder of x / y            | Double Output             |
   |     example: (1.5, 1.0) = 0.5     |                           |
 - -----------------------------------------------------------------
 - Description:
 -   Recreates the mod function by computing the remainder of two doubles x y
 -}
fmod :: Double -> Double -> Double
fmod x y =
  let
    -- z is the largest integer s.t. z*y <= x
    -- HINT use floating point division, then round down
    z = fromIntegral(floor(x/y))
  in x - z*y

{- ----------------------------------------------------------------------
 - sinApprox
 -----------------------------------------------------------------
-  |    Input    |                                                 |
 - |       x     | Double Input                                    |
 - -----------------------------------------------------------------
 - |   Output                                                      |
 - |    approximate sin value at x     | Double Output             |
 - -----------------------------------------------------------------
 - Description:
 -   Computes the sin approximation value of any double x using the sinTaylor function and fmod
 -}
sinApprox :: Double -> Double
sinApprox x
  | fmod x (2*pi) >= 0 && fmod x (2*pi) < (pi/4)  = sinTaylor 0 1 0 (fmod x (2*pi))
  | fmod x (2*pi) >= (pi/4) && fmod x (2*pi) < (3*pi/4)  = sinTaylor (pi/2) 0 1 (fmod x (2*pi))
  | fmod x (2*pi) >= (3*pi/4) && fmod x (2*pi) < (5*pi/4) = sinTaylor pi (-1) 0 (fmod x (2*pi))
  | fmod x (2*pi) >= (5*pi/4) && fmod x (2*pi) < (7*pi/4)  = sinTaylor (3*pi/2) 0 (-1) (fmod x (2*pi))
  | otherwise  = sinTaylor (2*pi) 1 0 (fmod x (2*pi))

{- ---------------------------------------------------------------------
 - cosApprox
 -----------------------------------------------------------------
-  |    Input    |                                                 |
 - |       x     | Double Input                                    |
 - -----------------------------------------------------------------
 - |    Output                                                     |                                                    
 - |    approximate cos value at x     | Double Output             |
 - ---------------------------------------------------------------------
 - Description:
 -   Computes the cosine approximation value of any double x using the sinApprox function by converting
     the sin value into cosine by using the formula
 -}
cosApprox :: Double -> Double
cosApprox x = -1 * sinApprox (x - pi/2)

{- ---------------------------------------------------------------------
 - tanApprox
  -----------------------------------------------------------------
-  |    Input    |                                                 |
 - |       x     | Double Input                                    |
 - -----------------------------------------------------------------
 - |    Output                                                     |                                                    
 - |    approximate tan value at x     | Double Output             |
 - ---------------------------------------------------------------------
 - Description:
 -   Computes the tan approximation value of any double x by using the sinApprox and cosApprox functions
     to solve the tan formula and acheive the tan value
 -}
tanApprox :: Double -> Double
tanApprox x = sinApprox x / cosApprox x

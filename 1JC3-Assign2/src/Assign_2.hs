{-|
Module      : 1JC3-Assign2.Assign_2.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 2 - McMaster CS 1JC3 2021
-}
module Assign_2 where

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
-- Date: Oct 31, 2021
macid :: String
macid = "kaliam1"

type Vector3D = (Double,Double,Double)

{- -----------------------------------------------------------------
 - getX
 - -----------------------------------------------------------------
 - Description:
 -   returns the x-coordinate of the vector
 -}
getX :: Vector3D -> Double
getX (x, y, z) = x

{- -----------------------------------------------------------------
 - getY
 - -----------------------------------------------------------------
 - Description:
 -   returns the y-coordinate of the vector
 -}
getY :: Vector3D -> Double
getY (x, y, z) = y

{- -----------------------------------------------------------------
 - getZ
 - -----------------------------------------------------------------
 - Description:
 -   returns the z-coordinate of the vector
 -}
getZ :: Vector3D -> Double
getZ (x, y, z) = z

{- -----------------------------------------------------------------
 - scalarMult
 - -----------------------------------------------------------------
 - Description:
 -   takes a vector and a constant and multiples that constant to each 
     vector and returns the scalar multiplication of the vector
 -}
scalarMult :: Double -> Vector3D -> Vector3D
scalarMult s (x, y, z) = (x', y', z')
    where
      x' = x*s
      y' = y*s
      z' = z*s

{- -----------------------------------------------------------------
 - add
 - -----------------------------------------------------------------
 - Description:
 -   takes two vectors and returns the sum of them. Adds each vector in 
     with it's respective vector in the second
 -}
add :: Vector3D -> Vector3D -> Vector3D
add (x0, y0, z0) (x1, y1, z1) = (x', y', z')
    where
      x' = x0 + x1
      y' = y0 + y1
      z' = z0 + z1


{- -----------------------------------------------------------------
 - innerProduct
 - -----------------------------------------------------------------
 - Description:
 -    takes two vectors and returns the inner product of the vectors
      which is each vector multiplied by each other and then the product
      of the vectors is added together 
 -}
innerProduct :: Vector3D -> Vector3D -> Double
innerProduct (x0, y0, z0) (x1, y1, z1) = prod_x + prod_y + prod_z
    where
      prod_x = x0 * x1
      prod_y = y0 * y1
      prod_z = z0 * z1

{- -----------------------------------------------------------------
 - distance
 - -----------------------------------------------------------------
 - Description:
 -   takes two vectors and computes the distance between them 
 -}
distance :: Vector3D -> Vector3D -> Double
distance (x0, y0, z0) (x1, y1, z1)  = sqrt (x' + y' + z')
    where
      x' =  (x1 - x0) ** 2
      y' =  (y1 - y0) ** 2
      z' =  (z1 - z0) ** 2


{- ------------------------------------------------------------------------
 - maxDistance
 - ------------------------------------------------------------------------
 - Description:
 -   takes a list of vectors and returns the vector which has the maximum 
     distance from the origin 
 -}
maxDistance :: [Vector3D] -> Vector3D
maxDistance []                                                  = (0, 0, 0)
maxDistance (x:xs) 
  | distance (0, 0, 0) x >  distance (0, 0, 0) (maxDistance xs) = x
  | distance (0, 0, 0) x == distance (0, 0, 0) (maxDistance xs) = x
  | otherwise                                                   = maxDistance xs



{- Test Plan

Function         : scalarMult
Test case Number : 1
Input            : 3 (1,2,3)
Expected Output  : (3,6,9)
Acutal Output    : (3,6,9)
Test case Number : 2
Input            : 2 (3,2,1)
Expected Output  : (6,4,2)
Acutal Output    : (6,4,2)
Test case Number : 3
Input            : 0 (1,2,3)
Expected Output  : (0,0,0)
Acutal Output    : (0,0,0)
----------------------------------------------
Function         : add
Test case Number : 1
Input            : (1,2,3) (1,2,3)
Expected Output  : (2,4,6)
Acutal Output    : (2,4,6)
Test case Number : 2
Input            : (0,0,0) (3,2,1)
Expected Output  : (3,2,1)
Acutal Output    : (3,2,1)
Test case Number : 3
Input            : (-2,2,4) (1,2,3)
Expected Output  : (-1,4,7)
Acutal Output    : (-1,4,7)
---------------------------------------------
Function         : innerProduct
Test case Number : 1
Input            : (1,2,3) (1,2,3)
Expected Output  : 14
Acutal Output    : 14
Test case Number : 2
Input            : (1,1,1) (3,2,1)
Expected Output  : 6
Acutal Output    : 6
Test case Number : 3
Input            : (10,10,-10) (1,2,3)
Expected Output  : 0
Acutal Output    : 0
---------------------------------------------
Function         : distance
Test case Number : 1
Input            : (4,4,4) (4,4,4)
Expected Output  : 0
Acutal Output    : 0
Test case Number : 2
Input            : (-2,2,3) (1,2,5)
Expected Output  : 3.605551275463989
Acutal Output    : 3.605551275463989
Test case Number : 3
Input            : (-1,-2,-3) (1,2,3)
Expected Output  : 7.483314773547883
Acutal Output    : 7.483314773547883
---------------------------------------------
Function         : maxDistance
Test case Number : 1
Input            : ([4,4,4) ,(1,2,3)]
Expected Output  : (4,4,4)
Acutal Output    : (4,4,4)
Test case Number : 2
Input            : ([4,4,4) ,(1,2,3), (42,22,41)]
Expected Output  : (42,22,41)
Acutal Output    : (42,22,41)
Test case Number : 3
Input            : []
Expected Output  : (0,0,0)
Acutal Output    : (0,0,0)

-}


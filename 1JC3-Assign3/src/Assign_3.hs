{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2021
-}
module Assign_3 where

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
-- Date: November 21, 2021
macid :: String
macid = "kaliam1"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}
data Graph a = Graph { getNodes :: [Node a]
                     , getEdges :: [Edge] }
  deriving (Show,Eq)

type Edge = (NodeID,NodeID)
type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq,Ord)


{- -----------------------------------------------------------------
 - Example Graph
 - -----------------------------------------------------------------
 -              -----        ----------
 -              | A |------->| C |    |
 -              -----        ----- <---
 -                |           |
 -                |     ------|
 -                v     |
 -              ----- <-|
 -              | B |
 -              -----
 -}
nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph =
  let
    nodes = [nodeA,nodeB,nodeC]
    edges = [(0,1),(0,2),(2,2),(2,1)]
  in Graph nodes edges

{- -----------------------------------------------------------------
 - maxNodeID
 - -----------------------------------------------------------------
 - Description:
 -    Returns the largest NodeID in a Graph
 -}

maxNodeID :: Graph a -> Maybe NodeID
maxNodeID (Graph [] edge) = Nothing
maxNodeID (Graph xs edge) = Just z
  where
    x = getNodes (Graph xs edge)
    y = map getNodeID x
    z = maximum y


{- -----------------------------------------------------------------
 - insertNode
 - -----------------------------------------------------------------
 - Description:
 -    Inserts a new Node with the given input value into a Graph. NodeID of
      the new Node is the previous max node ID plus 1. It uses an auxillary function
      maxNodeID' which functions the same way as the maxNodeID function but returns 
      an Int instead of Maybe Int.
 -}

insertNode :: a -> Graph a -> Graph a
insertNode v (Graph [] edge) = Graph [x] edge
  where
    x = Node 0 v
insertNode v (Graph xs edge) = Graph (xs ++ [z]) edge
  where
    x = maxNodeID' (Graph xs edge)
    y = x + 1
    z = Node y v

maxNodeID' :: Graph a -> NodeID
maxNodeID' (Graph xs edge) = z
  where
    x = getNodes (Graph xs edge)
    y = map getNodeID x
    z = maximum y

{- -----------------------------------------------------------------
 - removeNode
 - -----------------------------------------------------------------
 - Description:
 -    Removes the Node from a Graph that has the given coressponding NodeID
      using list comprehension. A new list of nodes is made that consists of
      all the previous nodes except for the one that is being removed. The edges
      also use list comprehension, which consists of all the previous edges besides
      the one that is being removed
 -}

removeNode :: NodeID -> Graph a -> Graph a
removeNode nID (Graph xs edge) = Graph xs' edge'
  where
    xs'   = [x | x <- getNodes (Graph xs edge), getNodeID x /= nID]
    edge' = [(x, y) | (x, y) <- getEdges (Graph xs edge), x /=nID, y/=nID]

{- -----------------------------------------------------------------
 - lookupNode
 - -----------------------------------------------------------------
 - Description:
 -    Returns the Node with the given coressponding NodeID using list comprehension
      if the list is emoty that means the NodeID does not exist in the Graph, otherwise
      the head of the list is return if the NodeID exists in the list, which is the Node 
 -}
lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID (Graph xs edge)
  | null ([x | x <- y, getNodeID x == nID]) = Nothing
  | otherwise                               = Just (head [x | x <- y, getNodeID x == nID])
    where
      y = getNodes (Graph xs edge)

{- -----------------------------------------------------------------
 - insertEdge
 - -----------------------------------------------------------------
 - Description:
 -   Inserts an edge from the Node with the given coressponding NodeID
     to the first part of the tuple with the given coressponding NodeID 
     in the second part of the tuple. Uses an auxillary function lookupNodeID'
     which functions the same way as lookupNode but instead returns a Bool.
     It returns False if the node is not in the Graph and True if it is. 
 -}
insertEdge :: Eq a => (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge _       (Graph [] _)  = Nothing
insertEdge (n1,n2) (Graph ns es)
  | not containsBothNodes = Nothing
  | containsEdge          = Just (Graph ns es)
  | otherwise             = Just (Graph ns es')
  where
    es' = getEdges (Graph ns es) ++ [(n1, n2)]
    containsBothNodes :: Bool
    containsBothNodes
      | lookupNode' n1 (Graph ns es) && lookupNode' n2 (Graph ns es) = True
      | otherwise                                                    = False
    containsEdge :: Bool
    containsEdge
      | null ([(x, y) | (x, y) <- getEdges (Graph ns es), (x, y) == (n1, n2)]) = False
      | otherwise                                                              = True
    lookupNode' :: NodeID -> Graph a -> Bool
    lookupNode' nID (Graph xs edge)
      | null ([x | x <- y, getNodeID x == nID]) = False
      | otherwise                               = True
        where
          y = getNodes (Graph xs edge)

{- Test Plan

Function         : maxNodeID
Test case Number : 1
Input            : (Graph [Node 2 'C', Node 3 'G', Node 1 'R'] [(2,1),(3,1),(2,3)])
Expected Output  : Just 3
Acutal Output    : Just 3
Function         : maxNodeID
Test case Number : 2
Input            : (Graph [Node 10 'C', Node 11 'G', Node 100 'R'] [(10,100),(11,100),(10,11)])
Expected Output  : Just 100
Acutal Output    : Just 100
Function         : maxNodeID
Test case Number : 3
Input            : (Graph [Node 1230 'C', Node 121 'G', Node 1020 'R'] [(1230,1020),(121,1020),(1230,121)])
Function         : maxNodeID
Expected Output  : Just 1230
Acutal Output    : Just 1230

Function         : insertNode
Test case Number : 1
Input            : 'A' (Graph [Node 11 'C', Node 12 'G', Node 101 'R'] [(11,100),(12,101),(11,12)])
Expected Output  : Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'C'}, Node {getNodeID = 12, getNodeVal = 'G'}, Node {getNodeID = 101, getNodeVal = 'R'}, Node {getNodeID = 102, getNodeVal = 'A'}], getEdges = [(11,100),(12,101),(11,12)]}
Acutal Output    : Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'C'}, Node {getNodeID = 12, getNodeVal = 'G'}, Node {getNodeID = 101, getNodeVal = 'R'}, Node {getNodeID = 102, getNodeVal = 'A'}], getEdges = [(11,100),(12,101),(11,12)]}
Function         : insertNode
Test case Number : 2
Input            : 'G' (Graph [] [])
Expected Output  : Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 'G'}], getEdges = []}
Acutal Output    : Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 'G'}], getEdges = []}
Function         : insertNode
Test case Number : 3
Input            : 'I' (Graph [Node 2 'C', Node 3 'G', Node 1 'R'] [(2,1),(3,1),(2,3)])
Expected Output  : Graph {getNodes = [Node {getNodeID = 2, getNodeVal = 'C'}, Node {getNodeID = 3, getNodeVal = 'G'}, Node {getNodeID = 1, getNodeVal = 'R'}, Node {getNodeID = 4, getNodeVal = 'I'}], getEdges = [(2,1),(3,1),(2,3)]}
Acutal Output    : Graph {getNodes = [Node {getNodeID = 2, getNodeVal = 'C'}, Node {getNodeID = 3, getNodeVal = 'G'}, Node {getNodeID = 1, getNodeVal = 'R'}, Node {getNodeID = 4, getNodeVal = 'I'}], getEdges = [(2,1),(3,1),(2,3)]}

Function         : removeNode
Test case Number : 1
Input            : 3 (Graph [Node 2 'C', Node 3 'G', Node 1 'R'] [(2,1),(3,1),(2,3)])
Expected Output  : Graph {getNodes = [Node {getNodeID = 2, getNodeVal = 'C'}, Node {getNodeID = 1, getNodeVal = 'R'}], getEdges = [(2,1)]}
Acutal Output    : Graph {getNodes = [Node {getNodeID = 2, getNodeVal = 'C'}, Node {getNodeID = 1, getNodeVal = 'R'}], getEdges = [(2,1)]}
Function         : removeNode
Test case Number : 2
Input            : 5 (Graph [Node 10 'K', Node 45 'G', Node 4 'R'] [(10,4),(45,4),(10,45)])
Expected Output  : Graph {getNodes = [Node {getNodeID = 10, getNodeVal = 'K'}, Node {getNodeID = 45, getNodeVal = 'G'}, Node {getNodeID = 4, getNodeVal = 'R'}], getEdges = [(10,4),(45,4),(10,45)]}
Acutal Output    : Graph {getNodes = [Node {getNodeID = 10, getNodeVal = 'K'}, Node {getNodeID = 45, getNodeVal = 'G'}, Node {getNodeID = 4, getNodeVal = 'R'}], getEdges = [(10,4),(45,4),(10,45)]}
Function         : removeNode
Test case Number : 3
Input            : 0 (Graph [] [])
Expected Output  : Graph {getNodes = [], getEdges = []}
Acutal Output    : Graph {getNodes = [], getEdges = []}

Function         : lookupNode
Test case Number : 1
Input            : 5 (Graph [Node 3 'C', Node 7 'R'] [(3,7)])
Expected Output  : Nothing
Acutal Output    : Nothing
Function         : lookupNode
Test case Number : 2
Input            : 4 (Graph [Node 20 'K', Node 4 'R', Node 2 'G'] [(20,2),(4,2),(20,4)])
Expected Output  : Just (Node {getNodeID = 4, getNodeVal = 'R'})
Acutal Output    : Just (Node {getNodeID = 4, getNodeVal = 'R'})
Function         : lookupNode
Test case Number : 3
Input            : 10 (Graph [Node 10 'J', Node 5 'G', Node 3 'K'] [(10,3),(5,3),(10,5)])
Expected Output  : Just (Node {getNodeID = 10, getNodeVal = 'J'})
Acutal Output    : Just (Node {getNodeID = 10, getNodeVal = 'J'})

Function         : insertEdge
Test case Number : 1
Input            : (4,5) (Graph [Node 2 'C', Node 3 'G', Node 1 'R'] [(2,1),(3,1),(2,3)])
Expected Output  : Nothing
Acutal Output    : Nothing
Function         : insertEdge
Test case Number : 2
Input            : (2,2) (Graph [Node 2 'A', Node 3 'B', Node 1 'C'] [(2,1),(3,1),(2,3)])
Expected Output  : Just (Graph {getNodes = [Node {getNodeID = 2, getNodeVal = 'A'}, Node {getNodeID = 3, getNodeVal = 'B'}, Node {getNodeID = 1, getNodeVal = 'C'}], getEdges = [(2,1),(3,1),(2,3),(2,2)]})
Acutal Output    : Just (Graph {getNodes = [Node {getNodeID = 2, getNodeVal = 'A'}, Node {getNodeID = 3, getNodeVal = 'B'}, Node {getNodeID = 1, getNodeVal = 'C'}], getEdges = [(2,1),(3,1),(2,3),(2,2)]})
Function         : insertEdge
Test case Number : 3
Input            : (1,10) (Graph [Node 10 'L', Node 12 'O', Node 1 'P'] [(10,1),(12,1),(10,12)])
Expected Output  : Just (Graph {getNodes = [Node {getNodeID = 10, getNodeVal = 'L'}, Node {getNodeID = 12, getNodeVal = 'O'}, Node {getNodeID = 1, getNodeVal = 'P'}], getEdges = [(10,1),(12,1),(10,12),(1,10)]})
Acutal Output    : Just (Graph {getNodes = [Node {getNodeID = 10, getNodeVal = 'L'}, Node {getNodeID = 12, getNodeVal = 'O'}, Node {getNodeID = 1, getNodeVal = 'P'}], getEdges = [(10,1),(12,1),(10,12),(1,10)]})

-}
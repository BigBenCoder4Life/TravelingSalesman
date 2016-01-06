--
-- Author: Ben Kirtley
-- Date: Spring 2015
-- Purpose: Solve the travelling salesman problem for a list of cities. 
-- This work complies with the James Madison University Honor Code.
--

module Mod9PA where

import Data.List        -- for permutations
import DistanceTuples   -- distance matrix

type City = String

-- Get a list of cities from the distance matrix
cities :: [City]
cities =  nub [c1|(c1,c2,c3)<-distanceMatrix]

-- Generate all circuits for the list of cities
allCircuits :: [[City]]
allCircuits = map append p
	where p = permutations (drop 1 cities)
              append perm =  origin ++ perm ++ origin
              origin = [head cities]
			  
-- Interrogate the distance matrix to find the distance between two cities
-- If the cities are not in the matrix, then the result is Nothing
distance :: City -> City -> (Maybe Integer)
distance city1 city2 
	| (length l == 0) = Nothing
	| otherwise =  Just (l !! 0)
	where l = [c3| (c1,c2,c3)<-distanceMatrix, c1 == city1 && c2 == city2] 
					  
-- Compute the length of a path.
-- The result is Nothing if a city in a path of at least two cities is missing 
-- from the distance matrix, because then of course we cannot compute this value.
pathLength :: [City] -> (Maybe Integer)
pathLength [] = Nothing  
pathLength [x] = Just 0  
pathLength (x:xs) = add (distance x y) (pathLength xs)
	where y = head xs 

-- Add two numbers together that maybe are Integers
-- Unpacks Types from encapsulation for use with 
-- library operations	
add :: Maybe Integer -> Maybe Integer ->Maybe Integer
add (Just a) (Just b) = Just (a + b)
add Nothing (Just b) = Nothing
add (Just a) Nothing = Nothing
add Nothing Nothing = Nothing
									  
-- Compare two paths and return the one with the shortest distance
-- If both have undefined distance, the result is []
-- If one has undefined distance, the result is the one defined
minPath :: [City] -> [City] -> [City]
minPath [] [] = []
minPath path1 path2 = 
	case (pathLength path1) of
		(Just d1) -> case (pathLength path2) of
			(Just d2) -> compare path1 path2	
			otherwise -> path1
		otherwise -> case (pathLength path2) of
			(Just d2) -> path2	
			otherwise -> []
	where compare d1 d2 = if pathLength d1 > pathLength d2
							then d2
							else d1	
									
-- Find the shortest circuit and its length.
findMinCircuit :: ([City], Maybe Integer)
findMinCircuit = (bestPath, pathLength bestPath)  
	where bestPath = foldl1 minPath allCircuits

-------------------------------------------------------------
-- Tests

testDistance =
  (distance "None" "Calgary") == Nothing &&
  (distance "Calgary" "None") == Nothing &&
  (distance "None" "None") == Nothing &&
  (distance "Calgary" "Calgary") == Nothing &&
  (distance "Calgary" "Winnipeg") == Just 1327

testPathLength =
  (pathLength []) == Nothing &&
  (pathLength ["None"]) == Just 0 &&
  (pathLength ["Calgary"]) == Just 0 &&
  (pathLength ["Calgary", "None"]) == Nothing &&
  (pathLength ["Calgary", "Winnipeg"]) == Just 1327 &&
  (pathLength ["Calgary", "Winnipeg", "Halifax"]) == Just 4828 &&
  (pathLength ["Calgary", "Winnipeg", "Nothing", "Halifax"]) == Nothing &&
  (pathLength ["Calgary", "Winnipeg", "Nothing"]) == Nothing &&
  (pathLength ["Calgary", "Winnipeg", "Ottawa"]) == Just 3469 &&
  (pathLength ["Nothing", "Winnipeg", "Ottawa"]) == Nothing &&
  (pathLength ["Calgary","Victoria","Toronto","Montreal","Halifax","Charlottetown","Ottawa","Winnipeg","Calgary"]) == Just 12423

testMinPath =
  (minPath [] []) == [] &&
  (minPath ["None"] []) == ["None"] &&
  (minPath [] ["None"]) == ["None"] &&
  (minPath ["None"] ["None"]) == ["None"] &&
  (minPath ["None", "None"] []) == [] &&
  (minPath ["Calgary", "Winnipeg"] ["Calgary","None"]) == ["Calgary","Winnipeg"] &&
  (minPath ["Calgary", "Winnipeg"] ["Calgary","Ottawa"]) == ["Calgary","Winnipeg"]

test =
  testDistance && testPathLength && testMinPath &&
  findMinCircuit == (["Calgary","Victoria","Toronto","Montreal","Halifax","Charlottetown","Ottawa","Winnipeg","Calgary"], Just 12423)
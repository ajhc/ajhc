{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 DependAnalysis

        Description:            Compute the dependencies between values. Can
                                be used for computing the dependencies in
                                variables and also the dependencies in types.
                                The code is used in type inference and
                                also kind inference.

        Primary Authors:        Bernie Pope, Robert Shelton

        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}


module FrontEnd.DependAnalysis (getBindGroups,  debugBindGroups) where

import List (nub)
import Data.Graph(stronglyConnComp, SCC(..))


--------------------------------------------------------------------------------

--
-- Given a list of nodes, a function to convert nodes to a unique name, a function
-- to convert nodes to a list of names on which the node is dependendant, bindgroups
-- will return a list of bind groups generater from the list of nodes given.
--
getBindGroups :: Ord name =>
                 [node]           ->    -- List of nodes
                 (node -> name)   ->    -- Function to convert nodes to a unique name
                 (node -> [name]) ->    -- Function to return dependencies of this node
                 [[node]]               -- Bindgroups

getBindGroups ns fn fd = map f $ stronglyConnComp [ (n, fn n, fd n) | n <- ns] where
    f (AcyclicSCC x) = [x]
    f (CyclicSCC xs) = xs
{-
getBindGroups ns getName getDeps
	= [ mapOnList nameToNodeFM group | group <- nameGroups ]
	where
	nameGroups = buildNameGroups nameList nameEdges
	nameList = map getName ns
	nameEdges = buildNameEdges ns getName getDeps
	nameToNodeFM = listToFM [ (getName x, x) | x <- ns ]

getBindGroups ns toName getDeps = filter (not . null) (map (concatMap f) $ Scc.scc ds) where
    f n = case M.lookup n m of
        --Nothing -> error $ "cannot find " ++ show n ++ " in " ++ unlines (map show (sort ds))
        --Just x -> x
        Nothing -> fail "Nothing"
        Just x -> return x
    ds = [ (toName x, getDeps x) | x <- ns ]
    m = M.fromList [ (toName x,x) | x <- ns]
-}

--
-- Create a list of edges from a list of nodes.
--
buildNameEdges :: [node]           ->    -- List of nodes
                  (node -> name)   ->    -- Function to convert nodes to a unique name
                  (node -> [name]) ->    -- Function to return dependencies of this node
                  [(name,name)]          -- Edges from list of nodes.
buildNameEdges [] _ _
	= []
buildNameEdges (n:ns) getName getDeps
	= map mapFunc (getDeps n) ++ (buildNameEdges ns getName getDeps)
	where
	mapFunc = ( \ s -> (getName n, s) )


--
-- Create a list of groups from a list of names.
--
{-
buildNameGroups :: Ord name      =>
                   [name]        ->    -- list of names
                   [(name,name)] ->    -- List of edges
                   [[name]]            -- List of bindgroups
buildNameGroups ns es
	= [ mapOnList intToNameFM group | group <- intGroups ]
	where
	intGroups = map preorder $ scc $ buildG (1, sizeFM nameToIntFM) intEdges
	intEdges = mapOnTuple nameToIntFM es
	nameToIntFM = listToFM nameIntList
	intToNameFM = listToFM [ (y,x) | (x,y) <- nameIntList ]
	nameIntList = zip ns [1..]

--
-- Use a finitemap to convert a list of type A into a list of type B
-- NB, not being able to find an element in the FM is not considered
--     an error.
--
mapOnList :: Ord a         =>
             FiniteMap a b ->    -- Finite map from a to b
             [a]           ->    -- List of a
             [b]                 -- List of b
mapOnList _ [] = []
mapOnList fm (a:as)
	= case (lookupFM fm a) of
			Just b  -> b : mapOnList fm as
			Nothing -> mapOnList fm as

--
-- Use a finitemap to convert a 2 tuple to a different type.
-- NB, not being able to find an element in the FM is not considered
--     an error.
--
mapOnTuple :: Ord a         =>
              FiniteMap a b ->
              [(a,a)]       ->
              [(b,b)]
mapOnTuple _ [] = []
mapOnTuple fm ((a1,a2):as)
	= case (lookupFM fm a1) of
		Just x  ->
			case (lookupFM fm a2) of
				Just y  -> (x,y) : (mapOnTuple fm as)
				Nothing -> mapOnTuple fm as
		Nothing -> mapOnTuple fm as

-}

--------------------------------------------------------------------------------
-- showBindGroups
--------------------------------------------------------------------------------

--
-- Display bind group information in a human readable (or as close to) form.
--
showBindGroups :: [[node]]        ->     -- List of nodes
		  (node->String)  ->     -- Function to convert a node to a string
                  String                 -- Printable string
showBindGroups ns getAlias
	= showBindGroups_ ns getAlias 0


--
-- Recursive function which does the work of showBindGroups.
--
showBindGroups_ :: [[node]]        ->     -- List of nodes
		   (node->String)  ->     -- Function to convert a node to a string
                   Int             ->     -- Bind group number
                   String                 -- Printable string
showBindGroups_ [] _ _
	= ""
showBindGroups_ (n:ns) getAlias groupNum
	= "Bindgroup " ++ show groupNum ++ " = "
	  ++ bgString ++ "\n"
	  ++ showBindGroups_ ns getAlias (groupNum + 1)
	where
	bgString = wrapString "EMPTY" (listToString n getAlias)

--------------------------------------------------------------------------------
-- debugBindGroups
--------------------------------------------------------------------------------

--
-- Display bind group information in a human readable (or as close to) form.
-- Also display dependencie and error information. Warning this function is slow
-- and fat. But without forcing name to be of type Ord, it is hard to improve
-- the algorithm.
--
debugBindGroups :: (Eq name) =>
                  [[node]]        ->     -- List of nodes
		  (node->String)  ->     -- Function to produce a printable name for the node
                  (node->name)    ->     -- Function to convert nodes to a unique name
		  (node->[name])  ->     -- Function to return dependencies of this node
                  String                 -- Printable string
debugBindGroups ns getAlias getName getDeps
	= debugBindGroups_ ns getAlias getName getDeps 0 []


--
-- Recursive function which does the work of showBindGroups.
--
debugBindGroups_ :: (Eq name) =>
                   [[node]]        ->     -- List of nodes
                   (node->String)  ->     -- Function to produce a printable name for the node
                   (node->name)    ->     -- Function to convert nodes to a unique name
		   (node->[name])  ->     -- Function to return dependencies of this node
                   Int             ->     -- Bind group number
		   [(Int,[name])]  ->     -- History information of names already processed
                   String                 -- Printable string
debugBindGroups_ [] _ _ _ _ _
	= ""
debugBindGroups_ (n:ns) getAlias getName getDeps groupNum history
	= show groupNum ++ " = "
	  ++ bgString ++ "\n"
	  ++ debugBindGroups_ ns getAlias getName getDeps (groupNum + 1) newHistory
	where
	bgString = showBindGroup (expandBindGroup n getAlias getDeps newHistory)
	newHistory = history ++ [(groupNum, [ getName x | x <- n ])]


--
-- Expand bindgroups, generating dependancie and error information.
--
expandBindGroup :: (Eq name) =>
                   [node]         ->               -- List of nodes
                   (node->String) ->               -- Function to produce a printable name for the node
		   (node->[name]) ->               -- Function to return dependencies of this node
                   [(Int,[name])] ->               -- History information of names already processed
                   ([String], [Int], [String])     -- Printable string in form (bindgroup, bgnums, Errors)
expandBindGroup [] _ _ _
	= ([],[],[])
expandBindGroup (n:ns) getAlias getDeps history
	= if err
		then (name:a, bgs++b, name:c)
		else (name:a, bgs++b, c)
	where
	name = getAlias n
	(bgs, err) = inHistory (getDeps n) history
	(a,b,c) = expandBindGroup ns getAlias getDeps history
-- NB ticti, you should not be calling inHistory on the name, but instead on the deps.

--
-- Convert the information generated by expandBindGroup into a printable
-- form.
--
showBindGroup :: ([String],[Int],[String]) -> String
showBindGroup (bg, deps, errors)
	= bgString ++ " " ++ depString ++ " " ++ errString
	where
	bgString  = wrapString [] $ listToString bg id
	depString = wrapString [] $ listToString (nub deps) show
	errString = wrapString [] $ listToString errors id

--
-- Convert a list of something, into a printable string.
--
listToString :: [a]         ->    -- List of things
                (a->String) ->    -- Function to convert things to Strings
                String            -- Single printable String.
listToString [] _
	= ""
listToString [l] lFunc
	= (lFunc l)
listToString (l:ls) lFunc
	= (lFunc l) ++ ", " ++ listToString ls lFunc


--
-- Given a list of names and the history of visited names, this function
-- generates a list of bindgroups that are depended upon as well as returning
-- a boolean value indicating whether all these dependencies are satisfied.
--
-- True -> ERROR, a name needed now has not been resolved.
--
inHistory :: Eq name =>
             [name]         ->    -- List of names to be searched for
             [(Int,[name])] ->    -- History information of names already processed
             ([Int],Bool)         -- Number of bind group that name is in, or its own alias.
inHistory [] _
	= ([],False)
inHistory (name:names) history
	= if location < 0
		then (bgs, False)
		else (location : bgs, err)
	where
	location = searchHistory name history
	(bgs, err) = inHistory names history

--
-- Check whether a particular name has occured befor and return the number
-- of the bindgroup it occured in.
--
searchHistory :: Eq name        =>
                 name           ->   -- List of names to be searched for
                 [(Int,[name])] ->   -- History information of names already processed
                 Int                 -- Bindgroup num that name occurred in (-1 is error)
searchHistory _ []
	= -1
searchHistory name ((bgnum, bgnames):history)
	= if elem name bgnames
		then bgnum
		else searchHistory name history

--
-- Neatly brackets a string using a replacement string (rep) if empty.
--
wrapString :: String -> String -> String
wrapString rep "" = "[" ++ rep ++ "]"
wrapString _   s  = "[" ++ s ++ "]"

--------------------------------------------------------------------------------

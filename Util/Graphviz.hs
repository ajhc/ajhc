-- | Simple graphviz output.
module Util.Graphviz(
    Orient(..),
    graphviz, graphviz'
) where

import Data.Graph.Inductive.Graph
import List(intersperse)

data Orient = Portrait | Landscape deriving (Eq, Show)

o2s :: Orient -> String
o2s Portrait = "\trotate = \"0\"\n"
o2s Landscape = "\trotate = \"90\"\n"


i2d :: Int -> Double
i2d = fromInteger . toInteger


-- | Format a graph for graphviz with reasonable defaults: title of \"fgl\",
-- 8.5x11 pages, one page, landscape orientation
graphviz' :: Graph g => g a b -> (a -> [(String,String)]) -> (b -> [(String,String)]) -> String
graphviz' g fnode fedge = graphviz g "fgl" fnode fedge  (8.5,11.0) (1,1) Landscape

sq :: String -> String
sq ('"':s) | last s == '"'  = init s
	   | otherwise	    = s
sq ('\'':s) | last s == '\''	= init s
	    | otherwise		= s
sq s = s


sl :: [(String,String)] -> String
sl [] = []
sl a = " [" ++ foldr ($) "]" (intersperse (',':) (map f a)) where
    f (x,y) = ((x ++ " = " ++  (show y)) ++)




graphviz :: Graph g =>    g a b   -- ^ The graph to format
			  -> String  -- ^ The title of the graph
                          -> (a -> [(String,String)])
                          -> (b -> [(String,String)])
			  -> (Double, Double)	-- ^ The size
			  -- of the page
			  -> (Int, Int)	-- ^ The width and
			  -- height of the page
			  -- grid
			  -> Orient  -- ^ The orientation of
			  -- the graph.
			  -> String


graphviz g t fnode fedge (w, h) p@(pw', ph') o =
    let n = labNodes g
	e = labEdges g
	ns = concatMap sn n
	es = concatMap se e
	sz w' h' = if o == Portrait then show w'++","++show h' else show h'++","++show w'
	ps = show w++","++show h
	(pw, ph) = if o == Portrait then p else (ph', pw')
	--gs = show ((w*(i2d pw))-m)++","++show ((h*(i2d ph))-m)
	gs = sz (w*(i2d pw)) (h*(i2d ph))
    in "digraph "++sq t++" {\n"
	    ++"\tmargin = \"0\"\n"
	    ++"\tpage = \""++ps++"\"\n"
	    ++"\tsize = \""++gs++"\"\n"
	    ++o2s o
	    ++"\tratio = \"fill\"\n"
	    ++ns
	    ++es
	++"}"
    where sn (n, a) | sa == ""	= ""
		    | otherwise	= '\t':(show n ++ sa ++ "\n")
	    where sa = sl (fnode a)
	  se (n1, n2, b) = '\t':(show n1 ++ " -> " ++ show n2 ++ sl (fedge b) ++ "\n")




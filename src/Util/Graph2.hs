-- | Data.Graph is sorely lacking in several ways, This just tries to fill in
-- some holes and provide a more convinient interface

module Util.Graph2 where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST (STArray, newArray, readArray, writeArray,getAssocs)
import Control.Monad.ST
import Data.Graph hiding(Graph)
import Data.Maybe
import GenUtil
import List(sort,sortBy,group,delete)
import qualified Data.Graph as G
import qualified Data.Map as Map


data Graph n = Graph G.Graph (Table n)

graphGraph (Graph x _) = x
graphNodes (Graph _ y) = y

instance Show n => Show (Graph n) where
    showsPrec n g = showsPrec n (Util.Graph2.scc g)

fromGraph :: Graph n -> [(n,[n])]
fromGraph (Graph g lv) = [ (lv!v,map (lv!) vs) | (v,vs) <- assocs g ]

-- | Build a graph from a list of nodes uniquely identified by keys,
-- with a list of keys of nodes this node should have edges to.
-- The out-list may contain keys that don't correspond to
-- nodes of the graph; they are ignored.
newGraph :: Ord k => [n] -> (n -> k) -> (n -> [k]) -> Graph n
newGraph ns fn fd = Graph graph nr where
    nr = listArray bounds0 ns
    max_v      	    = length ns - 1
    bounds0         = (0,max_v) :: (Vertex, Vertex)
    kmap = Map.fromList [ (fn n,i) | (i,n) <- zip [0 ..] ns ]
    graph	    = listArray bounds0 [mapMaybe (flip Map.lookup kmap) (snub $ fd n) | n <- ns]

fromScc (Left n) = [n]
fromScc (Right n) = n

-- | determine a set of loopbreakers subject to a fitness function
-- loopbreakers have a minimum of their  incoming edges ignored.
findLoopBreakers
    :: (n -> Int)  -- ^ fitness function, greater numbers mean more likely to be a loopbreaker
    -> (n -> Bool) -- ^ whether a node is suitable at all for a choice as loopbreaker
    -> Graph n     -- ^ the graph
    ->  ([n],[n])  -- ^ (loop breakers,dependency ordered nodes after loopbreaking)
findLoopBreakers func ex (Graph g ln) = ans where
    scc = G.scc g
    ans = f g scc [] [] where
        f g (Node v []:sccs) fs lb
            | v `elem` g ! v = let ng = (fmap (List.delete v) g) in  f ng (G.scc ng) [] (v:lb)
            | otherwise = f g sccs (v:fs) lb

        f g (n:_) fs lb = f ng (G.scc ng) [] (mv:lb) where
            mv = case  sortBy (\ a b -> compare (snd b) (snd a)) [ (v,func (ln!v)) | v <- ns, ex (ln!v) ] of
                ((mv,_):_) -> mv
                [] -> error "findLoopBreakers: no valid loopbreakers"
            ns = dec n []
            ng = fmap (List.delete mv) g

        f _ [] xs lb = (map ((ln!) . head) (group $ sort lb),reverse $ map (ln!) xs)
    dec (Node v ts) vs = v:foldr dec vs ts


sccGroups :: Graph n -> [[n]]
sccGroups g = map fromScc (Util.Graph2.scc g)

scc :: Graph n -> [Either n [n]]
scc (Graph g ln) = map decode forest where
    forest = G.scc g
    decode (Node v [])
        | v `elem` g ! v = Right [ln!v]
        | otherwise = Left (ln!v)
    decode other = Right (dec other [])
    dec (Node v ts) vs = ln!v:foldr dec vs ts

sccForest :: Graph n -> Forest n
sccForest (Graph g ln) = map (fmap (ln!)) forest where
    forest = G.scc g

dff :: Graph n -> Forest n
dff (Graph g ln) = map (fmap (ln!)) forest where
    forest = G.dff g

components :: Graph n -> [[n]]
components (Graph g ln) = map decode forest where
    forest = G.components g
    decode n = dec n []
    dec (Node v ts) vs = ln!v:foldr dec vs ts


topSort :: Graph n -> [n]
topSort (Graph g ln) = map (ln!) $ G.topSort g

cyclicNodes :: Graph n -> [n]
cyclicNodes g = concat [ xs | Right xs <- Util.Graph2.scc g]

toDag :: Graph n -> Graph [n]
toDag (Graph g lv) = Graph g' ns' where
    ns' = listArray (0,max_v) [ map (lv!) ns |  ns <- nss ]
    g' = listArray (0,max_v) [ snub [ v | n <- ns, v <- g!n ] | ns <- nss ]
    max_v = length nss - 1
    nss = map (flip f []) (G.scc g) where
        f (Node v ts) rs = v:foldr f rs ts

type AdjacencyMatrix s  = STArray s (Vertex,Vertex) Bool

transitiveClosureAM arr = do
    forM_ [0 .. max_v] $ \k -> do
        forM_ [0 .. max_v] $ \i -> do
            forM_ [0 .. max_v] $ \j -> do
                dij <- readArray arr (i,j)
                dik <- readArray arr (i,k)
                dkj <- readArray arr (k,j)
                writeArray arr (i,j) (dij || (dik && dkj))

transitiveClosure :: Graph n -> Graph n
transitiveClosure (Graph g ns) = let g' = runST (tc g) in (Graph g' ns) where
    (0,max_v) = bounds g
    tc :: G.Graph -> ST s G.Graph
    tc g = do
        arr <- newArray ((0,0),(max_v,max_v)) False :: ST s (STArray s (Vertex,Vertex) Bool)
        sequence_ [ writeArray arr (v,u) True | (v,vs) <- assocs g, u <- vs ]
        forM_ [0 .. max_v] $ \k -> do
            forM_ [0 .. max_v] $ \i -> do
                forM_ [0 .. max_v] $ \j -> do
                    dij <- readArray arr (i,j)
                    dik <- readArray arr (i,k)
                    dkj <- readArray arr (k,j)
                    writeArray arr (i,j) (dij || (dik && dkj))
                    return ()
        rs <- getAssocs arr
        let rs' = [ x | (x,True) <- rs ]
--        unsafeIOToST $ print rs'
        return (listArray (bounds g) [ [ v | (n',v) <- rs', n == n' ] | n <- vertices g ])

--transitiveClosure' ::  G.Graph -> G.Graph
--transitiveClosure' g = array (bounds g) [ (i,G.reachable g i) | i <- vertices g]

--transitiveClosure :: Graph n -> Graph n
--transitiveClosure (Graph g x1) = Graph (transitiveClosure' g) x1


instance Functor Graph where
    fmap f (Graph g n) = Graph g (fmap f n)

mapT    :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [ (,) v (f v (t!v)) | v <- indices t ]

--mapGraph :: (Either a [a] -> [b] -> b) -> Graph a -> Graph b



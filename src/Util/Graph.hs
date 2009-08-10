-- | Data.Graph is sorely lacking in several ways, This just tries to fill in
-- some holes and provide a more convinient interface

module Util.Graph where

import Data.Array
import Data.Graph hiding(Graph)
import GenUtil
import List(sort,sortBy,group,delete)
import qualified Data.Graph as G


data Graph n k = Graph G.Graph (Vertex -> n) (k -> Maybe Vertex) (n -> k)

instance Show n => Show (Graph n k) where
    showsPrec n g = showsPrec n (Util.Graph.scc g)

fromGraph :: Graph n k -> [(n,[n])] 
fromGraph (Graph g lv _ _) = [ (lv v,map lv vs) | (v,vs) <- assocs g ] 

newGraph :: Ord k => [n] -> (n -> k) -> (n -> [k]) -> Graph n k
newGraph ns fn fd = Graph ans lv' kv fn where
    (ans,lv,kv) = graphFromEdges [ (n,fn n,snub $ fd n) | n <- ns ]
    lv' x | (n,_,_) <- lv x = n

fromScc (Left n) = [n]
fromScc (Right n) = n

-- | determine a set of loopbreakers subject to a fitness function
-- loopbreakers have a minimum of their  incoming edges ignored.
findLoopBreakers ::
    (n -> Int)    -- ^ fitness function, greater numbers mean more likely to be a loopbreaker
    -> (n -> Bool) -- ^ whether a node is suitable at all for a choice as loopbreaker
    -> Graph n k  -- ^ the graph
    ->  ([n],[n]) -- ^ (loop breakers,dependency ordered nodes after loopbreaking)
findLoopBreakers func ex (Graph g ln kv fn) = ans where
    scc = G.scc g
    ans = f g scc [] [] where
        f g (Node v []:sccs) fs lb
            | v `elem` g ! v = let ng = (fmap (List.delete v) g) in  f ng (G.scc ng) [] (v:lb)
            | otherwise = f g sccs (v:fs) lb

        f g (n:_) fs lb = f ng (G.scc ng) [] (mv:lb) where
            mv = case  sortBy (\ a b -> compare (snd b) (snd a)) [ (v,func (ln v)) | v <- ns, ex (ln v) ] of
                ((mv,_):_) -> mv
                [] -> error "findLoopBreakers: no valid loopbreakers"
            ns = dec n []
            ng = fmap (List.delete mv) g

        f _ [] xs lb = (map (ln . head) (group $ sort lb),reverse $ map ln xs)
    dec (Node v ts) vs = v:foldr dec vs ts


sccGroups :: Graph n k -> [[n]]
sccGroups g = map fromScc (Util.Graph.scc g)

scc :: Graph n k -> [Either n [n]]
scc (Graph g ln kv fn) = map decode forest where
    forest = G.scc g
    decode (Node v [])
        | v `elem` g ! v = Right [ln v]
        | otherwise = Left (ln v)
    decode other = Right (dec other [])
    dec (Node v ts) vs = ln v:foldr dec vs ts

sccForest :: Graph n k -> Forest n
sccForest (Graph g ln kv fn) = map (fmap ln) forest where
    forest = G.scc g

dff :: Graph n k -> Forest n
dff (Graph g ln kv fn) = map (fmap ln) forest where
    forest = G.dff g

dfs :: Graph n k -> [k] -> Forest n
dfs (Graph g ln kv fn) ks = map (fmap ln) forest where
    forest = G.dfs g [ v | Just v <- map kv ks]

components :: Graph n k -> [[n]]
components (Graph g ln kv fn) = map decode forest where
    forest = G.components g
    decode n = dec n []
    dec (Node v ts) vs = ln v:foldr dec vs ts


reachable :: Graph n k -> [k] -> [n]
reachable (Graph g ln kv _) ns = map ln $ snub $  concatMap (G.reachable g) gs where
    gs = [ v | Just v <- map kv ns]

topSort :: Graph n k -> [n]
topSort (Graph g ln _ _) = map ln $ G.topSort g

cyclicNodes :: Graph n k -> [n]
cyclicNodes g = concat [ xs | Right xs <- Util.Graph.scc g]


transitiveClosure' ::  G.Graph -> G.Graph 
transitiveClosure' g = array (bounds g) [ (i,G.reachable g i) | i <- vertices g]

transitiveClosure :: Graph n k -> Graph n k
transitiveClosure (Graph g x1 x2 x3) = Graph (transitiveClosure' g) x1 x2 x3

mapT    :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [ (,) v (f v (t!v)) | v <- indices t ]




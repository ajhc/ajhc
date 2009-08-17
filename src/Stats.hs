module Stats(
    -- mutable
    Stats,
    new,
    tick,
    setPrintStats,
    ticks,
    theStats,
    isEmpty,
    null,
    Stats.print,
    clear,
    combine,
    -- pure
    printStat,
    printLStat,
    Stat,
    Stats.singleton,
    Stats.singleStat,
    prependStat,
    -- monad
    MonadStats(..),
    StatT,
    StatM,
    mtick,
    mticks,
    runStatT,
    runStatIO,
    runStatM,
    -- combined
    tickStat
    ) where


import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.IORef
import Data.Tree
import List(sort,groupBy)
import Prelude hiding(null)
import System.IO.Unsafe
import qualified Data.Map as Map
import qualified Prelude(null)

import StringTable.Atom
import CharIO
import GenUtil
import qualified Doc.Chars as C
import qualified Util.IntBag as IB

splitUp :: Int -> String -> [String]
splitUp n str = filter (not . Prelude.null) (f n str)  where
    f 0 str = []
    f n str = case span (`notElem` "/.{") str  of
        (x,"") -> [x]
        (x,('/':rs)) -> x:f (n - 1) rs
        (x,('.':rs)) -> x:f n rs
        (x,('{':rs)) -> case span (/= '}') rs of
            (a,'}':b) -> x:a:f n b
            (a,"") -> [x,a]
            _ -> error "this can't happen"
        _ -> error "this can't happen"


print greets stats = do
    l <- toList stats
    let fs = createForest 0 $ sort [(splitUp (-1) $ fromAtom x,y) | (x,y) <- l]
    mapM_ CharIO.putErrLn $ ( draw . fmap p ) (Node (greets,0) fs)  where
        p (x,0) = x
        p (x,n) = x ++ ": " ++ show n




createForest :: a -> [([String],a)] -> Forest (String,a)
createForest def xs = map f gs where
    f [(xs,ys)] =  Node (intercalate "." xs,ys) []
    f xs@((x:_,_):_) = Node (x,def) (createForest def [ (xs,ys) | (_:xs@(_:_),ys)<- xs])
    f _ = error "createForest: should not happen."
    gs = groupBy (\(x:_,_) (y:_,_) -> x == y) xs

draw :: Tree String -> [String]
draw (Node x ts0) = x : drawSubTrees ts0
  where drawSubTrees [] = []
        drawSubTrees [t] =
                {-[vLine] :-} shift lastBranch "  " (draw t)
        drawSubTrees (t:ts) =
                {-[vLine] :-} shift branch (C.vLine  ++ " ") (draw t) ++ drawSubTrees ts

        branch     = C.lTee ++ C.hLine
        lastBranch = C.llCorner ++ C.hLine

        shift first other = zipWith (++) (first : repeat other)
        --vLine = chr 0x254F



-- Pure varients

newtype Stat = Stat IB.IntBag
    deriving(Eq,Ord,Monoid)

prependStat :: String -> Stat -> Stat
prependStat name (Stat m) = Stat $ IB.fromList [ (fromAtom $ mappend (toAtom $ "{" ++ name ++ "}.")  (unsafeIntToAtom x),y) | (x,y) <- IB.toList m ]

printStat greets (Stat s) = do
    let fs = createForest 0 $ sort [(splitUp (-1) $ fromAtom (unsafeIntToAtom x),y) | (x,y) <- IB.toList s]
    mapM_ CharIO.putErrLn $ ( draw . fmap p ) (Node (greets,0) fs)  where
        p (x,0) = x
        p (x,n) = x ++ ": " ++ show n

printLStat n greets (Stat s) = do
    let fs = createForest 0 $ [ (x,y) | (x,y) <- Map.toList $ Map.fromListWith (+) [( splitUp n (fromAtom (unsafeIntToAtom x)),y) | (x,y) <- IB.toList s]]
    mapM_ CharIO.putErrLn $ ( draw . fmap p ) (Node (greets,0) fs)  where
        p (x,0) = x
        p (x,n) = x ++ ": " ++ show n




--------------
-- monad stats
--------------


class Monad m => MonadStats m where
    mticks' ::  Int -> Atom -> m ()
    mtickStat :: Stat -> m ()



newtype StatT m a = StatT (WriterT Stat m a)
    deriving(MonadIO, Functor, MonadFix, MonadTrans, Monad)


runStatT :: Monad m => StatT m a -> m (a,Stat)
runStatT (StatT m) =  runWriterT m


data StatM a = StatM a !Stat

instance Functor StatM where
    fmap f (StatM a s) = StatM (f a) s

instance Monad StatM where
    StatM _ s1 >> StatM y s2 = StatM y (s1 `mappend` s2)
    return x = StatM x mempty
    StatM x s1 >>= y = case y x of StatM z s2 -> StatM z (s1 `mappend` s2)

instance Stats.MonadStats StatM where
   mticks' 0 k = StatM () mempty
   mticks' n k = StatM () $ Stats.singleStat n k
   mtickStat s = StatM () s



runStatM ::  StatM a -> (a,Stat)
runStatM (StatM a s) = (a,s)


-- These are inlined so the 'toAtom' can become a caf and be shared
{-# INLINE mtick  #-}
{-# INLINE mticks #-}
mtick k = mticks 1 k
mticks 0 _ = return ()
mticks n k = let k' = toAtom k in k' `seq` n `seq` mticks' n k'


instance MonadStats Identity where
    mticks' _ _ = return ()
    mtickStat _ = return ()

instance MonadReader r m => MonadReader r (StatT m) where
    ask = lift $ ask
    local f (StatT m) = StatT $ local f m

instance (Monad m, Monad (t m), MonadTrans t, MonadStats m) => MonadStats (t m) where
    mticks' n k = lift $ mticks' n k
    mtickStat s = lift $ mtickStat s

instance Monad m => MonadStats (StatT m) where
    mticks' n k = StatT $ tell (Stat $ IB.msingleton (fromAtom k) n)
    mtickStat s =  StatT $ tell s

singleton n = Stat $ IB.singleton (fromAtom $ toAtom n)

singleStat :: ToAtom a => Int -> a -> Stat
singleStat 0 _ = mempty
singleStat n k = Stat $ IB.msingleton (fromAtom $ toAtom k) n

null (Stat r) = IB.null r

instance MonadStats IO where
    mticks' 0 _ = return ()
    mticks' n a = do
        p <- readIORef printStats
        when p (CharIO.putStrLn $ (show a ++ ": " ++ show n))
        ticks theStats n a
    mtickStat (Stat s) = do
        tickStat theStats (Stat s)
        p <- readIORef printStats
        when p $ forM_ (IB.toList s) $ \ (x,y) -> do
            CharIO.putStrLn (show (unsafeIntToAtom x) ++ ": " ++ show y)



--------------------
-- Stateful IO stats
--------------------

newtype Stats = Stats (IORef Stat)

{-# NOINLINE theStats #-}
theStats :: Stats
theStats = unsafePerformIO new

{-# NOINLINE printStats #-}
printStats :: IORef Bool
printStats = unsafePerformIO $ newIORef False

setPrintStats :: Bool -> IO ()
setPrintStats b = writeIORef printStats b

combine :: Stats -> Stats -> IO ()
combine (Stats s1) (Stats s2) = do
    s <- readIORef s2
    modifyIORef s1 (mappend s)

new = Stats `liftM` newIORef mempty

clear (Stats h) = writeIORef h mempty

toList (Stats r) = do
    Stat s <- readIORef r
    return [(unsafeIntToAtom x,y) | (x,y) <- IB.toList s]

isEmpty (Stats r) = null `liftM` readIORef r

tick stats k = ticks stats 1 k


ticks (Stats r) c k = modifyIORef r (mappend $ singleStat c k)

-----------------
-- pure + mutable
-----------------


tickStat ::  Stats -> Stat -> IO ()
tickStat (Stats r) s = modifyIORef r (mappend s)


runStatIO :: MonadIO m =>  Stats -> StatT m a -> m a
runStatIO stats action = do
    (a,s) <- runStatT action
    liftIO $ tickStat stats s
    return a


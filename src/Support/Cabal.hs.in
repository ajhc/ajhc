{-# LANGUAGE CPP #-}
module Support.Cabal (getDataFileNameMaybe) where
#ifdef WITH_CABAL
import Paths_ajhc (getDataFileName)
#endif

getDataFileNameMaybe :: FilePath -> IO (Maybe FilePath)
#ifdef WITH_CABAL
getDataFileNameMaybe s = do f <- getDataFileName s
                            return $ Just f
#else
getDataFileNameMaybe _ = return Nothing
#endif

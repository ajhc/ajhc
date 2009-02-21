{-# OPTIONS_JHC -fcpp #-}

#ifdef __JHC__
main = putStrLn "in Jhc"
#else
main = putStrLn "not in Jhc"
#endif

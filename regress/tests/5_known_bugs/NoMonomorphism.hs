{-# LANGUAGE NoMonomorphismRestriction #-}
{-# JHC_OPTIONS -fno-monomorphism-restriction #-}

x = 234
y = 15

main = do print (x `mod` y)
          print (x / y)
          print (x ** (-y))


{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}
module Support.Compat where

-- This module is meant to contain code
-- that only exists for compatability between platforms


import Control.Exception


#if __GLASGOW_HASKELL__ < 610
type SomeException = Exception
#endif

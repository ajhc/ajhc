module Info.Binary where

import Info.Info
import Binary

putInfo :: Binary.BinHandle -> Info.Info.Info -> IO ()
getInfo :: Binary.BinHandle -> IO Info.Info.Info

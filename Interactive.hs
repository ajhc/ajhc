module Interactive(Interactive.interact) where

import Data.Version
import System.Info

import Util.Interact
import Ho
import Version
import Options

interact :: Ho -> IO ()
interact _ho = beginInteraction emptyInteract { interactSettables = ["prog", "args"], interactVersion = versionString }



module Support.Transform where


data TransformParms p = TransformParms {
    transformIterate :: Iterate,
    transformDumpProgress :: Bool,
    transformSkipNoStats  :: Bool,
    transformOperation :: p -> IO p,
    transformCategory :: String,   -- ^ general name of transformation
    transformPass :: String,       -- ^ what pass we are in
    transformName :: String        -- ^ name of what we are working on
    }

transformParms = TransformParms {
    transformIterate = DontIterate,
    transformDumpProgress = False,
    transformSkipNoStats = False,
    transformCategory = "Unknown",
    transformPass = "",
    transformOperation = return,
    transformName = ""
    }

data Iterate = DontIterate | IterateMax !Int | IterateExactly !Int | IterateDone
    deriving(Eq)

doIterate IterateMax {}     True = True
doIterate IterateDone       True = True
doIterate IterateExactly {} _    = True
doIterate _ _ = False

iterateStep (IterateMax n) = IterateMax (n - 1)
iterateStep (IterateExactly n) = IterateExactly (n - 1)
iterateStep x = x


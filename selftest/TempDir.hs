import Support.TempDir
import System.IO
import Control.Exception

main = wrapMain $ do
    td <- getTempDir
    (fp,h) <- createTempFile "foo.jhc_core"
    hPutStrLn h "Hey, buddy"
    hClose h

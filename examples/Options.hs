
import Jhc.Options
import Text.Printf

main :: IO ()
main = do
    printf "isWindows:      %s\n" (show isWindows)
    printf "isPosix:        %s\n" (show isPosix)
    printf "isBigEndian:    %s\n" (show isBigEndian)
    printf "isLittleEndian: %s\n" (show isLittleEndian)
    printf "Target:         %s\n" (show target)


instance Show Target where
    show Grin = "Grin"
    show GhcHs = "GhcHs"
    show DotNet = "DotNet"
    show Java = "Java"

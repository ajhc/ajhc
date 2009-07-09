

import FrontEnd.Syn.Options


main = do
    cs <- getContents
    print (parseOptions cs)


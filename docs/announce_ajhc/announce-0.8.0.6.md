# ANNOUNCE: Ajhc 0.8.0.6 Release

We are happy to announce Ajhc 0.8.0.6.
Major change on the release is supporting THREAD. Yeah!

You can get Ajhc using "cabal install ajhc" command.
Ajhc's project web site is found at http://ajhc.metasepi.org/.
You find the source code at https://github.com/ajhc/ajhc/tags.
Welcome sending any bugs or your ideas to https://github.com/ajhc/ajhc/issues.

## Good news

* You can use forkOS interface on Ajhc.
* Your program runs on pthread with compiling -fpthread option. See below.

    $ cat Main.hs
    import Control.Monad
    import Control.Concurrent
    
    main :: IO ()
    main = do
      l <- putStrLn "Type some string and enter." >> getLine
      forkOS $ (forever $ putChar '*')
      forkOS $ (forever $ putStr l)
      forever $ putChar '.'
    $ ajhc -fpthread Main.hs
    $ ./hs.out

## Bad news

* Ajhc guards critical section only for the runtime.
* Example: StablePtr isn't guarded with lock.
* Ajhc's thread can't share any objects without Ptr type.
* And you may find funny pthread bugs. Please send us the report!

## Other changes

* No more depend on DrIFT.
* Add _JHC_JGC_LIMITED_NUM_GC_STACK flag to set number of limited gc_stack entries.
* Add _JHC_JGC_FIXED_MEGABLOCK flag to set number of limited megablock entries.
* Add compile flags -fcustomthread -fpthread -fnothread.
* You can implement your own thread API for Ajhc, if you choose -fcustomthread.

Enjoy! ;)
- - -
Metasepi team

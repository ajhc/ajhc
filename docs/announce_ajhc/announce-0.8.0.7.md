# ANNOUNCE: Ajhc Haskell Compiler 0.8.0.7 Release

We are happy to announce Ajhc 0.8.0.7.
You can program interrupt handler with Haskell language on this release.
Major change on the release is supporting THREAD. Yeah!

You can get Ajhc using "cabal install ajhc" command.
The usage is found at Ajhc's project web site http://ajhc.metasepi.org/.
The source code at https://github.com/ajhc/ajhc/tags.

Welcome sending any bugs or your ideas to https://github.com/ajhc/ajhc/issues.

## An example of interrupt handler written with Haskell

<https://github.com/ajhc/demo-cortex-m3/tree/master/stm32f3-discovery>

The demo for Cortex-M4 has main context and intrrupt context.
The main context waits time expire with polling counter.
<https://github.com/ajhc/demo-cortex-m3/blob/master/stm32f3-discovery/hs_src/Intr.hs#L17>

The interrupt context is called from clock exception, and decrement counter.
<https://github.com/ajhc/demo-cortex-m3/blob/master/stm32f3-discovery/hs_src/Intr.hs#L9>

## Other changes

* Guard StablePtr critical section.
* Add _JHC_JGC_SAVING_MALLOC_HEAP option for getting smaller malloc heap.
* Link forkIO to forkOS.

Enjoy! :)
- - -
Metasepi team

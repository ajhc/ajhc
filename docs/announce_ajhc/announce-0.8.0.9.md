# ANNOUNCE: Ajhc Haskell Compiler 0.8.0.9 Release
To: jhc@haskell.org, haskell-cafe@haskell.org, metasepi@googlegroups.com

We are happy to announce Ajhc 0.8.0.9.
This version is a point release to fix some BUGs, and support Android NDK platform.

You can get Ajhc using "cabal install ajhc" command.
The usage is found at Ajhc's project web site http://ajhc.metasepi.org/.
The source code at https://github.com/ajhc/ajhc.

Welcome sending any bugs or your ideas to https://github.com/ajhc/ajhc/issues.

## Support new platforms

### Android NDK http://developer.android.com/tools/sdk/ndk/index.html

The demo movie at http://www.youtube.com/watch?v=n6cepTfnFoo.
The touchable cube application is written with Haskell and compiled by Ajhc.
In the demo, the application is breaked by ndk-gdb debugger when running GC.
You could watch the demo source code at \url{https://github.com/ajhc/demo-android-ndk}.

## Other changes

* Experimental support Mac OS X to compile Ajhc and use it.
* Start to run buid test for Mac OS X on Travis CI.
  https://travis-ci.org/ajhc/ajhc/builds/12218394
* Depend on derive package. But near future use DrIFT again. For Haskell 98 compliant.
  https://github.com/ajhc/ajhc/issues/47
* Fix the bugs forgetting to clear memory.

Enjoy! :)
- - -
Metasepi team

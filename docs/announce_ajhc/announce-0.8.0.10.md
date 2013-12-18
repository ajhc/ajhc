# ANNOUNCE: Ajhc Haskell Compiler 0.8.0.10 Release
To: jhc@haskell.org, haskell-cafe@haskell.org, metasepi@googlegroups.com

We are happy to announce Ajhc 0.8.0.10 as Christmas release!

You can get Ajhc using "cabal install drift && cabal install ajhc" command.
The usage is found at Ajhc's project web site http://ajhc.metasepi.org/.
The source code at https://github.com/ajhc/ajhc.

Welcome sending any bugs or your ideas to https://github.com/ajhc/ajhc/issues.

## News

### Android demo

Android demo application using Haskell is available at Google Play.

https://play.google.com/store/apps/details?id=org.metasepi.ajhc.android.cube

### Clear license notification

* Runtime: [MIT License](https://github.com/ajhc/ajhc/blob/master/rts/LICENSE)
* Haskell libraries: [MIT License](https://github.com/ajhc/ajhc/blob/master/lib/LICENSE)
* The others: [GPLv2 or Later](https://github.com/ajhc/ajhc/blob/arafura/COPYING)

## Other changes

* Depend on DrIFT again / Do not need derive
* Fix GC root BUG
* Use findExecutable instead of the raw `which`
* Come back bytestring
* And fix other BUGs

Enjoy! :)
- - -
Metasepi team

# [Ajhc](http://ajhc.github.com/) - arafura-jhc [![Build Status](https://travis-ci.org/ajhc/ajhc.png)](https://travis-ci.org/ajhc/ajhc)

A fork of [jhc](http://repetae.net/computer/jhc/).
And also Haskell compiler. 

This project is founded by [Metasepi Project](http://metasepi.masterq.net/).

## How to install

    $ cabal install ajhc

## How to build latest version

    $ sudo apt-get install make locales autoconf libreadline-dev \
      libwww-perl libconfig-yaml-perl graphviz haskell-platform drift pandoc \
      libghc-readline-dev libghc-utf8-string-dev libghc-hssyck-dev
    $ git clone git://github.com/ajhc/ajhc.git
    $ cd ajhc
    $ git checkout arafura
    $ autoreconf -i
    $ ./configure
    $ make cabal-install

## For developing

First, you should use the arafura branch, rather than master branch.

* [master branch](https://github.com/ajhc/ajhc/tree/master): Do not touch me, mirror of [jhc's darcs repository](http://repetae.net/dw/darcsweb.cgi?r=jhc).
* [arafura branch](https://github.com/ajhc/ajhc/tree/arafura): For developing Ajhc.

You should send patch to jhc, if testing on Ajhc is good.

    $ darcs get http://repetae.net/repos/jhc
    $ cd jhc/
    $ patch -p1 < ~/yourfile.patch
    $ darcs record -a
    $ darcs send

The command [darcs send](http://darcs.net/Using/Send) sends email the patch to
jhc author (= [John Meacham](http://repetae.net/)).

## Needing to install/build on Windows (currently in progress)

### Install msys

http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/

Install below.

* C Compiler
* MSYS Basic System
* MinGW Developer ToolKit

You should run cabal install on msys console.

### Install Git for Windows

http://msysgit.github.com/

It's good choosing the below option.

* "Checkout as-is, commit as-is"
* "Run Git and included Unix tools from the Windows Command Prompt"

### Install Haskell Platform

http://www.haskell.org/platform/windows.html

### Install DrIFT

    $ cabal install DrIFT-cabalized

## Future plan

### Done

* Try to rewrite a part of NetBSD bootloader.
  http://www.youtube.com/watch?v=JMyxy5Qo2m8
* Translate Jhc User’s Manual to Japanese.
  http://metasepi.masterq.net/posts/2013-01-12-jhc_manual_ja.html
* Fix bug that run stm32f3-discovery demo. It causes Ajhc RTS heep impl.
  https://github.com/ajhc/demo-cortex-m3/tree/master/stm32f3-discovery
* Support to build on Windows.
* Cabalize Ajhc. Ajhc's hl files will be not controled with cabal.

### Yet

* No more depend on DrIFT. Use http://hackage.haskell.org/package/derive.
* No more depend on Perl (LWP and YAML).
* Support LPCXpresso NXP LPX1769 with demo.
* Rewrite Cortex-M3 base library with Ajhc.
* Pass all regress test, and enable regress fail setting on travis-ci.
  https://travis-ci.org/ajhc/ajhc
* Analyze jhc internal (Japanese doc).
  http://metasepi.masterq.net/posts/2013-01-31-jhc_internal_overview.html
* Find critical region of Ajhc for reentrant and thread safe.
* Port Haskell libraries on haskell-platform to Ajhc.
* Understand jhc's region inference.
* Find the method to implement GC that can be interrupted.
* Get smaller RTS. Benchmark the RTS for running on custom FPGA CPU.
* Start rewritng NetBSD kernel with Ajhc.

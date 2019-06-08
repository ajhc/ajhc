# [Ajhc](http://ajhc.metasepi.org/) - arafura-jhc

## *** We are closing Ajhc project. ***

[Metasepi Arafura](http://metasepi.org/en/posts/2013-01-09-design_arafura.html) iteration is closed.
We decided to use [ATS Programming Language](http://www.ats-lang.org/) for our next iteration.
After merging Ajhc's [Context Local Heap](http://metasepi.org/doc/metasepi-icfp2014.pdf) into jhc,
this project will be shutdown.

Please use [jhc](http://repetae.net/computer/jhc/) instead of Ajhc. The [jhc-components](https://github.com/csabahruska/jhc-components) may be useful, if you can't build original jhc.</p>

A fork of [jhc](http://repetae.net/computer/jhc/).
And also a Haskell compiler. 
This project is founded by [Metasepi Project](http://metasepi.org/).

## Build Status

* [![Build Status](https://travis-ci.org/ajhc/ajhc.png?branch=arafura)](https://travis-ci.org/ajhc/ajhc) Ubuntu
* [![Build Status](https://travis-ci.org/ajhc/ajhc.png?branch=arafura-ci4osx)](https://travis-ci.org/ajhc/ajhc) Mac OS X

## How to install

    $ sudo apt-get install haskell-platform gcc m4 patch libncurses5-dev
    $ cabal install drift
    $ export PATH=$PATH:$HOME/.cabal/bin
    $ cabal install ajhc

## How to build latest version

    $ sudo apt-get install make locales autoconf drift \
      libconfig-yaml-perl graphviz haskell-platform cpphs pandoc hscolour po4a \
      libghc-temporary-dev libghc-haskeline-dev libghc-utf8-string-dev libghc-hssyck-dev \
      libghc-test-framework-th-dev libghc-test-framework-hunit-dev \
      libghc-test-framework-quickcheck2-dev libghc-uniplate-dev libgc-dev gcc valgrind
    $ git clone git://github.com/ajhc/ajhc.git
    $ cd ajhc
    $ git checkout arafura
    $ autoreconf -i
    $ ./configure
    $ make
    $ make install

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

Perhaps you can get internal overview with following compile flow image:

![](https://raw.github.com/ajhc/ajhc/arafura/docs/jhc_compile_flow.png)

## Future plan

* Benchmark (speed, memory size, compare with eLua/mruby/.NET Micro Framework)
* No more depend on Perl (LWP and YAML).
* Port Haskell libraries on haskell-platform to Ajhc.
* Understand jhc's region inference.
* Play with Google Native Client. https://developers.google.com/native-client/
* Write Linux kernel driver with Haskell.
* Get smaller RTS. Benchmark the RTS for running on custom FPGA CPU.
* Start rewritng NetBSD kernel with Ajhc.

## License

* Runtime: [MIT License](https://github.com/ajhc/ajhc/blob/master/rts/LICENSE)
* Haskell libraries: [MIT License](https://github.com/ajhc/ajhc/blob/master/lib/LICENSE)
* The others: [GPLv2 or Later](https://github.com/ajhc/ajhc/blob/arafura/COPYING)

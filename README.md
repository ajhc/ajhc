# [Ajhc](http://ajhc.github.com/) - arafura-jhc [![Build Status](https://travis-ci.org/ajhc/ajhc.png)](https://travis-ci.org/ajhc/ajhc)

A fork of [jhc](http://repetae.net/computer/jhc/).
And also Haskell compiler. 

This project is founded by [Metasepi Project](http://metasepi.masterq.net/).

## How to install

    $ sudo apt-get install make locales autoconf libreadline-dev \
      libwww-perl libconfig-yaml-perl graphviz haskell-platform drift pandoc \
      libghc-readline-dev libghc-utf8-string-dev libghc-hssyck-dev libghc-pandoc-dev
    $ git clone git://github.com/ajhc/ajhc.git
    $ cd ajhc
    $ git checkout arafura
    $ autoreconf -i
    $ ./configure
    $ make
    $ sudo make install

## For developing

Please use the arafura branch, rather than master branch.

* [master branch](https://github.com/ajhc/ajhc/tree/master): Do not touch me, mirror of [jhc's darcs repository](http://repetae.net/dw/darcsweb.cgi?r=jhc).
* [arafura branch](https://github.com/ajhc/ajhc/tree/arafura): For developing Ajhc.

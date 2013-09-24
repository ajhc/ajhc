#!/usr/bin/env sh
sudo apt-get -qq update
sudo apt-get -qq -y install make locales autoconf m4 libconfig-yaml-perl graphviz haskell-platform cpphs pandoc hscolour po4a libgc-dev gcc-multilib valgrind
sudo apt-get -qq -y install libghc-temporary-dev libghc-haskeline-dev libghc-utf8-string-dev libghc-hssyck-dev libghc-test-framework-th-dev libghc-test-framework-hunit-dev libghc-test-framework-quickcheck2-dev libghc-uniplate-dev
cabal update || cabal update
cabal install derive-2.5.11 || cabal install derive-2.5.11

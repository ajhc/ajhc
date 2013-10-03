#!/usr/bin/env sh
wget "http://lambda.haskell.org/platform/download/2013.2.0.0/Haskell Platform 2013.2.0.0 32bit.pkg"
sudo installer -pkg "Haskell Platform 2013.2.0.0 32bit.pkg" -target /
cabal update || cabal update
export HSPACKS="derive temporary haskeline utf8-string HsSyck cpphs test-framework-th test-framework-hunit test-framework-quickcheck2 uniplate"
cabal install --disable-documentation $HSPACKS || cabal --disable-documentation install $HSPACKS

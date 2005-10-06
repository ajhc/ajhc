echo `pwd`
mkdir _darcs
touch _darcs/inventory
[ -e Doc ] || darcs get http://repetae.net/john/repos/Doc
[ -e Boolean ] || darcs get http://repetae.net/john/repos/Boolean
make && ./jhc $* -flint -v test/HelloWorld.hs

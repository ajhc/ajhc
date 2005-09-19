echo `pwd`
mkdir _darcs
touch _darcs/inventory
[ -e Doc ] || darcs get http://repetae.net/john/repos/Doc
[ -e Boolean ] || darcs get http://repetae.net/john/repos/Boolean
make && ./jhc -v /home/john/prog/hs/jhc/jhc/HelloWorld.hs

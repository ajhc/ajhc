#!/bin/zsh
set +x

settitle() {
    [[ -t 1 ]] || return
    print -Pn "\e]2;$1\a"
}

SLEEPFILE=/tmp/compileloop.sleeping
# TIMES=`ls -lt $(cat depend.make | sed -e 's/^HSFILES=//')`
echo 0 > $SLEEPFILE
while true; do
    #TTIMES=`ls -lt $(cat depend.make | sed -e 's/^HSFILES=//')`
    if ( make -q $* )  { settitle "cl: `cat $SLEEPFILE`" ; sleep 1 } else {  settitle "cl: working"; rm -f $SLEEPFILE; echo "changed"; { date ; make $*;  echo $? > $SLEEPFILE } |& tee -a compile.log; sleep 3 }
done


#!/bin/zsh
set +x
SLEEPFILE=/tmp/compileloop.sleeping
TIMES=`ls -lt $(cat depend.make | sed -e 's/^HSFILES=//')`
echo 0 > $SLEEPFILE
while true; do
    TTIMES=`ls -lt $(cat depend.make | sed -e 's/^HSFILES=//')`
    if [[ "$TIMES" !=  "$TTIMES" ]] { TIMES=$TTIMES; rm -f $SLEEPFILE; echo "changed"; { date ; make $*;  echo $? > $SLEEPFILE } |& tee -a compile.log } else {
        sleep 1
    }
done


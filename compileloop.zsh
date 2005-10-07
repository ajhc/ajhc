#!/bin/zsh
set +x
TIMES=`ls -lt $(cat depend.make | sed -e 's/^HSFILES=//')`
while true; do
    TTIMES=`ls -lt $(cat depend.make | sed -e 's/^HSFILES=//')`
    if [[ "$TIMES" !=  "$TTIMES" ]] { TIMES=$TTIMES; echo "changed"; { date ; make $* }|& tee -a compile.log }
    sleep 1
done


#!/bin/zsh

SLEEPFILE=/tmp/compileloop.sleeping
while ! [[ -f $SLEEPFILE ]] { echo -n '.'; sleep 1; }
echo '!'
if [[  `cat $SLEEPFILE` == "0" ]] {
    if [[ -z "$*" ]]; then exec true; else exec $*; fi
}
echo "error compiling.."
tail compile.log
exit 4


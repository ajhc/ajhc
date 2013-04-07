#!/bin/sh

FILENAME=$1

usage () {
    echo "Usage: ajhc_dump_code.sh HASKELL_CODE"
    exit 1
}

if [ "$FILENAME" != "" ] && [ -e $FILENAME ]; then
    :
else
    usage
fi

ajhc $FILENAME --ignore-cache \
 -dcore \
 -dcore-afterlift \
 -dcore-beforelift \
 -dcore-initial \
 -dcore-mangled \
 -dcore-mini \
 -dcore-pass \
 -dcore-steps \
 -ddatatable \
 -ddatatable-builtin \
 -de-alias \
 -de-info \
 -de-verbose \
 -doptimization-stats \
 -drules \
 -drules-spec \
 -dgrin \
 -dgrin-datalog \
 -dgrin-final \
 -dgrin-graph \
 -dgrin-initial \
 -dgrin-normalized \
 -dgrin-posteval \
 -dgrin-preeval \
 -dsteps \
 -dtags \
 -dc \

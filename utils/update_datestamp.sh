#!/bin/sh
CHANGELOG=$1
CONFIGUREAC=$2

set -ex

DATESTAMP=$(grep -o '^[A-Z].*[0-9][0-9][0-9][0-9]' "$CHANGELOG" | sort -n | head -1 |  xargs -n 1 '-d\n' date "+%Y%m%d" -d)
SHORTVERSION=$(sed -ne 's/^AC_INIT(\[.*\],\[\(.*\)\.[0-9]*\])/\1/p' $CONFIGUREAC)
sed -i -e 's/^\(AC_INIT(\[.*\],\[.*\.\)[0-9]*\(\])\)/\1'$DATESTAMP'\2/' $CONFIGUREAC
sed -i -e 's/^REVISION=\([0-9]*\)$/REVISION='$DATESTAMP'/' $CONFIGUREAC
sed -i -e 's/^SHORTVERSION=\([0-9.]*\)$/SHORTVERSION='$SHORTVERSION'/' $CONFIGUREAC

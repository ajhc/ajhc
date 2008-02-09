#!/bin/sh
CHANGELOG=$1
CONFIGUREAC=$2

DATESTAMP=`date -d "\`head $CHANGELOG | grep -o '^.*\d\d\d\d'\`" "+%Y%m%d"`
echo "New Datestamp: $DATESTAMP"
sed -i -e 's/^\(AC_INIT(\[.*\],\[.*\.\)[0-9]*\(\])\)/\1'$DATESTAMP'\2/' $CONFIGUREAC

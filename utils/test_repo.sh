#!/bin/sh

set -x

REPO=`pwd`
BNAME=`basename $REPO`

TDIR=`mktemp -d /var/tmp/test_repo.XXXXXXXX`

cd $TDIR

darcs get $REPO

cd $BNAME

autoreconf -i
./configure
make distcheck

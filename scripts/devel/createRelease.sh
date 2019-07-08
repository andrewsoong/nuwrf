#!/usr/bin/env bash
set -e

REPOSITORY_URL=svn+ssh://progressdirect/svn/nu-wrf/code
NU_WRF_PREFIX=nu-wrf_
NU_WRF_RELEASES=/discover/nobackup/projects/nu-wrf/releases/stable

VERSION_NUMBER=$1
if [ "$VERSION_NUMBER" == "" ]
then
    echo "Usage: $0 <repository-tag>"
    echo 
    echo "Available tags include:"
    svn ls $REPOSITORY_URL/tags 2> /dev/null
    exit 1
fi

REPOSITORY_TAG=$REPOSITORY_URL/tags/releases/$VERSION_NUMBER
NU_WRF_FILENAME=$NU_WRF_PREFIX$VERSION_NUMBER

svn export $REPOSITORY_TAG $NU_WRF_FILENAME
#tar czf $NU_WRF_FILENAME.tgz $NU_WRF_FILENAME
# EMK...Create separate gzip and bzip2 copies of tar file
tar cf $NU_WRF_FILENAME.tar $NU_WRF_FILENAME
bzip2 $NU_WRF_FILENAME.tar
chgrp s0942 $NU_WRF_FILENAME.tar.bz2
chmod 640 $NU_WRF_FILENAME.tar.bz2
mv $NU_WRF_FILENAME.tar.bz2 $NU_WRF_RELEASES

tar czf $NU_WRF_FILENAME.tgz $NU_WRF_FILENAME
chgrp s0942 $NU_WRF_FILENAME.tgz
chmod 640 $NU_WRF_FILENAME.tgz
mv $NU_WRF_FILENAME.tgz $NU_WRF_RELEASES

rm -rf $NU_WRF_FILENAME

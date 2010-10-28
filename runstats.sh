#!/bin/bash

set -x

if [[ -z $1 ]] ; then
  SITE=andrewmoore.com
else
  SITE=$1
fi

VISITORS=/usr/bin/visitors
ACCESSLOG=/var/log/apache2/$SITE/access.log*
WORKDIR=/home/amoore/public_html
OUTPUT=$WORKDIR/stats.html
PREFIX="--prefix http://$SITE --prefix http://www.$SITE "
ZCAT="/bin/zcat -f"
EXCLUDE="--exclude private --exclude 65.165.109.82 --exclude cpe-24-163-202-170.kc.res.rr.com --exclude cake.mooresystems.com --exclude /wiki/skins --exclude usemsgcache --exclude action=raw --exclude TheKCGuy_daily_rss.xml --exclude skins"

$ZCAT $ACCESSLOG | \
          $VISITORS \
	  $EXCLUDE \
          $PREFIX \
	  --trails -G \
          -AKZX \
          -f $OUTPUT \
          -
$ZCAT $ACCESSLOG | \
          $VISITORS \
          $EXCLUDE \
	  $PREFIX \
          --graphviz \
          - \
          > $WORKDIR/visitors.dot

/usr/bin/dot $WORKDIR/visitors.dot -Tpng > $WORKDIR/visitors.png



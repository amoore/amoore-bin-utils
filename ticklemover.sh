#!/bin/bash

set -x

tickleroot=/home/amoore/docs/tickler

today=$(date '+%e'| sed s/\ //)
tomorrow=$(date --date='tomorrow' '+%e'| sed s/\ //)

todaydir=$tickleroot/$today
tomorrowdir=$tickleroot/$tomorrow

if [[ -d $todaydir ]] ; then
   mv $todaydir/* $tomorrowdir
fi


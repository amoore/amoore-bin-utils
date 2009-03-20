#!/bin/bash

set -x

say_greeting() {
    # flite -t "Good Morning. It is now $(date '+%A %B %e %l %M %p')" 
    flite -t "Good Morning. It is now $(date '+%l %M %p on %A %B %e')"
    flite -t "$(/home/amoore/bin/weather.pl)"

}

copy_podcasts() {

    PODSOURCE=/home/amoore/simpleshare/bashpodder
    PODDEST=/media/usb0
    
    # make sure that the flash drive is mounted
    if [ ! -d $PODDEST/lost+found ] ; then
	echo "USB drive is not mounted. Cannot copy podcasts. Exiting."
	exit 1
    fi
    
    # purge flash drive of old podcast directories
    sudo rm -rf $PODDEST/2008*
    
    # copy podcasts from bashpodder directory to flashdrive
    for podday in $(ls -d /home/amoore/simpleshare/bashpodder/2008* | sort -r) ; do
	echo "copying $podday"
	sudo cp -a $podday $PODDEST
	if [ $? -eq 1 ] ; then
	    echo "failed to copy $podday"
	    break
	else
	    echo "removing $podday"
	    sudo rm -rf $podday
	fi
    done
}


say_greeting
# copy_podcasts




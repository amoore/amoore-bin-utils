#!/usr/bin/env python

import feedparser
import urllib
import shutil

d = feedparser.parse('Put the address to the RSS feed here.')
destination_directory = '/home/amoore/tmp/photos'

for i in d['items']:
    # print i
    for e in i['enclosures']:
        thislink = e['href']
        print 'fetching ' + thislink + '...'
        ( filename, headers ) = urllib.urlretrieve( thislink )
        print 'saving ' + filename + ' to ' + destination_directory
        shutil.copy( filename, destination_directory )


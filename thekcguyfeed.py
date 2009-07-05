#!/usr/bin/python
#
# generate the daily rss feed for TheKCGuy.com. 
# 
# based on the program that generates RSS feeds of Wikipedia's Featured Articles and Picture of the Day.
# http://en.wikipedia.org/wiki/User:Skagedal/Fafafa
#
# 
#

import sys
import os
import string
import datetime
import time
import urllib
import re
import cPickle
from HTMLParser import HTMLParser
import PyRSS2Gen
import getopt

#
# Settings
#

# ...General
settings = {
	'rss_webmaster': 'thekcguy@gmail.com',
	'program_name':  'Fafafa',
	'version':       '0.8.1'
	}

# ...for Featured Articles
settings_fa = {
	'entries': 10,
	'output_filename': '/home/www/thekcguy.com/TheKCGuy_daily_rss.xml',
	'cache_filename':  '/tmp/TheKCGuy_daily.pickle',
	# 'output_filename': '/tmp/TheKCGuy_daily_rss_test.xml',
	# 'cache_filename':  '/tmp/TheKCGuy_daily_test.pickle',
	'url':             'http://www.thekcguy.com/Kansas_City/%(month)s_%(day)d%%2C_%(year)d',
	'rss_title':       'TheKCGuy\'s Kansas City News and Events',
	'rss_link':        'http://www.thekcguy.com/',
	'rss_description': 'TheKCGuy\'s Kansas City News and Events'
	}


# Find the URL of the article for a specific date
#
# ASSUMPTION: articles for a specific day, say May 30, 2006, can be found at:
# [[May_30,_2006]]

months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

def get_url(date):
	return settings['url'] % \
		{ 'month': months[date.month - 1], 'day': date.day, 'year': date.year }

# Subclassing of URLopener - sets "User-agent: ", which Wikipedia requires to be set
# to something else than the default "Python-urllib"

class MyURLopener(urllib.URLopener):
	version = settings['program_name'] + "/" + settings['version']

# Caching of HTML from Wikipedia

class CacheItem:
	def __init__(self, html, fetchtime):
		self.html = html
		self.fetchtime = fetchtime
			
class WPCache:

	def __init__(self, cachefilename):
		self.url_opener = MyURLopener()
		self.filename = cachefilename
		if (os.path.exists(cachefilename)):
			file = open(cachefilename)
			self.cache = cPickle.load(file)
			file.close()
		else:
			self.cache = {}
	
	def get_html(self, date):
		if date in self.cache:
			return self.cache[date].html
		else:
			if ('potd' in settings and settings['potd'] and (date != datetime.date.today())):
				return False
			html = self.url_opener.open(get_url(date)).read()
			cacheitem = CacheItem(html, time.gmtime())
			self.cache[date] = cacheitem
			return html

	def too_old(self, date):
		return (datetime.date.today() - date).days > settings['entries']

	# Weed out old entries, so cache doesn't get big
	def weed_out_old(self):
		self.cache = dict([x for x in self.cache.items() if not self.too_old(x[0])])
		
	def remove_today(self):
		today = datetime.date.today()
		del self.cache[today]

	def save(self):
		self.weed_out_old()
		file = open(self.filename, "w")
		p = cPickle.Pickler(file)
		p.dump(self.cache)
		

# this HTMLParser will do two things:
# 1) find the content of the article between the start and end comment tags that mediawiki adds
# 2) make relative URLs into absoulute URLs
class MyHTMLParser(HTMLParser):
	in_content = False
	current_content = ''
	def handle_comment(self, comment):
		''' ASSUMPTION: Content of article is between <!-- start content --> and <!-- end content -->'''
		if comment == ' start content ':
			self.in_content = True
		if comment == ' end content ':
			self.in_content = False
	def handle_data(self, data):
		if self.in_content:
			self.current_content += data
	def handle_starttag( self, tag, attrs ):
		if self.in_content:
			tag_contents = [ tag ]
			for attr in attrs:
				if tag == 'a' and attr[0] == 'href' and attr[1][0] =='/':
					attr = ( attr[0], self.make_url_absolute( attr[1] ), attr[2:] )
				tag_contents.append( '%(name)s="%(value)s"' % { 'name': attr[0], 'value': attr[1] } )
			self.current_content += '<' + string.join( tag_contents, ' ' ) + '>'
	def handle_endtag( self, tag ):
		if self.in_content:
			self.current_content += '</%(tag)s>' % { 'tag': tag }
	def get_content(self):
		return self.current_content
	def make_url_absolute(self, url):
		if settings['rss_link'].endswith('/'):
			return settings['rss_link'][0:-1] + url
		else:
			return settings['rss_link'] + url

# Get the content of the article
#
def get_content(s):
	parser  = MyHTMLParser()
	parser.feed(s)
	content = parser.get_content()
	parser.close()
	return content

# Get title of article - expects html filtered by get_content
#
# ASSUMPTION: 
# * The text inside the first bolded a-tag is the title
# ** If that can't be found, the first bolded text is the title
# *** If that can't be found, the first a-tag is the title
# **** If all else fails, return '(unknown title)'

res_title = [re.compile('<b><a[^>]*>([^<]*)</a>'),
	     re.compile('<b>([^<]*)</b>'),
	     re.compile('<a[^>]*>([^<]*)</a>')]
def get_title(s):
	# Recursive helper function
	def get_title_r(res, s):
		if res == []:
			return '(unknown title)'
		else:
			try:
				m = res[0].search(s)
				s = m.group(1)
				s = s[0].upper() + s[1:]
				return s
			except:
				return get_title_r(res[1:], s)

	return get_title_r(res_title, s)

# Create RSS item - expects html filtered by get_content

def rss_item(date, content):
	if 'no_title' in settings and settings['no_title']:
		title = "%s %d" % (months[date.month - 1], date.day)
	else:
		title = "%s %d: %s" % (months[date.month - 1], date.day, get_title(content))
	return PyRSS2Gen.RSSItem(
		title       = title,
		link        = get_url( date ),
		description = content,
		pubDate	    = datetime.datetime(date.year, date.month, date.day),
		guid	    = PyRSS2Gen.Guid( get_url( date ) ),
		)

# Puts the final RSS together

def make_rss(items):
	rss = PyRSS2Gen.RSS2(
		title         = settings['rss_title'],
		link          = settings['rss_link'],
		description   = settings['rss_description'],
		lastBuildDate = datetime.datetime.now(),
		items         = items
		)
        return rss

# Main
def main():
	# These should probably be command line options, but for now they're hard-coded.
	settings.update(settings_fa)

	today = datetime.date.today()
	one_day = datetime.timedelta(days = 1)

	cache = WPCache(settings['cache_filename'])
	
	# process command line arguments
	try:
		opts, args = getopt.getopt(sys.argv[1:], "", ["redo-today"])
	except getopt.GetoptError, err:
		# print help information and exit:
		print str(err)
		usage()
		sys.exit(2)
	output = None
	verbose = False
	for o, a in opts:
		if o == "--redo-today":
			cache.remove_today()
		else:
			assert False, "unhandled option"

	dates = [today - one_day*x for x in range(settings['entries'])]

	def item(date):
		html = cache.get_html(date)
		if html:
			content = get_content(cache.get_html(date))
		else:
			content = ''
		return rss_item(date, content)

	items = [item(date) for date in dates]
	rss = make_rss(items)

	# Write to file
	rss.write_xml(open(settings['output_filename'], "w"))
	
	cache.save()

# Don't run if we're imported

if __name__ == '__main__':
	main()

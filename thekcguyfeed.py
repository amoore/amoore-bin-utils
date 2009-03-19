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
import xml.sax.saxutils
from HTMLParser import HTMLParser

#
# Settings
#

# ...General
settings = {
	'rss_webmaster': 'thekcguy@gmail.com',
	'program_name': 'Fafafa',
	'version': '0.8.1'
	}

# ...for Featured Articles
settings_fa = {
	'entries': 10,
	'output_filename': '/tmp/example.xml',
	'cache_filename': '/tmp/example.pickle',
	'url': 'http://www.thekcguy.com/Kansas_City/%(month)s_%(day)d%%2C_%(year)d',
	'rss_title': 'TheKCGuy\'s Kansas City News and Events',
	'rss_link': 'http://www.thekcguy.com/',
	'rss_description': 'RSS feed of the daily news and events from thekcguy.com'
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

def too_old(date):
	return (datetime.date.today() - date).days > settings['entries']

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

	# Weed out old entries, so cache doesn't get big
	def weed_out_old(self):
		self.cache = dict([x for x in self.cache.items() if not too_old(x[0])])
		
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
			print 'entering content'
			self.in_content = True
		if comment == ' end content ':
			print 'leaving content'
			self.in_content = False
	def handle_data(self, data):
		if self.in_content:
			self.current_content += data
	def handle_starttag( self, tag, attrs ):
		if self.in_content:
			attribute_string = ''
			for attr in attrs:
				if tag == 'a' and attr[0] == 'href':
					attr = ( attr[0], self.make_url_absolute( attr[1] ) )
				attribute_string += '%(name)s="%(value)s"' % { 'name': attr[0], 'value': attr[1] } + ' '
			self.current_content += '<%(tag)s %(attribute_string)s>' % \
				{ 'tag': tag, 'attribute_string' : attribute_string }
	def handle_endtag( self, tag ):
		if self.in_content:
			self.current_content += '</%(tag)s>' % { 'tag': tag }
	def get_content(self):
		return self.current_content
	def make_url_absolute(self, url):
		return settings['rss_link'] + url

# Get the content of the article
#
def get_content(s):
	parser = MyHTMLParser()
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
	return """<item>
<title>%(title)s</title>
<link>%(url)s</link>
<description>%(escaped_content)s</description>
</item>
""" % {
		'title': title,
		'url': get_url(date),
		'escaped_content': xml.sax.saxutils.escape(content)}

# Puts the final RSS together

def rss(items):
	return """<?xml version="1.0" encoding="UTF-8"?>
	<rss version="2.0" xmlns:blogChannel="http://backend.userland.com/blogChannelModule">

<channel>
<title>%(rss_title)s</title>
<link>%(rss_link)s</link>
<description>%(rss_description)s</description>
<language>en-us</language>
<copyright>GNU Free Documentation License</copyright>
<lastBuildDate>%(build_date)s</lastBuildDate>
<docs>http://blogs.law.harvard.edu/tech/rss</docs>
<webMaster>%(webmaster)s</webMaster>
<generator>%(generator)s</generator>

%(items)s

</channel>
</rss>
""" % {
'rss_title': settings['rss_title'],
'rss_link': settings['rss_link'],
'rss_description': settings['rss_description'],
'webmaster': settings['rss_webmaster'],
'build_date': time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime()),
'items': items,
'generator': settings['program_name'] + " " + settings['version'] }

# Main

def main():
	# Primitive command line parsing
	settings.update(settings_fa)

	today = datetime.date.today()
	one_day = datetime.timedelta(days = 1)

	cache = WPCache(settings['cache_filename'])
	
	dates = [today - one_day*x for x in range(settings['entries'])]

	def item(date):
		html = cache.get_html(date)
		if html:
			content = get_content(cache.get_html(date))
		else:
			content = ''
		return rss_item(date, content)

	# Iterate over the items
	items = string.join([item(date) for date in dates], "")
	the_rss = rss(items)

	# Write to file
	file = open(settings['output_filename'], "w")
	file.write(the_rss)
	file.close()
	
	cache.save()

# Don't run if we're imported

if __name__ == '__main__':
	main()

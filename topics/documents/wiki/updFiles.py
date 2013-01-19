#!/usr/local/bin/python
# -*- coding: utf-8 -*-

import os

fmt = '## Relevant files\n'

maps = {}

# traversing Rascal sources
for root, dirs, filenames in os.walk('/Users/zaytsev/projects/slps/shared/rascal/src/'):
	for f in filter(lambda x:x.endswith('.rsc'),filenames):
		rd = open(os.path.join(root,f),'r')
		txt = ''.join(rd.readlines())
		if txt.find('@wiki')>-1:
			for where in txt.split('@wiki{')[1].split('}')[0].split(','):
				g = where[0].upper()+where[1:]
				good = os.path.join(root, f).split('projects/slps/')[1]
				if g in maps.keys():
					maps[g].append(good)
				else:
					maps[g] = [good]
		rd.close()

# traversing command line tools, generators, wrappers and python repo
for root, dirs, filenames in os.walk('/Users/zaytsev/projects/slps/shared/'):
	for f in filenames:
		rd = open(os.path.join(root,f),'r')
		txt = ''.join(rd.readlines())
		if txt.find('# wiki: ')>-1:
			for where in txt.split('# wiki:')[1].split('\n')[0].split(','):
				g = where[0].upper()+where[1:]
				good = os.path.join(root, f).split('projects/slps/')[1]
				if g in maps.keys():
					maps[g].append(good)
				else:
					maps[g] = [good]
		rd.close()

for aff in maps.keys():
	links = []
	after = []
	try:
		f = open('texts/%s.md' % aff,'r')
		lines = f.readlines()
		if fmt in lines:
			before = lines[:lines.index(fmt)]
			nyet = False
			for line in lines[lines.index(fmt)+1:]:
				if nyet:
					after.append(line)
				elif line.strip() or line.startswith('##'):
					links.append(line)
				else:
					nyet = True
		else:
			before = lines
		f.close()
		# print 'Yes',aff
	except IOError:
		print 'No',aff
		before = []
		links = []
	# process links
	# * [`shared/xsd/xbgf.xsd`](../blob/master/shared/xsd/xbgf.xsd)
	links = map(lambda x:x.split('`')[1],links)
	for link in maps[aff]:
		if link not in links:
			links.append(link)
			print 'Added',link,'to',aff
	f = open('texts/%s.md' % aff,'w')
	for line in before:
		f.write(line)
	f.write(fmt)
	for link in links:
		f.write('* [`%s`](../blob/master/%s)\n' % (link,link))
	f.write('\n')
	for line in after:
		f.write(line)
	f.close()

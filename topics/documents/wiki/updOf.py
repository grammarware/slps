#!/usr/local/bin/python
# -*- coding: utf-8 -*-

import os

parts = {}

# * Abridge is a part of [[XBGF]]

for root, dirs, filenames in os.walk('texts'):
	for f in filenames:
		rd = open(os.path.join(root,f),'r')
		lines = rd.readlines()
		changed = False
		for i in range(0,len(lines)):
			if lines[i].startswith('* ') and lines[i].find('is a part of')>-1:
				what = lines[i].split(' is a part of ')[0][2:]
				expect = f.split('.')[0]
				if what != expect:
					print '%s claimed to be %s, fixed.' % (expect, what)
					changed = True
					lines[i] = '* %s is a part of %s' % (expect, lines[i].split(' is a part of ')[-1])
				whole = lines[i].split('[[')[1].split(']]')[0]
				if whole not in parts.keys():
					parts[whole] = []
				parts[whole].append(expect)
		rd.close()
		if changed:
			rd = open(os.path.join(root,f),'w')
			for line in lines:
				rd.write(line)
			rd.close()

for big in parts.keys():
	f = open('texts/%s.md' % big, 'r')
	links = [p.split(']]')[0].upper() for p in ''.join(f.readlines()).split('[[')[1:]]
	for small in parts[big]:
		if small.upper() not in links:
			print '%s does not refer to %s, but totally should.' % (big, small)
	f.close()

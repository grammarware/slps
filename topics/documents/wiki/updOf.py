#!/usr/local/bin/python
# -*- coding: utf-8 -*-

import os

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
		rd.close()
		if changed:
			rd = open(os.path.join(root,f),'w')
			for line in lines:
				rd.write(line)
			rd.close()

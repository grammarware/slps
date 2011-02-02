#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF
import levels

def hei(adg):
	paths = []
	for i in range(0,len(adg)):
		paths.append(longest(i,adg))
	return max(paths)

def longest(n,adg):
	if len(adg[n])==0:
		return 0
	else:
		paths = []
		for x in adg[n]:
			paths.append(1+longest(x,adg))
		return max(paths)

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates an HEI metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	adg = levels.getADigraph(bgf)
	#print 'HEI =',hei(adg)
	print hei(adg)
	sys.exit(0)

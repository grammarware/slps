#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF
import defined
import used

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a BOTTOM metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	bottoms = []
	definednts = defined.defined(bgf)
	for nt in used.used(bgf):
		if nt not in definednts:
			bottoms.append(nt)
	#print 'BOTTOM =',len(bottoms)
	print len(bottoms)
	sys.exit(0)

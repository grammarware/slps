#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF

def defined(g):
	nts = []
	for p in g.prods:
		if p.nt not in nts:
			nts.append(p.nt)
	return nts
	
if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a DEFINED metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	#print 'DEFINED =',len(defined(bgf))
	print len(defined(bgf))
	sys.exit(0)

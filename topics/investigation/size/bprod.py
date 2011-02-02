#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a non-normalised PROD metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	#print 'BPROD =',len(bgf.prods)
	print len(bgf.prods)
	sys.exit(0)

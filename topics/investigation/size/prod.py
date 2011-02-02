#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a normalised PROD metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	prod = 0
	for p in bgf.prods:
		if p.expr.wrapped.__class__.__name__ == 'Choice':
			prod += len(p.expr.wrapped.data)
		else:
			prod += 1
	#print 'PROD =',prod
	print prod
	sys.exit(0)

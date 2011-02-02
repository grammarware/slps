#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a TERM metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	term = []
	for p in bgf.prods:
		for n in p.expr.wrapped.getXml().findall('.//terminal'):
			if n.text not in term:
				term.append(n.text)
	#print 'TERM =',len(term)
	print len(term)
	sys.exit(0)

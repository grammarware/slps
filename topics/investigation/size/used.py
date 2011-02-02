#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF

def used(g):
	nts = []
	for p in g.prods:
		for n in p.expr.wrapped.getXml().findall('.//nonterminal'):
			if n.text not in nts:
				nts.append(n.text)
	return nts

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a USED metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	#print 'USED =',len(used(bgf))
	print len(used(bgf))
	sys.exit(0)

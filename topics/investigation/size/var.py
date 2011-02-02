#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF

def var(g):
	nts = []
	for p in g.prods:
		if p.nt not in nts:
			nts.append(p.nt)
		for n in p.expr.wrapped.getXml().findall('.//nonterminal'):
			if n.text not in nts:
				nts.append(n.text)
	return nts
	
if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a VAR metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	#print 'VAR =',len(var)
	print len(var(bgf))
	sys.exit(0)

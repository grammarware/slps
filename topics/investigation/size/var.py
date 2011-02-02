#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a VAR metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	var = []
	for p in bgf.prods:
		if p.nt not in var:
			var.append(p.nt)
		for n in p.expr.wrapped.getXml().findall('.//nonterminal'):
			if n.text not in var:
				var.append(n.text)
	#print 'VAR =',len(var)
	print len(var)
	sys.exit(0)

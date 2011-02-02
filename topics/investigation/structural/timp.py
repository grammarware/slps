#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
import levels
sys.path.append('../../../shared/python')
import BGF

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a TIMP metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	cg = levels.getClosure(levels.getCallGraph(bgf))
	n = len(cg)
	e = sum(map(len,cg.values()))
	# Power and Malloy made two mistakes:
	# (1) the number of edges in a complete directed graph is n(n-1), not n(n-1)/2, as in a complete undirected graph!
	# (2) we don't have to substract another 1 from the number of nonterminals to account for a start symbol
	# To compute TIMP exactly as they intended to, run this:
	# print '%.1f' % (100*2*(e-n+1)/(0.0+(n-1)*(n-2)))
	# To run our fixed version, uncomment this:
	print '%.1f' % (100*(e-n+1)/(0.0+n*(n-1)))
	sys.exit(0)

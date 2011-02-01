#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import os
import sys
sys.path.append('../../../shared/python')
import slpsns
import BGF
import elementtree.ElementTree as ET

calls = {}

if __name__ == "__main__":
	# |>*
	if len(sys.argv) != 2:
		#print 'This tool generates a BNF representation for any given BGF grammar.'
		print 'Usage:'
		print '      bgf2bnf <bgf-input> <bnf-output>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	for p in bgf.prods:
		if p.nt not in calls.keys():
			calls[p.nt] = []
		for n in p.expr.wrapped.getXml().findall('.//nonterminal'):
			if n.text not in calls[p.nt]:
				calls[p.nt].append(n.text)
	#for n in calls.keys():
	#	print n,'▻',calls[n]
	#print '--------------------'
	for n in calls.keys():
		for x in calls[n]:
			if x not in calls.keys():
				calls[x] = []
			for y in calls[x]:
				if y not in calls[n]:
					calls[n].append(y)
		calls[n].sort()
	#for n in calls.keys():
	#	print n,'▻*',calls[n]
	#print '--------------------'
	unassigned = calls.keys()
	levels = []
	while len(unassigned)>0:
		nt = unassigned[0]
		levels.append([])
		levels[-1].append(nt)
		unassigned = unassigned[1:]
		for n in calls[nt]:
			if nt in calls[n]:
				levels[-1].append(n)
				if n in unassigned:
					unassigned.remove(n)
	print 'LEV =',len(levels)
	#for l in levels:
	#	print l
	sys.exit(0)

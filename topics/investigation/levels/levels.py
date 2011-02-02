#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import os
import sys
sys.path.append('../../../shared/python')
import slpsns
import BGF
import elementtree.ElementTree as ET

calls = {}

def getADigraph(g):
	calls = getCallGraph(g)
	levels = getLevels(g)
	adg = []
	for i in range(0,len(levels)):
		adg.append([])
		for j in range(0,len(levels)):
			if i == j:
				# to ensure acyclicity
				continue
			for n in levels[i]:
				for m in levels[j]:
					if m in calls[n] and j not in adg[i]:
						adg[i].append(j)
	return adg

def getLevels(g):
	calls = getCallGraph(g)
	calls = getClosure(calls)
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
	return levels

def getCallGraph(g):
	calls = {}
	for p in g.prods:
		if p.nt not in calls.keys():
			calls[p.nt] = []
		for n in p.expr.wrapped.getXml().findall('.//nonterminal'):
			if n.text not in calls[p.nt]:
				calls[p.nt].append(n.text)
			if n.text not in calls.keys():
				calls[n.text] = []
	#for n in calls.keys():
	#	print n,'▻',calls[n]
	#print '--------------------'
	return calls

def getClosure(cg):
	calls = cg.copy()
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
	return calls

if __name__ == "__main__":
	# |>*
	if len(sys.argv) != 2:
		print 'This tool generates grammatical levels for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	for l in getLevels(bgf):
		print l
	sys.exit(0)

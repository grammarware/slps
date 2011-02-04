#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import os, sys, math
import slpsns
import elementtree.ElementTree as ET
import BGF

######################################################################
##########                     Size metrics                 ##########
######################################################################
# TERM - number of terminal symbols in the grammar
def TERM(g):
	return len(term(g))
def term(g):
	ts = []
	for p in g.prods:
		for n in p.expr.wrapped.getXml().findall('.//terminal'):
			if n.text not in ts:
				ts.append(n.text)
	return ts

# VAR - number of nonterminal symbols in the grammar
def VAR(g):
	return len(var(g))
def var(g):
	nts = []
	for p in g.prods:
		if p.nt not in nts:
			nts.append(p.nt)
		for n in p.expr.wrapped.getXml().findall('.//nonterminal'):
			if n.text not in nts:
				nts.append(n.text)
	return nts

# VAL - number of values utilised in the grammar
def VAL(g):
	return len(val(g))
def val(g):
	vals = []
	for v in g.getXml().findall('.//value'):
		if v.text not in vals:
			vals.append(v.text)
	return vals

# LABS - number of production labels and expression selectors used in the grammar
def LAB(g):
	return len(lab(g))
def lab(g):
	# returns the number of production labels used in the grammar
	labs = []
	for v in g.prods:
		if v.label and v.label not in labs:
			labs.append(v.label)
	for v in g.getXml().findall('.//selectable'):
		if v.findtext('selector') not in labs:
			labs.append(v.findtext('selector'))
	return labs

# USED - number of nonterminal symbols used in the grammar
def USED(g):
	return len(used(g))
def used(g):
	nts = []
	for p in g.prods:
		for n in p.expr.wrapped.getXml().findall('.//nonterminal'):
			if n.text not in nts:
				nts.append(n.text)
	return nts

# DEFD - number of nonterminal symbols defined by the grammar 
def DEFD(g):
	return len(defd(g))
def defd(g):
	nts = []
	for p in g.prods:
		if p.nt not in nts:
			nts.append(p.nt)
	return nts

# TOP - number of top (defined but not used) nonterminal symbols in the grammar
def TOP(g):
	return len(top(g))
def top(g):
	tops = []
	usednts = used(g)
	for nt in defd(g):
		if nt not in usednts:
			tops.append(nt)
	return tops

# BOT - number of bottom (used but not defined) nonterminal symbols in the grammar
def BOT(g):
	return len(bot(g))
def bot(g):
	bottoms = []
	definednts = defd(g)
	for nt in used(g):
		if nt not in definednts:
			bottoms.append(nt)
	return bottoms

# BPROD - number of productions in the grammar (storage point of view)
def BPROD(g):
	return len(g.prods)

# PROD - number of productions in the grammar (classic point of view)
def PROD(g):
	prod = 0
	for p in g.prods:
		if p.expr.wrapped.__class__.__name__ == 'Choice':
			prod += len(p.expr.wrapped.data)
		else:
			prod += 1
	return prod

######################################################################
##########               Structural metrics                 ##########
######################################################################
# first some general functions for internal use
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
	return calculateLevels(getClosure(getCallGraph(g)))

#
def calculateLevels(calls):
	unassigned = calls.keys()
	levels = []
	while len(unassigned)>0:
		nt         = unassigned[0]
		unassigned = unassigned[1:]
		levels.append([])
		levels[-1].append(nt)
		for n in calls[nt]:
			if (nt in calls[n]) and (n not in levels[-1]) and (n in unassigned):
				levels[-1].append(n)
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
	# simple copy doesn't work because of the inner arrays
	#calls = cg.copy()
	calls = {}
	for x in cg.keys():
		calls[x]=cg[x][:]
	for n in calls.keys():
		for x in calls[n]:
			for y in calls[x]:
				if y not in calls[n]:
					calls[n].append(y)
		calls[n].sort()
	#for n in calls.keys():
	#	print n,'▻*',calls[n]
	#print '--------------------'
	return calls

#
def makeOneStep(cg):
	calls = cg.copy()
	for n in cg.keys():
		for x in cg[n]:
			calls[n] = union(calls[n],cg[x])
		calls[n].sort()
	#for n in calls.keys():
	#	print n,'▻*',calls[n]
	#print '--------------------'
	return calls

# DEP - cardinality of the largest grammatical level
def DEP(g):
	return max(map(len,getLevels(g)))

# LEV - number of grammatical levels
def LEV(g):
	return len(getLevels(g))

# CLEV - number of grammatical levels normalised by the number of nonterminals
def CLEV(g):
	return 100*LEV(g)/(0.0+VAR(g))

# RLEV - number of recursive grammatical levels
def RLEV(g):
	cg = getCallGraph(g)
	return len(filter(lambda x:(len(x)>1)or(x[0] in cg[x[0]]),getLevels(g)))

# NLEV - number of non-trivial grammatical levels
def NLEV(g):
	cg = getCallGraph(g)
	return len(filter(lambda x:len(x)>1,getLevels(g)))

# HEI - the longest chain of grammatical levels in their ordered directed graph
def longest(n,adg):
	if len(adg[n])==0:
		return 0
	else:
		paths = []
		for x in adg[n]:
			paths.append(1+longest(x,adg))
		return max(paths)

def HEI(g):
	adg = getADigraph(g)
	paths = []
	for i in range(0,len(adg)):
		paths.append(longest(i,adg))
	return max(paths)

# TIMP - tree impurity
def TIMP(g):
	return impurityOfCallGraph(getClosure(getCallGraph(g)))

# TIMP - tree impurity of the immediate successor graph
def TIMPI(g):
	return impurityOfCallGraph(getCallGraph(g))

def impurityOfCallGraph(cg):
	n = len(cg)
	e = sum(map(len,cg.values()))
	if n<2:
		return 100
	else:
		# Power and Malloy made two mistakes:
		# (1) the number of edges in a complete directed graph is n(n-1), not n(n-1)/2, as in a complete undirected graph!
		# (2) we don't have to substract another 1 from the number of nonterminals to account for a start symbol
		# To compute TIMP exactly as they intended to, run this:
		# return (100*2*(e-n+1)/(0.0+(n-1)*(n-2)))
		# To run our fixed version, uncomment this:
		return (100*(e-n+1)/(0.0+n*(n-1)))

######################################################################
##########               Complexity metrics                 ##########
######################################################################
# MCC - McCabe cyclomatic complexity
def mccabe(node):
	if node.__class__.__name__ in ('Production','Selectable'):
		return mccabe(node.expr)
	elif node.__class__.__name__ == 'Expression':
		return mccabe(node.wrapped)
	elif node.__class__.__name__ == 'Marked':
		return mccabe(node.data)
	elif node.__class__.__name__ in ('Plus','Star','Optional'):
		return 1+mccabe(node.data)
	elif node.__class__.__name__ in ('Terminal','Nonterminal','Epsilon','Any','Empty','Value'):
		return 0
	elif node.__class__.__name__ == 'Choice':
		return len(node.data)-1 + sum(map(mccabe,node.data))
	elif node.__class__.__name__ == 'Sequence':
		return sum(map(mccabe,node.data))
	else:
		print 'How to deal with',node.__class__.__name__,'?'
		return 0

def MCC(g):
	# classic part
	usual = sum(map(mccabe,g.prods))
	# account for the grammar having multiple productions per nonterminal
	alt = 0
	passed = []
	for p in g.prods:
		if p.nt in passed:
			alt += 1
		else:
			passed.append(p.nt)
	return usual + alt

# AVS - average size of the right hand side of grammar productions
def rhssize(node):
	if node.__class__.__name__ in ('Production','Selectable'):
		return rhssize(node.expr)
	elif node.__class__.__name__ == 'Expression':
		return rhssize(node.wrapped)
	elif node.__class__.__name__ == 'Marked':
		return rhssize(node.data)
	elif node.__class__.__name__ in ('Plus','Star','Optional'):
		return rhssize(node.data)
	elif node.__class__.__name__ in ('Terminal','Nonterminal','Value'):
		return 1
	elif node.__class__.__name__ in ('Epsilon','Any','Empty'):
		return 0
	elif node.__class__.__name__ in ('Choice','Sequence'):
		return sum(map(rhssize,node.data))
	else:
		print 'How to deal with',node.__class__.__name__,'?'
		return 0

def AVS(g):
	return sum(map(rhssize,g.prods))/(0.0+VAR(g))

def AVSp(g):
	return sum(map(rhssize,g.prods))/(0.0+PROD(g))

# HAL - Halstead effort
def opr(node):
	# number of occurrences of operators
	if node.__class__.__name__ == 'Grammar':
		return sum(map(opr,node.prods))
	elif node.__class__.__name__ == 'Production':
		return opr(node.expr)
	elif node.__class__.__name__ == 'Selectable':
		return 1+opr(node.expr)
	elif node.__class__.__name__ == 'Expression':
		return opr(node.wrapped)
	elif node.__class__.__name__ == 'Marked':
		return 1+opr(node.data)
	elif node.__class__.__name__ in ('Plus','Star','Optional'):
		return 1+opr(node.data)
	elif node.__class__.__name__ in ('Terminal','Nonterminal','Value'):
		return 0
	elif node.__class__.__name__ in ('Epsilon','Any','Empty'):
		return 1
	elif node.__class__.__name__ in ('Choice','Sequence'):
		return sum(map(opr,node.data))
	else:
		print 'How to deal with',node.__class__.__name__,'?'
		return 0
		
def opd(node):
	# number of occurrences of operands
	if node.__class__.__name__ == 'Grammar':
		return sum(map(opd,node.prods))
	elif node.__class__.__name__ == 'Production':
		if node.label:
			return 2+opd(node.expr)
		else:
			return 1+opd(node.expr)
	elif node.__class__.__name__ == 'Selectable':
		return 1+opd(node.expr)
	elif node.__class__.__name__ == 'Expression':
		return opd(node.wrapped)
	elif node.__class__.__name__ == 'Marked':
		return opd(node.data)
	elif node.__class__.__name__ in ('Plus','Star','Optional'):
		return opd(node.data)
	elif node.__class__.__name__ in ('Terminal','Nonterminal','Value'):
		return 1
	elif node.__class__.__name__ in ('Epsilon','Any','Empty'):
		return 0
	elif node.__class__.__name__ in ('Choice','Sequence'):
		return sum(map(opd,node.data))
	else:
		print 'How to deal with',node.__class__.__name__,'?'
		return 0

def union(a,b):
	c = a[:]
	for x in b:
		if x not in c:
	 		c.append(x)
	return c

def allOperators(node):
	if node.__class__.__name__ == 'Grammar':
		return reduce(union,map(allOperators,node.prods),[])
	elif node.__class__.__name__ == 'Production':
		return allOperators(node.expr)
	elif node.__class__.__name__ == 'Selectable':
		return union(allOperators(node.expr),node.__class__.__name__)
	elif node.__class__.__name__ == 'Expression':
		return allOperators(node.wrapped)
	elif node.__class__.__name__ in ('Plus','Star','Optional','Marked'):
		return union(allOperators(node.data),node.__class__.__name__)
	elif node.__class__.__name__ in ('Terminal','Nonterminal','Value'):
		return []
	elif node.__class__.__name__ in ('Epsilon','Any','Empty'):
		return [node.__class__.__name__]
	elif node.__class__.__name__ in ('Choice','Sequence'):
		return reduce(union,map(allOperators,node.data),[])
	else:
		print 'How to deal with',node.__class__.__name__,'?'
		return 0

# Halstead preparations
def hal_mu1(g):
	# Number of unique operators
	# Selectable, Marked, Plus, Star, Optional, Epsilon, Empty, Any, Choice, Sequence
	#mu1 = 10
 	return len(allOperators(g))
def hal_mu2(g):
	# Number of unique operands
	return VAR(g) + TERM(g) + VAL(g) + LAB(g)
def hal_eta1(g):
	# Total occurrences of operators
	return opr(g)
def hal_eta2(g):
	# Total occurrences of operands
	return opd(g)

# HALEN - Halstead length
def HALEN(g):
	eta1 = hal_eta1(g)
	eta2 = hal_eta2(g)
	hal = eta1 + eta2
	return hal

# HAVOC - Halstead vocabulary
def HAVOC(g):
	mu1 = hal_mu1(g)
	mu2 = hal_mu2(g)
	hal = mu1 + mu2
	return hal

# HAVOL - Halstead volume
def HAVOL(g):
	hal = HALEN(g)*math.log(HAVOC(g),2)
	return hal

# HADIF - Halstead difficulty
def HADIF(g):
	mu1 = hal_mu1(g)
	mu2 = hal_mu2(g)
	eta2 = hal_eta2(g)
	hal = (mu1*eta2) / (2.0*mu2)
	return hal

# HADIF - Halstead effort
def HAEFF(g):
	hal = HADIF(g)*HAVOL(g)
	return hal

# HALEV - Halstead level
def HALEV(g):
	mu1 = hal_mu1(g)
	mu2 = hal_mu2(g)
	eta1 = hal_eta1(g)
	eta2 = hal_eta2(g)
	hal = (2.0*mu2)/(mu1*eta2)
	return hal

# HATIM - Halstead time
def HATIM(g):
	return HAEFF(g)/18.0

######################################################################
# Experiments
def shortestPath(a,b,cg):
	return shortestPathAllPars(a,b,cg,getClosure(cg),[])

def shortestPathAllPars(a,b,cg,ccg,visited):
	visited.append(a)
	if a == b:
		#print '∆:',a,'is',b
		return 0
	if b in cg[a]:
		#print '∆:',a,'is next to',b
		return 1
	if b not in ccg[a]:
		#print '∆:',a,'is inaccessible from',b
		return 1000000
	if not cg[a]:
		#print '∆:',a,'is inaccessible from',b
		return 1000000
	#print '∆:',a,'-->?'
	goto = filter(lambda x:x not in visited,cg[a])
	if not goto:
		#print '∆:',a,'is inaccessible from',b
		return 1000000
	return 1 + min(map(lambda x:shortestPathAllPars(x,b,cg,ccg,visited),goto))

def nameLevel(level,roots,g):
	return '/'.join(union([],map(lambda x:nameLevel1Root(level,x,getCallGraph(g)),roots)))+'('+str(len(level))+')'
	
def nameLevel1Root(level,root,cg):
	deltas = map(lambda x:shortestPath(root,x,cg),level)
	#d = min(deltas):
	return '|'.join(map(lambda i:level[i],filter(lambda i:deltas[i]==min(deltas),range(0,len(level)))))

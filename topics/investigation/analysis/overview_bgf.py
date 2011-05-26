#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os, sys
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF3
from functools import reduce

def isalpha(x):
	if len(x)<2 or (len(x)==2 and not x.isalpha()) or x=='...' or x=='===':
		return False
	else:
		return reduce(lambda a,b:a and (b=='.' or b=='_' or b=='-' or b=='=' or b.isalnum()),x,True)

def isnotalpha(x):
	return not isalpha(x)

#
def term(g):
	ts = []
	for p in g.prods:
		for n in p.expr.wrapped.getXml().findall('.//terminal'):
			if n.text not in ts:
				ts.append(n.text)
	return ts
def defd(g):
	nts = []
	for p in g.prods:
		if p.nt not in nts:
			nts.append(p.nt)
	return nts
def setminus(a,b):
	c = a[:]
	for x in b:
		if x in c:
	 		c.remove(x)
	return c
def nrused(g):
	# non-recursively used
	nts = []
	for p in g.prods:
		for n in p.expr.wrapped.getXml().findall('.//nonterminal'):
			if (n.text not in nts) and (n.text != p.nt):
				nts.append(n.text)
	return nts

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print('This tool presents an overview of a BGF file.')
		print('Usage:')
		print('      checkbgf <bgf>')
		sys.exit(1)
	grammar = BGF3.Grammar()
	grammar.parse(sys.argv[1])
	print('<?xml version="1.0"?><xml>')
	wrap = lambda x,y:map(lambda z:'<'+x+'>'+z+'</'+x+'>',y)
	print((''.join(wrap('bottom',setminus(nrused(grammar),defd(grammar)) ))+''.join(wrap('top',setminus(setminus(defd(grammar),nrused(grammar)),grammar.roots) ))).replace('&','&amp;'))
	terms = term(grammar)
	htmlify = lambda s:map(lambda x:x.replace('&','&amp;').replace('>','&gt;').replace('<','&lt;'),s)
	print(''.join(wrap('keyword',htmlify(filter(isalpha,terms))))+''.join(wrap('terminal',htmlify(filter(isnotalpha,terms)))))
	print('</xml>')

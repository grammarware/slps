#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
import math
sys.path.append('../../../shared/python')
import BGF
sys.path.append('../size')
import var
import term

def values(g):
	# returns the number of values used in the grammar
	vals = []
	for v in g.getXml().findall('.//value'):
		if v.text not in vals:
			vals.append(v.text)
	return vals

def labels(g):
	# returns the number of production labels used in the grammar
	labs = []
	for v in g.prods:
		if v.label and v.label not in labs:
			labs.append(v.label)
	return labs

def selectors(g):
	# returns the number of expression selectors used in the grammar
	sels = []
	for v in g.getXml().findall('.//selectable'):
		if v.findtext('selector') not in sels:
			sels.append(v.findtext('selector'))
	return sels

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

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a HAL metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	
	# Selectable, Marked, Plus, Star, Optional, Epsilon, Empty, Any, Choice, Sequence
	mu1 = 10
	mu2 = len(var.var(bgf)) + len(term.term(bgf)) + len(values(bgf)) + len(labels(bgf)) + len(selectors(bgf))
	eta1 = opr(bgf)
	eta2 = opd(bgf)
	hal = (mu1*eta2*(eta1+eta2)*math.log(mu1+mu2,2)) / (2*mu2)
	#print 'µ₁ =',mu1,', µ₂ =',mu2,', η₁ =',eta1,', η₂ =',eta2
	#print 'HAL =',hal
	print '%.2f' % hal
	sys.exit(0)

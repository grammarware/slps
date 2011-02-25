#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import os,sys
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import BGF
import metrics

def refs(n,g):
	nts = []
	for p in g.prods:
		for nt in p.expr.wrapped.getXml().findall('.//nonterminal'):
			if nt.text == n:
				nts.append(p.nt)
	return nts

def chain(n,g):
	ps = g.getProdsOfN(n)
	if len(ps)>1:
		return None
	if ps[0].expr.wrapped.__class__.__name__ == 'Nonterminal':
		return ps[0].expr.wrapped.data
	print '--->', ps[0].expr.wrapped.__class__.__name__
	return None

def isdistrneeded(node):
	if node.__class__.__name__ == 'Production':
		if node.expr.wrapped.__class__.__name__ == 'Choice':
			return isdistrneeded(node.expr.wrapped.data)
		else:
			return isdistrneeded(node.expr.wrapped)
	elif node.__class__.__name__ == 'Expression':
		return isdistrneeded(node.wrapped)
	elif node.__class__.__name__ == 'Selectable':
		return isdistrneeded(node.expr)
	elif node.__class__.__name__ in ('Star','Plus','Optional','Epsilon','Empty','Value','Nonterminal','Any','Terminal'):
		# TODO: optional can be refactored into the choice
		return False
	elif node.__class__.__name__ in ('Sequence','Marked'):
		return isdistrneeded(node.data)
	elif node.__class__.__name__ == 'Choice':
		return True
	elif node.__class__.__name__ == 'list':
		for x in node:
			if isdistrneeded(x):
				return True
		return False
	else:
		# we don't deal with marked
		print 'How to deal with',node.__class__.__name__,'?'
		print node
		return ''

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print 'This generator proposes to surface all inner choices in the grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input> <xbgf-output>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	xbgf = open(sys.argv[2],'w')
	xbgf.write('<?xml version="1.0" encoding="UTF-8"?><xbgf:sequence xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:bgf="http://planet-sl.org/bgf">\n')
	needtodistr = []
	for p in bgf.prods:
		if isdistrneeded(p):
			if p.nt not in needtodistr:
				needtodistr.append(p.nt)
		#if p.expr.wrapped.__class__.__name__ == 'Choice':
		#	for e in p.expr.wrapped.data:
		#		if e.wrapped.getXml().findall('.//choice'):
		#			if p.nt not in needtodistr:
		#				needtodistr.append(p.nt)
		#else:
		#	if p.expr.wrapped.getXml().findall('.//choice'):
		#		if p.nt not in needtodistr:
		#			needtodistr.append(p.nt)
	for n in needtodistr:
		print 'Nonterminal',n,'contains inner choices.'
		xbgf.write('<xbgf:distribute><nonterminal>'+n+'</nonterminal></xbgf:distribute>\n')
	xbgf.write('</xbgf:sequence>')
	xbgf.close()
	sys.exit(0)

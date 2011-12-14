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

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print 'This generator proposes to remove all chain productions and inline all once-used nonterminal symbols.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input> <xbgf-output>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	xbgf = open(sys.argv[2],'w')
	xbgf.write('<?xml version="1.0" encoding="UTF-8"?><xbgf:sequence xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:bgf="http://planet-sl.org/bgf">\n')
	chains = {}
	nts = metrics.var(bgf)
	for n in nts:
		ps = bgf.getProdsOfN(n)
		if len(ps) == 1 and ps[0].expr.wrapped.__class__.__name__ == 'Nonterminal':
			chains[ps[0].expr.wrapped.data] = n
	refers1 = {}
	refers2 = {}
	for p in bgf.prods:
		for n in p.expr.wrapped.getXml().findall('.//nonterminal'):
			if n.text not in refers1.keys():
				refers1[n.text] = [p.nt]
			else:
				refers1[n.text].append(p.nt)
			if p.nt not in refers2.keys():
				refers2[p.nt] = [n.text]
			else:
				refers2[p.nt].append(n.text)
	for n in nts:
		c = refs(n,bgf)
		if len(c)==0:
			if n in bgf.roots:
				print 'Nonterminal',n,'confirmed as root.'
			else:
				print 'Unused non-root found:',n
		elif len(c)==1:
			d = len(bgf.getProdsOfN(n))
			if d==1:
				if n in chains.keys():
					print 'Nonterminal',n,'is advised to be unchained.'
					xbgf.write('<xbgf:unchain><bgf:production><nonterminal>'+chains[n]+'</nonterminal><bgf:expression><nonterminal>'+n+'</nonterminal></bgf:expression></bgf:production></xbgf:unchain>\n')
				elif n in bgf.roots:
					print 'We never inline the root.'
				elif n in refers1.keys() and n in refers2.keys() and refers1[n]==refers2[n]:
					print 'Nonterminal',n,'is co-recursive, cannot be inlined with XBGF.'
				else:
					print 'Nonterminal',n,'is advised to be inlined.'					
					#if n in refers1.keys():
					#	print 'REFERS(1):',refers1[n]
					#if n in refers2.keys():
					#	print 'REFERS(2):',refers2[n]
					xbgf.write('<xbgf:inline>'+n+'</xbgf:inline>\n')
			else:
				pass
				#print 'Nonterminal',n,'can be inlined with',d,'productions.'
		else:
			pass
			#print 'Nonterminal',n,'is used',len(c),'times by',c
	xbgf.write('</xbgf:sequence>')
	xbgf.close()
	sys.exit(0)

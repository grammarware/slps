#!/usr/local/bin/python
import os
import sys
sys.path.append('../../../shared/python')
import slpsns
import elementtree.ElementTree as ET

names   = []

if __name__ == "__main__":
	if len(sys.argv) != 4:
		print 'This tool extracts a portion of a grammar that starts at a given root nonterminal and includes all referenced nonterminals as well.'
		print 'Usage:'
		print '      subgrammar <bgf-input> <new-root> <bgf-output>'
		sys.exit(1)
	slpsns.init(ET)
	bgf = ET.parse(sys.argv[1])
	grammar = {}
	for prod in bgf.findall('//'+slpsns.bgf_('production')):
		nt = prod.findtext('nonterminal')
		if nt in grammar.keys():
			grammar[nt].append(prod)
		else:
			grammar[nt]=[prod]
	newBgf = ET.Element(slpsns.bgf_('grammar'))
	nts = [sys.argv[2]]
	ET.SubElement(newBgf,'root').text = nts[0]
	oldnts = []
	while nts:
		for prod in grammar[nts[0]]:
			newBgf.append(prod)
		for nt in prod.findall('.//nonterminal'):
			if (nt.text not in oldnts) and (nt.text not in nts):
				nts.append(nt.text)
		oldnts.append(nts[0])	# car
		nts = nts[1:]			# cdr
	ET.ElementTree(newBgf).write(sys.argv[3])
	sys.exit(0)

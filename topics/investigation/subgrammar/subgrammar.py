#!/usr/local/bin/python
import os,sys
import elementtree.ElementTree as ET
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import BGF
import metrics

processed = []

def addProductionsOf(r,cg,src,tgt):
	if r in processed:
		return
	for p in src.prods:
		if p.nt == r:
			tgt.addProd(p)
	processed.append(r)
	for n in cg[r]:
		addProductionsOf(n,cg,src,tgt)
	return
	
if __name__ == "__main__":
	if len(sys.argv) < 4:
		print 'This tool extracts a portion of a grammar that starts at a given root nonterminal and includes all referenced nonterminals as well.'
		print 'Usage:'
		print '      subgrammar <bgf-input> <new-root> [<new-root>...] <bgf-output>'
		sys.exit(1)
	bgf = BGF.Grammar()
	newBgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	roots = sys.argv[2:-1]
	print 'Setting root(s) to',roots
	for r in roots:
		newBgf.addRoot(r)
	cg = metrics.getCallGraph(bgf)
	for r in roots:
		addProductionsOf(r,cg,bgf,newBgf)
	ET.ElementTree(newBgf.getXml()).write(sys.argv[-1])
	sys.exit(0)

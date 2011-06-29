#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os, sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import BGF3
from functools import reduce

if __name__ == "__main__":
	if len(sys.argv) < 4:
		print('Usage:')
		print('	merge.py input1.bgf [inputX.bgf ...] output.bgf')
		sys.exit(-1)
	bgfs = []
	names = []
	nts = []
	for fname in sys.argv[1:-1]:
		g = BGF3.Grammar()
		g.parse(fname)
		bgfs.append(g)
		names.append(fname)
	print(len(bgfs),'grammars read.')
	for i in range(0,len(bgfs)):
		n = []
		for p in bgfs[i].prods:
			if p.nt not in n:
				n.append(p.nt)
		nts.append(n)
		print('\t',names[i],'has',len(bgfs[i].prods),'productions,',len(nts[i]),'nonterminals and',len(bgfs[i].roots),'roots')
	# FINAL STEP: compose BGF
	allbgf = BGF3.Grammar()
	# roots are combined
	for g in bgfs:
		for r in g.roots:
			allbgf.addRoot(r)
		# TODO: write a real merge, not just concatenate
		for p in g.prods:
			allbgf.addProd(p)
	ET.ElementTree(allbgf.getXml()).write(sys.argv[-1])
	print('Merged BGF written to',sys.argv[-1])

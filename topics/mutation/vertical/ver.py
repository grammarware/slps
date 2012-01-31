#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os,sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF3
import XBGF3
import slpsns

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print('This tool generates an XBGF script to verticalize all horizontally defined nonterminals.')
		print('Usage:')
		print('      '+sys.argv[0]+' <bgf-input> <xbgf-output>')
		sys.exit(1)
	bgf = BGF3.Grammar()
	bgf.parse(sys.argv[1])
	xbgf = XBGF3.Sequence()
	for p in bgf.prods:
		if p.expr.wrapped.__class__.__name__ == 'Choice':
			print('Verticalize',p.nt)
			s = XBGF3.Step('vertical')
			n = XBGF3.Nonterminal()
			n.setName(p.nt)
			s.addParam(n)
			xbgf.addStep(s)
	ET.ElementTree(xbgf.getXml()).write(sys.argv[2])
	sys.exit(0)

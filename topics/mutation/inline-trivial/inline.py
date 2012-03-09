#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os,sys
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import BGF3
import metrics3

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print('This generator proposes to remove all chain productions and inline all once-used nonterminal symbols.')
		print('Usage:')
		print('      '+sys.argv[0]+' <bgf-input> <xbgf-output>')
		sys.exit(1)
	bgf = BGF3.Grammar()
	bgf.parse(sys.argv[1])
	xbgf = open(sys.argv[2],'w')
	xbgf.write('<?xml version="1.0" encoding="UTF-8"?><xbgf:sequence xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:bgf="http://planet-sl.org/bgf">\n')
	inline = []
	nts = metrics3.var(bgf)
	for n in nts:
		ps = bgf.getProdsOfN(n)
		if len(ps) == 1 and ps[0].expr.wrapped.who() == 'Nonterminal':
			print('Nonterminal',n,'is advised to be inlined.')
			xbgf.write('<xbgf:inline>'+n+'</xbgf:inline>\n')
	xbgf.write('</xbgf:sequence>')
	xbgf.close()
	sys.exit(0)

#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os,sys
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF3

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print('This tool generates an XBGF script to horizontalize all vertical nonterminals present in the grammar.')
		print('Usage:')
		print('      '+sys.argv[0]+' <bgf-input> <xbgf-output')
		sys.exit(1)
	bgf = BGF3.Grammar()
	bgf.parse(sys.argv[1])
	single = []
	multiple = []
	for p in bgf.prods:
		if p.nt not in single and p.nt not in multiple:
			single.append(p.nt)
		elif p.nt in single:
			single.remove(p.nt)
			multiple.append(p.nt)
		elif p.nt in multiple:
			pass
	xbgf = open(sys.argv[2],'w')
	xbgf.write('<?xml version="1.0" encoding="UTF-8"?>\n<xbgf:sequence xmlns:xbgf="http://planet-sl.org/xbgf">\n')
	for n in multiple:
		xbgf.write('<xbgf:horizontal><nonterminal>'+n+'</nonterminal></xbgf:horizontal>\n')
	xbgf.write('</xbgf:sequence>')
	xbgf.close()
	sys.exit(0)

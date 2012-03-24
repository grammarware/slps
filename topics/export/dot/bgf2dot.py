#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import os,sys
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import BGF
import metrics
import graphs

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print 'This tool generates a nonterminal dependency graph (a sort graph).'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input> <dot-output>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	dot = open(sys.argv[2],'w')
	dot.write(graphs.bgf2dot(bgf))
	dot.close()
	sys.exit(0)

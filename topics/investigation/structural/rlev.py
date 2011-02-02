#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF
import levels

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a RLEV metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	cg = levels.getCallGraph(bgf)
	gl = levels.getLevels(bgf)
	#print 'RLEV =',len(filter(lambda x:(len(x)>1)or(x[0] in cg[x[0]]),gl))
	print len(filter(lambda x:(len(x)>1)or(x[0] in cg[x[0]]),gl))
	sys.exit(0)

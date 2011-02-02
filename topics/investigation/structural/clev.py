#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
import levels
sys.path.append('../../../shared/python')
import BGF
sys.path.append('../size')
import var

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a CLEV metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	gl = levels.getLevels(bgf)
	nts = var.var(bgf)
	#print 'CLEV =',len(gl)/(0.0+len(nts))
	print '%.1f' % (100*len(gl)/(0.0+len(nts)))
	sys.exit(0)

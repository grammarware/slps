#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF
import defined
import used

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a TOP metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	tops = []
	usednts = used.used(bgf)
	for nt in defined.defined(bgf):
		if nt not in usednts:
			tops.append(nt)
	#print 'TOP =',len(tops)
	print len(tops)
	sys.exit(0)

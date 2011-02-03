#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import os,sys
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import BGF
import metrics

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates a LEV metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	print metrics.LEV(bgf)
	sys.exit(0)

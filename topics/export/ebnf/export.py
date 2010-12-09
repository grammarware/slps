#!/usr/local/bin/python
import os
import sys
sys.path.append('../../../shared/python')
import slpsns
import BGF
import elementtree.ElementTree as ET

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print 'This tool extracts a Rascal grammar.'
		print 'Usage:'
		print '      bgf2bnf <bgf-input> <bnf-output>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	bnf = open(sys.argv[2],'w')
	bnf.write(str(bgf))
	bnf.close()
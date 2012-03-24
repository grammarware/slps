#!/usr/local/bin/python
import os,sys
import elementtree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF, NLG

if __name__ == "__main__":
	if len(sys.argv) not in (2,3):
		print 'This tool tells a story about a grammar.'
		print 'Usage:'
		print '      tell <bgf-input> [<text-output>]'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	storyteller = NLG.NLGEngine(bgf)
	if len(sys.argv) == 2:
		print storyteller.describeGrammar()
	else:
		story = open(sys.argv[2],'w')
		story.write(storyteller.describeGrammar())
		story.close()
	sys.exit(0)

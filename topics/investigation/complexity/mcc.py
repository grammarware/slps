#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF

def mccabe(node):
	if node.__class__.__name__ == 'Grammar':
		return sum(map(mccabe,node.prods))
	elif node.__class__.__name__ in ('Production','Selectable'):
		return mccabe(node.expr)
	elif node.__class__.__name__ == 'Expression':
		return mccabe(node.wrapped)
	elif node.__class__.__name__ == 'Marked':
		return mccabe(node.data)
	elif node.__class__.__name__ in ('Plus','Star','Optional'):
		return 1+mccabe(node.data)
	elif node.__class__.__name__ in ('Terminal','Nonterminal','Epsilon','Any','Empty','Value'):
		return 0
	elif node.__class__.__name__ == 'Choice':
		return len(node.data)-1 + max(map(mccabe,node.data))
	elif node.__class__.__name__ == 'Sequence':
		return sum(map(mccabe,node.data))
	else:
		print 'How to deal with',node.__class__.__name__,'?'
		return 0

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates an MCC metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	#print 'MCC =',mccabe(bgf)
	print mccabe(bgf)
	sys.exit(0)

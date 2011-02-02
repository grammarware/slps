#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import sys
sys.path.append('../../../shared/python')
import BGF
sys.path.append('../size')
import var

def rhssize(node):
	if node.__class__.__name__ == 'Grammar':
		return sum(map(rhssize,node.prods))/(0.0+len(var.var(node)))
	elif node.__class__.__name__ in ('Production','Selectable'):
		return rhssize(node.expr)
	elif node.__class__.__name__ == 'Expression':
		return rhssize(node.wrapped)
	elif node.__class__.__name__ == 'Marked':
		return rhssize(node.data)
	elif node.__class__.__name__ in ('Plus','Star','Optional'):
		return rhssize(node.data)
	elif node.__class__.__name__ in ('Terminal','Nonterminal','Value'):
		return 1
	elif node.__class__.__name__ in ('Epsilon','Any','Empty'):
		return 0
	elif node.__class__.__name__ in ('Choice','Sequence'):
		return sum(map(rhssize,node.data))
	else:
		print 'How to deal with',node.__class__.__name__,'?'
		return 0

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool calculates an AVS metric for any given BGF grammar.'
		print 'Usage:'
		print '      '+sys.argv[0]+' <bgf-input>'
		sys.exit(1)
	bgf = BGF.Grammar()
	bgf.parse(sys.argv[1])
	#print 'AVS =',rhssize(bgf)
	print '%.1f' % rhssize(bgf)
	sys.exit(0)

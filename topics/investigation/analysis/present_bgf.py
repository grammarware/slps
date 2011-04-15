#!/usr/bin/python
import os, sys
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF, bench

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool presents an overview of a BGF file.'
		print 'Usage:'
		print '      checkbgf <bgf>'
		sys.exit(1)
	grammar = BGF.Grammar()
	grammar.parse(sys.argv[1])
	mb = bench.MeasurementBench(grammar)
	print 'Productions:         ',len(grammar.prods)
	print 'Top alternatives:    ',mb.PROD()
	print 'Nonterminals defined:',mb.DEFD()
	pa = lambda a:reduce(lambda x,y:x+', '+str(y),a[1:],'('+str(a[0]))+')'
	if mb.UNDEF():
		print '%20s:' % 'bottom',     mb.UNDEF(), pa(mb.getBottoms())
	else:
		print '%20s:' % 'bottom',     mb.UNDEF()
	if mb.ROOT():
		print '%20s:' % 'root',       mb.ROOT(), pa(grammar.roots)
	else:
		print '%20s:' % 'root',       mb.ROOT()
	if mb.DEAD():
		print '%20s:' % 'dead top',   mb.DEAD(), pa(mb.getDeadTops())
	else:
		print '%20s:' % 'dead top',   mb.DEAD()
	print 'Terminals used:      ',mb.TERM()

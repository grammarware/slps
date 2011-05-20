#!/usr/local/bin/python
import os, sys
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF, bench, metrics

def isalpha(x):
	if len(x)<2 or (len(x)==2 and not x.isalpha()) or x=='...' or x=='===':
		return False
	else:
		return reduce(lambda a,b:a and (b=='.' or b=='_' or b=='-' or b=='=' or b.isalnum()),x,True)

def isnotalpha(x):
	return not isalpha(x)

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print 'This tool presents an overview of a BGF file.'
		print 'Usage:'
		print '      checkbgf <bgf>'
		sys.exit(1)
	grammar = BGF.Grammar()
	grammar.parse(sys.argv[1])
	mb = bench.MeasurementBench(grammar)
	print '<?xml version="1.0"?><xml>'
	wrap = lambda x,y:map(lambda z:'<'+x+'>'+z+'</'+x+'>',y)
	print ''.join(wrap('bottom',mb.getBottoms()))+''.join(wrap('top',mb.getDeadTops()))
	terms = metrics.term(grammar)
	htmlify = lambda s:map(lambda x:x.replace('&','&amp;').replace('>','&gt;').replace('<','&lt;'),s)
	print ''.join(wrap('keyword',htmlify(filter(isalpha,terms))))+''.join(wrap('terminal',htmlify(filter(isnotalpha,terms))))
	print '</xml>'

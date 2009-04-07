#!/usr/bin/python
import sys
import re

p0 = re.compile('options [{][^}]*[}]')
p5 = re.compile('\'}\'')
p4 = re.compile('tokens [{][^}]*[}]')
p1 = re.compile('[{][^}]*[}]')
p2 = re.compile('[@][a-z]*')
p3 = re.compile('returns [\[][^\]]*[\]]')
p6 = re.compile('/[*]([^*]|[*][^/])*[*]/')
p4 = re.compile('tokens [{][^}]*[}]')

def main():
	if (len(sys.argv)!=3):
		print "Usage: antlrstrip.py Grammar.g Grammar_pure.g"
		return
	inf = open(sys.argv[1],"r")
	out = open(sys.argv[2],'w')
	# reading the grammar
	lines = ' '.join(inf.readlines())
	# regexp-powered cleaning
	lines = p6.sub('',lines)	# kill comments
	lines = p5.sub('',lines)	# kill '}'s
	lines = p1.sub('',lines)	# kill semantic actions
	lines = p2.sub('',lines)	# kill modifiers
	lines = p3.sub('',lines)	# kill returns
	lines = p0.sub('',lines)	# kill options
	lines = p4.sub('',lines)	# kill tokens
	# whitespace shrinking
	lines = ' '.join(lines.split())
	# removing terminal symbols, pretty-printing the rest
	lines = map(lambda x:(not x.split(':')[0].strip().isupper())*x,lines.split(';'))
	while (not lines[-2].strip()): lines.pop()
	# writing the pretty-printed result
	out.writelines(';\n'.join(lines))
	inf.close()
	out.close()

if __name__ == "__main__":
    main()


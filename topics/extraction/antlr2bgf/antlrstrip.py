#!/usr/bin/python
import sys
import re

p1 = re.compile('[{][^}]*[}]')
p2 = re.compile('[@][a-z]*')
p3 = re.compile('returns [\[][^\]]*[\]]')

def main():
	if (len(sys.argv)!=3):
		print "Usage: antlrstrap.py Grammar.g Grammar_pure.g"
		return
	inf = open(sys.argv[1],"r")
	out = open(sys.argv[2],'w')
	# reading the grammar
	lines = ' '.join(inf.readlines())
	# regexp-powered cleaning
	lines = p1.sub('',lines)
	lines = p2.sub('',lines)
	lines = p3.sub('',lines)
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


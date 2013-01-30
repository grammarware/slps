#!/usr/local/bin/python
# -*- coding: utf-8 -*-

import os

tpl  = '| %-25s | %10s | %15s |'
sep = '|'+'-'*27+'|'+'-'*11+':|'+'-'*16+':|'

languages = {'py': 'Python', 'c': 'C', 'h': 'C', 'cpp': 'C++', 'java': 'Java', 'rsc': 'Rascal', 'xsd': 'XML Schema', 'bgf': 'BGF', 'baseline': 'BGF', 'xbgf': 'XBGF', 'cbgf': 'ΞBGF', 'exbgf': 'EXBGF', 'lcf': 'LCF', 'cs': 'C#', 'html': 'HTML', 'pro': 'Prolog', 'g': 'ANTLR', 'sdf': 'SDF', 'asf': 'ASF', 'dot': 'Graphviz', 'ecore': 'Ecore', 'trafo': 'FST', 'ldf': 'LDF', 'xldf': 'XLDF', 'xsl': 'XSLT', 'xslt': 'XSLT', 'lll': 'LLL', 'xq': 'XQuery', 'xquery': 'XQuery', 'hs': 'Haskell', 'txl': 'TXL', 'Txl': 'TXL', 'grm': 'TXL', 'pl': 'Prolog', 'graphml': 'GraphML', 'megal': 'MegaL', 'md': 'Markdown', 'red': 'Red', 'reds': 'Red/System', 'rebol': 'REBOL', 'cob': 'COBOL', 'xmi': 'Ecore', 'genmodel': 'Ecore', 'btf': 'BTF', 'Makefile': 'make', 'sh': 'Shell', 'edd': 'EDD'}

results = {}

for lang in languages.keys():
	if languages[lang] not in results.keys():
		results[languages[lang]] = [0,0]
	if lang in ('Makefile'):
		f = os.popen('./calc2 %s' % lang)
	else:
		f = os.popen('./calc %s' % lang)
	for line in map(lambda x:x.split(),f.readlines()):
		if line[-1] == 'files.lst':
			results[languages[lang]][0] += int(line[0])
		if line[-1] == 'files.src':
			results[languages[lang]][1] += int(line[0])
	f.close()

for d in ('tools', 'generators', 'wrappers', 'internals'):
	f = os.popen('ls -1 ~/projects/slps/shared/%s/ | wc -l' % d)
	results['Shell'][0] += int(f.readlines()[0].split()[0])
	f.close()
	f = os.popen('wc -l ~/projects/slps/shared/%s/*' % d)
	results['Shell'][1] += int(filter(lambda x:x.find('total')>-1,f.readlines())[0].split()[0])
	f.close()

table = []
straight = []

print tpl.replace('i','s')*3 % tuple(map(lambda x:'**%s**' % x,('Software language', 'Files', 'LOC'))*3)
print (sep+'---')*3+sep
for x in sorted(results.keys()):
	if results[x][0]>0:
		# print tpl % ('[[%s]]'%x, results[x][0], results[x][1])
		straight.append(('[[%s]]'%x, results[x][0], results[x][1]))

L = len(straight)/3+1

for no in range(0,L):
	if 2*L+no < len(straight):
		last = straight[2*L+no]
	else:
		last = ('','','')
	table.append((straight[no][0],straight[no][1],straight[no][2], straight[L+no][0],straight[L+no][1],straight[L+no][2], last[0],last[1],last[2]))

for line in table:
	print tpl*3 % line

#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys

if len(sys.argv)!=2:
	print 'Gimme one file name, or else.'
	sys.exit(-1)
zoo = sys.argv[1]
# READ
f = open(zoo,'r')
b = map(lambda x:x.strip(), f.readlines())
f.close()
# COMPRESS
last = ''
cx = 0
newb = []
i = 0
while b[i] != '\\hline':
	newb.append(b[i])
	i += 1
while b[i+1][0] != '\\':
	newb.append(b[i])
	i += 1
	col = {}
	while b[i] != '\\hline':
		if b[i] not in col.keys():
			col[b[i]] = 1
		else:
			col[b[i]] += 1
		i += 1
	for k in col.keys():
		sep = k.split('\t&\t')
		sep.insert(1,str(col[k]))
		newb.append('\t&\t'.join(sep))
while i < len(b):
	newb.append(b[i])
	i += 1
b = newb
# WRITE
f = open(zoo,'w')
for line in b:
	f.write(line.replace('#','\#').replace('µ','$\\mu$')+'\n')
f.close()
# END

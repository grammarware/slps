#!/usr/bin/env python

# READ
f = open('zoo.tex','r')
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
f = open('zoo.tex','w')
for line in b:
	# f.write('----%i go!' % i)
	f.write(line+'\n')
f.close()
# END

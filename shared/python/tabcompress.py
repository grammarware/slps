#!/usr/bin/env python

# READ
f = open('zoo.tex','r')
b = map(lambda x:x.strip(), f.readlines())
f.close()
# COMPRESS
last = ''
cx = 0
newb = []
for line in b:
	if line[0] == '\\':
		line = line.replace('{|l|l|l|l|l|}','{|l|c|l|l|l|l|}')
	if len(line.split('\t&\t')) != 5 or line[0]=='\\':
		if last != '':
			sep = last.split('\t&\t')
			sep.insert(1,str(cx))
			newb.append('\t&\t'.join(sep))
		last = ''
		cx = 0
		newb.append(line)
		continue
	if line == last:
		cx += 1
		continue
	else:
		if last != '':
			sep = last.split('\t&\t')
			sep.insert(1,str(cx))
			newb.append('\t&\t'.join(sep))
		last = line
		cx = 1
b = newb
# WRITE
f = open('zoo.tex','w')
for line in b:
	# f.write('----%i go!' % i)
	f.write(line+'\n')
f.close()
# END

#!/usr/local/bin/python
# -*- coding: utf-8 -*-

import os

fmt1 = '## Relevant files\n'
fmt2 = '## Contributors\n'

def replaceSection(lines,hdr,newlines):
	# 0 before; 1 in; 2 after
	rlines = []
	state = 0
	if hdr not in lines:
		return lines + ['\n',hdr] + newlines + ['\n']
	for line in lines:
		if state==0:
			if line == hdr:
				# ignore next lines
				state = 1
				# insert newlines
				rlines.append(line)
				rlines.extend(newlines)
			else:
				# use the line
				rlines.append(line)
		elif state == 1:
			if line.strip()=='' or line.startswith('##'):
				# stop skipping
				state = 2
				# use the rest
				rlines.append(line)
		elif state == 2:
			rlines.append(line)
	return rlines

def rname(x):
	if x == 'grammarware':
		return 'Vadim Zaytsev'
	elif x == 'rlaemmel':
		return 'Ralf Lämmel'
	else:
		print 'Should I know who',x,'is?'
		sys.exit()

maps = {}
worked = {}

for root, dirs, filenames in os.walk('texts'):
	for f in filenames:
		rd = open(os.path.join(root,f),'r')
		lines = rd.readlines()
		if fmt1 in lines:
			for line in lines[lines.index(fmt1)+1:]:
				if not line.strip() or line.startswith('##'):
					break
				what = line.split('`')[1]
				if f in maps.keys():
					maps[f].append(what)
				else:
					maps[f] = [what]
		if fmt2 in lines:
			for line in lines[lines.index(fmt2)+1:]:
				if not line.strip() or line.startswith('##'):
					break
				who = line.split('@')[1].split(')')[0]
				# print who,'worked on',f
				if f in worked.keys():
					worked[f].append(who)
				else:
					worked[f] = [who]
		rd.close()

for aff in maps.keys():
	if aff not in worked.keys():
		worked[aff] = []
	for rf in maps[aff]:
		print 'Checking up',rf
		# f = os.popen('git log --follow --pretty=format:%%an -- ~/projects/slps/%s | sort | uniq' % rf)
		f = os.popen('git log --follow --pretty=format:%%an:%%s  -- ~/projects/slps/%s | grep -v tag | sed \'s/:.*//\' | sort | uniq' % rf)
		for line in f.readlines():
			if line.strip() not in worked[aff]:
				worked[aff].append(line.strip())
		f.close()
	links = []
	after = []
	f = open('texts/%s' % aff,'r')
	lines = f.readlines()
	rlines = replaceSection(lines,fmt1,['* [`%s`](../blob/master/%s)\n' % (link,link) for link in maps[aff]])
	rlines = replaceSection(rlines,fmt2,sorted(['* [%s (@%s)](https://github.com/%s)\n' % (rname(pers),pers,pers) for pers in worked[aff]]))
	f.close()
	# print 'Yes',aff
	if lines != rlines:
		f = open('texts/%s' % aff,'w')
		for line in rlines:
			f.write(line)
		f.close()

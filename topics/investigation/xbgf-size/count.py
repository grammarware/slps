#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
import os
import sys
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import slpsns
import xml.etree.ElementTree as ET

if __name__ == '__main__':
	if len(sys.argv)<3:
		print('Usage:\n\tcount.py <input-def> <input-file>...')
		sys.exit(1)
	# xbgf = ET
	names = []
	allknown = []
	curname = ''
	fd = {}
	format = open(sys.argv[1],'r')
	for line in format.readlines():
		line = line.strip()
		if not line:
			continue
		if line[0] == '#':
			curname = line[1:].strip()
			if curname not in fd.keys():
				fd[curname] = []
			if curname not in names:
				names.append(curname)
		elif line[0] == '?':
			lhs,rhs = line[1:].strip().split('=')
			# lhs = lhs.split('+')
			# rhs = rhs.split('+')
			lefts = []
			rights = []
			for e in lhs.split('+'):
				lefts.extend(fd[e.strip()])
			for e in rhs.split('+'):
				rights.extend(fd[e.strip()])
			broken = False
			for e in lefts:
				if e not in rights:
					print(e,'present in',lhs,'but not in',rhs)
					broken = True
			for e in rights:
				if e not in lefts:
					print(e,'present in',rhs,'but not in',lhs)
					broken = True
			if broken:
				print('Assertion does not hold!')
				sys.exit(1)
		else:
			if line not in fd[curname]:
				fd[curname].append(line)
			if line not in allknown:
				allknown.append(line)
	format.close()
	# print(fd)
	cx = {}
	for fn in sys.argv[2:]:
		f = ET.parse(fn)
		tovisit = f.findall('*')
		for e in tovisit:
			name = e.tag
			if name[0] == '{':
				name = name.split('}')[0].split('/')[-1]+':'+name.split('}')[-1]
			# print(name)
			if name in cx.keys():
				cx[name] += 1
			else:
				cx[name] = 1
			if name in fd['Walk in']:
				tovisit.extend(e.findall('*'))
	# f.close()
	for cat in names:
		if cat == 'Disregard':
			continue
		catcx = 0
		for nincat in fd[cat]:
			if nincat in cx.keys():
				catcx += cx[nincat]
		if catcx == 0:
			continue
		elif catcx < 10:
			details = []
			for nincat in fd[cat]:
				if nincat in cx.keys():
					details.append(nincat)
			print(cat,':',catcx,'(',', '.join(details),')')
		else:
			print(cat,':',catcx)
	for fnd in cx.keys():
		if fnd not in allknown:
			print('Also found',fnd)
	sys.exit(0)

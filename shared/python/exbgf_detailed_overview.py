#!/usr/bin/python
import os
import sys
import slpsns
import slpsXPath
import elementtree.ElementTree as ET

names   = []
targets = {}
results = {}

def report(keys,key,note):
	s = note.replace('exbgf:','')
	cx = 0
	for x in keys:
		cx += results[key][x]
		if results[key][x]:
			s += '&'+`results[key][x]`
		else:
			s += '& ---'
	if cx:
		print s+'&'+`cx`+'\\\\'

if __name__ == "__main__":
	if len(sys.argv) != 4:
		print 'This tool generates an overview of a bunch of EXBGF scripts.'
		print 'Usage:'
		print '      exbgfover <exbgf.xsd> <lcf> <exbgfs-path>'
		sys.exit(1)
	xsd = ET.parse(sys.argv[1])
	gn = 0
	for x in xsd.findall('/'+slpsns.xsd_('element')+'/'+slpsns.xsd_('complexType')+'/'+slpsns.xsd_('choice')):
		names.append([])
		for y in x.findall(slpsns.xsd_('element')):
			if y.attrib.has_key('ref'):
				names[gn].append(y.attrib['ref'])
				# print 'added',y.attrib['ref']
		gn += 1
	# names.remove([])
	lcf = ET.parse(sys.argv[2])
	for x in lcf.findall('/target'):
		name = x.findtext('name')
		targets[name] = []
		for y in x.findall('branch/*/perform'):
			targets[name].append(y.text)
	path = sys.argv[3]
	if path[-1] != '/':
		path += '/'
	results['LOC'] = {}
	results['NOI'] = {}
	results['NOX'] = {}
	for x in names:
		for y in x:
			results[y] = {}
			for z in targets.keys():
				results[y][z] = 0
	for x in targets.keys():
		results['LOC'][x] = 0
		results['NOI'][x] = 0
		results['NOX'][x] = 0
		for y in targets[x]:
			results['LOC'][x] += slpsXPath.loc(path+y+'.exbgf')
			results['NOI'][x] += slpsXPath.noi(path+y+'.exbgf')
			xbgf = ET.parse(path+y+'.exbgf')
			results['NOX'][x] += len(xbgf.findall('/*'))
			for z in names:
				for q in z:
					# print q
					qr = q.replace('exbgf:',slpsns.exbgf_(''))
					# qr = '*'
					results[q][x] += len(xbgf.findall(qr))
					# for w in xbgf.findall(qr)
					# print slpsns.exbgf_('*')+'/'+qr
					results[q][x] += len(xbgf.findall('*/'+qr))
					# print slpsns.exbgf_('*')+'/'+slpsns.exbgf_('*')+'/'+qr
					results[q][x] += len(xbgf.findall('*/*/'+qr))
					# results[q][x] += len(xbgf.findall(q.replace('xbgf:',slpsns.xbgf_(''))))
	# print names
	for x in names:
		for y in x:
			used = False
			for z in targets.keys():
				if results[y][z]:
					used = True
			if not used:
				print '%%',y,'not used in any EXBGF script'
	sorted = targets.keys()[:]
	sorted.sort()
	print '\\begin{tabular}{l|'+('c|'*len(targets))+'|c}'
	for x in sorted:
		print '&\\textbf{'+x+'}',
	print '&\\textbf{Total}\\\\\\hline'
	for x in names:
		for y in x:
			report(sorted,y,'\\xbgfNumber{'+y+'}')
		print '\\hline'
	print '\\end{tabular}'
	sys.exit(0)


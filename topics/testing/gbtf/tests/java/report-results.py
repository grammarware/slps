#!/usr/bin/python
# -*- coding: utf-8 -*-
import os, sys, math, glob

def readDashSep(s):
	# aaa\n----------\nbbb...
	f = open(s,'r')
	res = ['']
	met = ['']
	nam = ['']
	i = 0
	for line in f.readlines():
		if line.strip()[-10:] == '----------':
			i += 1
			res.append('')
			met.append(line.split('-')[2])
			nam.append(line.split('-')[3].split('.')[0])
		else:
			res[i] += line.strip()
	f.close()
	return res[1:],met[1:],nam[1:]

if __name__ == "__main__":
	if len(sys.argv) > 1:
		print 'No parameters expected!'
		sys.exit(-1)
	report = {}
	repkeys = []
	for f in glob.glob( 'results/*.res'):
		out,met,nam = readDashSep(f)
		total = len(out)
		passed = total
		bymethod = {}
		for tc in range(0,total):
			if met[tc] not in bymethod.keys():
				bymethod[met[tc]]=[0,0,[]]
			if out[tc] == 'pass' or out[tc] == '':
				bymethod[met[tc]][0] += 1
			else:
				bymethod[met[tc]][2].append(nam[tc])
			bymethod[met[tc]][1] += 1
		repkeys.append(f.replace('.res','').replace('results/',''))
		report[repkeys[-1]] = bymethod
	for k in repkeys:
		passed = total = 0
		repkk = []
		for method in ('sc','rc','nc','bc','cdbc'):
			if method in report[k].keys():
				repkk.append(method)
		for m in repkk:
			passed += report[k][m][0]
			total  += report[k][m][1]
			print '%-20s'%(k+' '+m.upper()),':','%6.2f'%(100.0*report[k][m][0]/report[k][m][1]),'% [',report[k][m][0],'/',report[k][m][1],']'
			if report[k][m][1]-report[k][m][0]<5 and report[k][m][1]!=report[k][m][0]:
				print ' => fails on',report[k][m][2]
		print '%-20s'%(k+' total'),':','%6.2f'%(100.0*passed/total),'% [',passed,'/',total,']'
	sys.exit()

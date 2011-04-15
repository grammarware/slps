#!/usr/bin/python
# -*- coding: utf-8 -*-
import os, sys, math, glob

def readDashSep(s):
	# aaa\n----------\nbbb...
	f = open(s,'r')
	res = [['']]
	i = 0
	for line in f.readlines():
		if line.strip()[-10:] == '----------':
			i += 1
			res.append(['',line.split('-')[0].split('.')[0][2:],line.split('-')[1],line.split('-')[2],line.split('-')[3].split('.')[0]])
			#              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  not necessary
		else:
			res[i][0] += line.strip()
	f.close()
	for i in range(1,len(res)):
		if res[i][0] in ('','pass'):
			res[i][0] = True
		else:
			res[i][0] = False
	return res[1:]

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print 'Please provide two grammar names and a makefile to generate, e.g.:'
		print '\t./report-matches.py 00001 00010'
		sys.exit(1)
	one = sys.argv[1]
	two = sys.argv[2]
	report = {}
	repkeys = []
	pairs = []
	allcols = []
	allrows = []
	for f in glob.glob(one[-1]+'2'+two[-1]+'.res'):
		out = readDashSep(f)
		#vs = f.split('-')[4].split('.')[0]+'::'+f.split('-')[2]
		vs = two+'::javaSource'
		fails = filter(lambda x:not x[0],out)
		for p in fails:
			# [False, '00001', 'WS', 'sc', '67']
			# ../00001.bgf-WS-sc-67.src
			print '[',p[1]+'::'+p[2],'vs',vs,'] [',p[3]+'-'+p[4],']'+'----------'
			os.system('cat ../'+p[1]+'.bgf-'+p[2]+'-'+p[3]+'-'+p[4]+'.src')
	sys.exit(0)

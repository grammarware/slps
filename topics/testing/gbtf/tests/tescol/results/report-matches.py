#!/usr/bin/python
# -*- coding: utf-8 -*-
import os, sys, math, glob

total = {}
ccxx = {}

def reporttotals(n):
	print 'GREPABILITY1, ',n,
	for x in ('void','nearly void','perfect','nearly perfect','nearly exclusive','probable','inclusion','embracing','block','probable block','max','none'):
		if x in total.keys():
			print ',',total[x],
		else:
			print ', 0',
	print
	print 'GREPABILITY2, ',n,
	for x in ('universal','void','nearly void','perfect','nearly perfect','nearly exclusive','probable','inclusion','embracing','block','max','probable block'):
		if x in ccxx.keys():
			print ',',ccxx[x],
		else:
			print ', 0',
	print

def inc(t,n):
	if n == 0:
		return
	if t != 'universal':
		if t not in total.keys():
			total[t] = 0
		total[t] += n
	if t != 'none':
		if t not in ccxx.keys():
			ccxx[t] = 0
		ccxx[t] += 1
	return

def setminus(a,b):
	c = []
	for x in a:
		if x not in b:
			c.append(x)
	return c

def setinter(a,b):
	c = []
	for x in a:
		if x in b:
			c.append(x)
	return c

def seteq(a,b):
	for x in a:
		if x not in b:
			return False
	for x in b:
		if x not in a:
			return False
	return True

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

def unique(a):
	b = []
	for x in a:
		if x not in b:
			b.append(x)
	return b

if __name__ == "__main__":
	if len(sys.argv) != 4:
		print 'Please provide two grammar names and a CSV file name for report, e.g.:'
		print '\t./report-matches.py 00001 00010 sorted.csv'
		sys.exit(1)
	one = sys.argv[1]
	two = sys.argv[2]
	report = {}
	repkeys = []
	pairs = []
	allcols = []
	allrows = []
	for f in glob.glob(one+'*'+two+'.bgf.res'):
		out = readDashSep(f)
		vs = f.split('-')[4].split('.')[0]+'::'+f.split('-')[2]
		matcheswith = unique(map(lambda x:x[1]+'::'+x[2],filter(lambda x:x[0],out)))
		allrows = unique(map(lambda x:x[2],out))
		m = vs.split('::')[1]
		if m not in allcols:
			allcols.append(m)
		for m in matcheswith:
			ours = filter(lambda x:[x[1],x[2]]==m.split('::'),out)
			totals  = len(ours)
			ok = filter(lambda x:x[0],ours)
			passed = len(ok)
			#print '%-22s'%m,'vs','%-22s'%vs,':',passed,'/',total,map(lambda x:x[3],ok)
			pairs.append([m,vs,passed,totals])
	# now we have pairs.
	#allrows = unique(map(lambda x:x[0].split('::')[1],pairs))
	#allcols = unique(map(lambda x:x[1].split('::')[1],pairs))
	matches = {}
	amatches = {}
	cx = 0
	for p in pairs:
		n1 = p[0].split('::')[1]
		n2 = p[1].split('::')[1]
		if n1 not in matches.keys():
			matches[n1] = {}
		if n2 not in amatches.keys():
			amatches[n2] = {}
		matches [n1][n2] = (0.0+p[2])/p[3]
		amatches[n2][n1] = (0.0+p[2])/p[3]
	# resorting the table
	rows = []
	cols = []
	# universal
	for n in amatches.keys():
		# the second criterion is imperfect
		if len(amatches[n])>0.75*len(matches.keys()) and sum(amatches[n].values())>0.75*len(amatches[n]):
			print 'UNIVERSAL match with',n
			cx += 1
			inc('universal',1)
			cols.append(n)
			for k in amatches[n].keys():
				matches[k].pop(n)
			amatches.pop(n)
			if n in matches.keys():
				matches.pop(n)
	# main loop
	change = True
	while change:
		change = False
		# void
		for n in matches.keys():
			if len(matches[n])==0:
				print 'VOID match for',n
				cx += 1
				inc('void',1)
				rows.append(n)
				change = True
				matches.pop(n)
		if change: continue
		# nearly void
		for n in matches.keys():
			if max(matches[n].values()) < 0.25:
				print 'NEARLY VOID match of',n
				cx += 1
				#print n,'-->',matches[n]
				inc('nearly void',1)
				rows.append(n)
				change = True
				matches.pop(n)
		if change: continue
		# perfect
		for n in matches.keys():
			# only one red match
			if len(matches[n])==1 and matches[n].values()==[1.0]:
				m = matches[n].keys()[0]
				if len(amatches[m])==1:
					print 'PERFECT match of',n,'with',m
					cx += 1
					inc('perfect',1)
					rows.append(n)
					cols.append(m)
					change = True
					matches.pop(n)
					amatches.pop(m)
					for k in matches.keys():
						if m in matches[k].keys():
							matches[k].pop(m)
					for k in amatches.keys():
						if n in amatches[k].keys():
							amatches[k].pop(n)
		if change: continue
		# nearly perfect
		for n in matches.keys():
			# only one green match
			if len(filter(lambda x:x>0.75,matches[n].values()))==1:
				for k in matches[n].keys():
					if matches[n][k]>0.75:
						m = k
				back = []
				for k in amatches[m].keys():
					if amatches[m][k]>0.5:
						back.append(k)
				if len(back)!=1:
					# when more match back, it's not NEARLY PERFECT, it's something like BLOCK
					continue
				#if len(filter(lambda x:x>0.75,amatches[m].values()))==1 and amatches[m][n]>0.75:
				if amatches[m][n]>0.75:
					print 'NEARLY PERFECT match of',n,'with',m
					cx += 1
					inc('nearly perfect',1)
					rows.append(n)
					cols.append(m)
					change = True
					matches.pop(n)
					amatches.pop(m)
					for k in matches.keys():
						if m in matches[k].keys():
							matches[k].pop(m)
					for k in amatches.keys():
						if n in amatches[k].keys():
							amatches[k].pop(n)
		if change: continue
		for n in matches.keys():
				# only one green or red match back
				for m in matches[n].keys():
					if matches[n][m] > 0.75:
						if len(filter(lambda x:x>matches[n][m],matches[n].values()))>0:
							# special case when there is a better match than m
							continue
						if m not in amatches.keys():
							# whut?!
							continue
						back = amatches[m].values()
						back.remove(matches[n][m])
						if len(back) == 0 or max(back) < 0.5:
							print 'NEARLY EXCLUSIVE match of',n,'with',m
							cx += 1
							inc('nearly exclusive',1)
							rows.append(n)
							cols.append(m)
							change = True
							matches.pop(n)
							amatches.pop(m)
							for k in matches.keys():
								if m in matches[k].keys():
									matches[k].pop(m)
							for k in amatches.keys():
								if n in amatches[k].keys():
									amatches[k].pop(n)
							break
		if change: continue
		# one to many --- quite hard and nothing to test with
		#for n in matches.keys():
		#	if matches[n].values() == [1.0]*len(matches[n]):
		#		print 'ONE2MANY? match of',n,'with',matches[n].keys()
		#		print 'ONE2MANY:',n,'matches with',matches[n]
		#		for k in matches[n].keys():
		#			print 'ONE2MANY:',k,'amatches with',amatches[k]
		#		#for m in matches[n].keys():
		#		#	amatches[m].pop(n)
		#		#matches.pop(n)
		#if change: continue
		#
		# probable
		for n in matches.keys():
			# only one blue match
			if len(filter(lambda x:x>0.25,matches[n].values()))==1:
				for k in matches[n].keys():
					if matches[n][k]>0.25:
						m = k
						break
				back = []
				for k in amatches[m].keys():
					if amatches[m][k]>0.5:
						back.append(k)
				if len(back)!=1:
					# when more match back, it's not PROBABLE, it's something like BLOCK
					continue
				#if len(filter(lambda x:x>0.75,amatches[m].values()))==1 and amatches[m][n]>0.75:
				if amatches[m][n]>0.25:
					print 'PROBABLE match of',n,'with',m
					cx += 1
					inc('probable',1)
					rows.append(n)
					cols.append(m)
					change = True
					matches.pop(n)
					amatches.pop(m)
					for k in matches.keys():
						if m in matches[k].keys():
							matches[k].pop(m)
					for k in amatches.keys():
						if n in amatches[k].keys():
							amatches[k].pop(n)
		if change: continue
		# inclusion - TEST?
		for n in matches.keys():
			if matches[n].keys() and matches[n].values() == [1.0]*len(matches[n]):
				yes = True
				for m in matches[n].keys():
					if len(amatches[m])>1:
						yes = False
				if not yes:
					continue
				print 'INCLUSION match of',n,'with',matches[n].keys()
				cx += 1
				inc('inclusion',1)
				rows.append(n)
				for m in matches[n].keys():
					cols.append(m)
					amatches.pop(m)
				change = True
				matches.pop(n)
		if change: continue
		# embrace
		for m in amatches.keys():
			if amatches[m].keys() and amatches[m].values() == [1.0]*len(amatches[m]):
				yes = True
				for n in amatches[m].keys():
					if len(matches[n])>1:
						yes = False
				if not yes:
					continue
				print 'EMBRACING match of',amatches[m].keys(),'with',m
				cx += 1
				inc('embracing',len(amatches[m].keys()))
				for n in amatches[m].keys():
					rows.append(n)
					matches.pop(n)
				cols.append(m)
				amatches.pop(m)
				change = True
		if change: continue
		# block
		for n in matches.keys():
			if min(matches[n].values())>0.75:
				lblock = [n]
				rblock = matches[n].keys()
				for m in matches.keys():
					#print 'L:',lblock
					#print 'R:',rblock
					if m in lblock:
						continue
					valz = filter(lambda x:x[1]>0.75,matches[m].items())
					keyz = map(lambda x:x[0],valz)
					valz = map(lambda x:x[1],valz)
					if len(valz)>0:
						if seteq(rblock,keyz):
							lblock.append(m)
						elif len(setminus(rblock,keyz)) in (1,2):
							lblock.append(m)
							rblock=setinter(keyz,rblock)
				if len(lblock)<2 or len(rblock)<2:
					continue
				print 'BLOCK match of\n\t',lblock,'\nwith\n\t',rblock
				cx += 1
				inc('block',len(lblock))
				for n1 in lblock:
					rows.append(n1)
				for n2 in rblock:
					cols.append(n2)
				change = True
				for x in lblock:
					if x in matches.keys():
						matches.pop(x)
					for y in amatches.keys():
						if x in amatches[y]:
							amatches[y].pop(x)
				for x in rblock:
					if x in amatches.keys():
						amatches.pop(x)
					for y in matches.keys():
						if x in matches[y]:
							matches[y].pop(x)
				break
		if change: continue
		# probable block
		for n in matches.keys():
			if matches[n].values() and min(matches[n].values())>0.25:
				lblock = [n]
				rblock = matches[n].keys()
				for m in matches.keys():
					#print 'L:',lblock
					#print 'R:',rblock
					if m in lblock:
						continue
					valz = filter(lambda x:x[1]>0.25,matches[m].items())
					keyz = map(lambda x:x[0],valz)
					valz = map(lambda x:x[1],valz)
					if len(valz)>0:
						if seteq(rblock,keyz):
							lblock.append(m)
						elif len(setminus(rblock,keyz)) in (1,2):
							lblock.append(m)
							rblock=setinter(keyz,rblock)
				if len(lblock)<2 or len(rblock)<2:
					continue
				print 'PROBABLE BLOCK match of\n\t',lblock,'\nwith\n\t',rblock
				cx += 1
				inc('probable block',len(lblock))
				for n1 in lblock:
					rows.append(n1)
				for n2 in rblock:
					cols.append(n2)
				change = True
				for x in lblock:
					if x in matches.keys():
						matches.pop(x)
					for y in amatches.keys():
						if x in amatches[y]:
							amatches[y].pop(x)
				for x in rblock:
					if x in amatches.keys():
						amatches.pop(x)
					for y in matches.keys():
						if x in matches[y]:
							matches[y].pop(x)
				break
		if change: continue
		# max
		for n in matches.keys():
			valz = matches[n].values()
			mx = max(valz)
			valz.remove(mx)
			if len(valz)==0:
				# little trick, never mind; bottom line: max of a sequence of one is always happening
				valz=[-1]
			if mx>max(valz):
				# only one maximum
				for k in matches[n].keys():
					if matches[n][k] == mx:
						m = k
						break
				print 'MAX match of',n,'with',m
				cx += 1
				inc('max',1)
				rows.append(n)
				cols.append(m)
				change = True
				matches.pop(n)
				amatches.pop(m)
				for k in matches.keys():
					if m in matches[k].keys():
						matches[k].pop(m)
				for k in amatches.keys():
					if n in amatches[k].keys():
						amatches[k].pop(n)
				break
	# nothing to do, let's print
	inc('none',len(matches.keys()))
	if len(matches)>0:
		print 'THE REST are:',matches.keys()
		for k in matches.keys():
			print k,'matches with',matches[k]
			rows.append(k)
	for k in amatches.keys():
		cols.append(k)
	#print 'ROWS:',rows
	for n in allrows:
		if n not in rows:
			print 'Fixing an empty row for',n
			rows.append(n)
			inc('none',1)
	#print 'COLS:',cols
	for n in allcols:
		if n not in cols:
			print 'Fixing an empty col for',n
			cols.append(n)
	print 'TOTAL:',cx,'(',sum(total.values()),')','matches,',len(matches),'unmatched.'
	reporttotals(two)
	# write a table
	f = open(sys.argv[3],'w')
	matrix = []
	for i in range(0,len(rows)):
		matrix.append([0]*len(cols))
	for p in pairs:
		matrix[rows.index(p[0].split('::')[1])][cols.index(p[1].split('::')[1])] = (0.0+p[2])/p[3]
	for i in cols:
		f.write(' , '+str(i))
	f.write('\n')
	for i in range(0,len(matrix)):
		f.write(str(rows[i]))
		for j in range(0,len(matrix[i])):
			f.write(' , '+str(matrix[i][j]))
		f.write('\n')
	f.close()
	sys.exit(0)

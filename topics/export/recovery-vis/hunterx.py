#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import RPL
import os, sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF3
from functools import reduce

# debug = False
debug = True

# RPL = Recovery Process Log
vis = RPL.RPL('recovery.rpl')

defaults = {'definition-separator-symbol':'|||||'}
config = {}
masked = {}
always_terminals = []
always_nonterminals = []
ignore_tokens = []
ignore_lines = []
nonterminals_alphabet = ['-','_']
nonterminals_start = []
multiples = []
aliases = {}

metasymbols = \
	[
		'DEFINING-SYMBOL',
		'TERMINATOR-SYMBOL',
		'MULTIPLE-DEFINING-SYMBOL',
		'DEFINITION-SEPARATOR-SYMBOL',
		'START-GROUP-SYMBOL',
		'END-GROUP-SYMBOL',
		'START-OPTION-SYMBOL',
		'END-OPTION-SYMBOL',
		'START-REPETITION-STAR-SYMBOL',
		'END-REPETITION-STAR-SYMBOL',
		'START-REPETITION-PLUS-SYMBOL',
		'END-REPETITION-PLUS-SYMBOL',
		'START-SEPLIST-STAR-SYMBOL',
		'END-SEPLIST-STAR-SYMBOL',
		'START-SEPLIST-PLUS-SYMBOL',
		'END-SEPLIST-PLUS-SYMBOL',
		'POSTFIX-OPTION-SYMBOL',
		'POSTFIX-REPETITION-STAR-SYMBOL',
		'POSTFIX-REPETITION-PLUS-SYMBOL',
	]
specials = \
	[
		'POSSIBLE-TERMINATOR-SYMBOL',
		'CONCATENATE-SYMBOL',
		'LINE-CONTINUATION-SYMBOL'
		'START-TERMINAL-SYMBOL',
		'END-TERMINAL-SYMBOL',
		'START-NONTERMINAL-SYMBOL',
		'END-NONTERMINAL-SYMBOL',
		'NONTERMINAL-IF-CONTAINS',
		'NONTERMINAL-IF-DEFINED',
		'NONTERMINAL-IF-UPPERCASE',
		'NONTERMINAL-IF-LOWERCASE',
		'NONTERMINAL-IF-CAMELCASE',
		'NONTERMINAL-IF-MIXEDCASE',
		'TERMINAL-IF-UNDEFINED',
		'TERMINAL-IF-UPPERCASE',
		'TERMINAL-IF-LOWERCASE',
		'TERMINAL-IF-CAMELCASE',
		'TERMINAL-IF-MIXEDCASE',
		'IGNORE-EXTRA-NEWLINES',
		'GLUE-NONALPHANUMERIC-TERMINALS',
	]
specials.extend(metasymbols)

def recordStep(s):
	global vis
	print('STEP '+s)
	vis.add(RPL.Step(s))

def recordMsg(s):
	global vis
	print('STEP '+s)
	vis.add(RPL.Message(s))

def reportprods(prods,terminator,final):
	global vis
	print('The grammar is perceived like this:')
	for p in prods:
		print('\t',p[1],'is defined as',p[2:])
	tts = RPL.TokenSeq()
	# ts.prods2tokens(prods)
	# vis.write('<pre>')
	for p in prods:
		ts = RPL.TokenSeq()
		# vis.write('<span class="box nt">'+p[1]+'</span> ')
		ts.add(RPL.Token(p[1],'nt'))
		if 'defining-symbol' in config.keys():
			# vis.write('<span class="box meta">'+config['defining-symbol']+'</span> ')
			ts.add(RPL.Token(config['defining-symbol'],'meta'))
		for t in p[2:]:
			if not terminator and 'terminator-symbol' in config.keys() and t == config['terminator-symbol']:
				# vis.write('<span class="box meta">'+config['terminator-symbol'].replace('\t','⇥').replace('\n','↩').replace(' ','␣')+'</span>')
				ts.add(RPL.Token(config['terminator-symbol'],'meta'))				
			elif 'start-terminal-symbol' in config.keys() and t[0]==config['start-terminal-symbol'] and t[-1]==config['end-terminal-symbol']:
				# vis.write('<span class="box t">'+t[1:-1]+'</span> ')
				ts.add(RPL.Token(t[1:-1],'t'))
			elif 'start-nonterminal-symbol' in config.keys() and t[0]==config['start-nonterminal-symbol'] and t[-1]==config['end-nonterminal-symbol']:
				# vis.write('<span class="box nt">'+t[1:-1]+'</span> ')				
				ts.add(RPL.Token(t[1:-1],'nt'))
			elif final and 'start-nonterminal-symbol' not in config.keys():
				# vis.write('<span class="box nt">'+t+'</span> ')				
				ts.add(RPL.Token(t,'nt'))
			else:
				# vis.write('<span class="box">'+t+'</span> ')
				ts.add(RPL.Token(t,''))
		if terminator and 'terminator-symbol' in config.keys():
			# vis.write('<span class="box meta">'+config['terminator-symbol'].replace('\t','⇥').replace('\n','↩').replace(' ','␣')+'</span>')
			ts.add(RPL.Token(config['terminator-symbol'],'meta'))
		# vis.write('\n')
		tts.add(ts)
	# vis.write('</pre>')
	vis.add(tts)

def reporttokens(tokens):
	global vis
	ts = RPL.TokenSeq()
	# vis.write('<pre>')
	for t in tokens:
		if t == '\n':
			# vis.write('<span class="box">↩</span>\n')
			# ts.add(RPL.Token('↩',''))
			ts.add(RPL.Token('\n',''))
		elif 'start-terminal-symbol' in config.keys() and t[0]==config['start-terminal-symbol'] and t[-1]==config['end-terminal-symbol']:
			# vis.write('<span class="box t">'+t[1:-1]+'</span> ')
			ts.add(RPL.Token(t[1:-1],'t'))
		elif 'start-nonterminal-symbol' in config.keys() and t[0]==config['start-nonterminal-symbol'] and t[-1]==config['end-nonterminal-symbol']:
			# vis.write('<span class="box nt">'+t[1:-1]+'</span> ')				
			ts.add(RPL.Token(t[1:-1],'nt'))
		else:
			# vis.write('<span class="box">'+t+'</span> ')
			ts.add(RPL.Token(t,''))
	# vis.write('</pre>')
	vis.add(ts)

def isUpperCase(x):
	return reduce(lambda a,b:a and (b.isalpha() or b in nonterminals_alphabet) and b.isupper(),x,True)

def isLowerCase(x):
	return reduce(lambda a,b:a and (b.isalpha() or b in nonterminals_alphabet) and b.islower(),x,True)

def isCamelCase(x):
	return isUpperCase(x[0]) and not isUpperCase(x) and isAlpha(x)

def isMixedCase(x):
	return isLowerCase(x[0]) and not isLowerCase(x) and isAlpha(x)

def isAlphaNum(x):
	return reduce(lambda a,b:a and (b.isalnum() or b in nonterminals_alphabet),x,True)

def isAlpha(x):
	return reduce(lambda a,b:a and (b.isalpha() or b in nonterminals_alphabet),x,True)

def isQNumber(x):
	if x =='.':
		return False
	else:
		return reduce(lambda a,b:a and (b=='.' or b.isdigit()),x,True)

def removeComments(ts,s,e):
	while s in ts:
		i = ts.index(s)
		# special case
		if i>1 and 'start-terminal-symbol' in config.keys() and ts[i-1:i+2]==[config['start-terminal-symbol'],s,config['end-terminal-symbol']]:
			recordStep('0: adjusted for the comment starting symbol being used as a terminal.')
			nts = ts[:i-1]
			nts.append(ts[i-1]+ts[i]+ts[i+1])
			nts.extend(ts[i+2:])
			ts = nts
			continue
		j = endOfContext(ts,i,e)
		if j<0:
			recordMsg('0 error: mismatched comment delimiters.')
			j = i
		nts = ts[:i]
		nts.extend(ts[j:])
		#print('<<<',ts)
		ts = nts
		#print('>>>',ts)
	return ts
	
def splitTokenStreamByAlphas(s):
	global debug
	ts = [s[0]]
	i = 1
	alpha = isAlphaNum(s[0])
	inQuotes = False
	while (i<len(s)):
		# if debug:
		# 	print('Examining',s[i],'with inQuotes=',inQuotes,':',ts[-1])
		if 'start-terminal-symbol' in config.keys() and 'end-terminal-symbol' in config.keys():
			if not inQuotes and s[i] == config['start-terminal-symbol']:
				ts.append(s[i])
				inQuotes = True
				i += 1
				continue
			elif inQuotes:
				if s[i] == config['end-terminal-symbol']:
					inQuotes = False
				ts[-1] += s[i]
				i += 1
				alpha = False
				continue
		if alpha:
			if isAlphaNum(s[i]):
				ts[-1] += s[i]
			else:
				alpha = False
				ts.append(s[i])
		else:
			ts.append(s[i])
			alpha = isAlphaNum(s[i])
		i += 1
	if 'tabulation-symbol' in config.keys():
		ts = mapglue(ts,config['tabulation-symbol'])
		return ['\t' if t=='TABULATION' else t for t in filter(lambda x:x not in [' ',' ','	'],['TABULATION' if t == config['tabulation-symbol'] else t for t in ts])]
	return list(filter(lambda x:x not in [' ',' ','	'],ts))
	# not space, not hard space, not tab; newlines are preserved for now

def reconsiderSpaces(ts,sep,vs):
	nts = [ts[0]]
	vs = list(vs)
	vs.append('\n')
	for x in ts[1:]:
		if x in ignore_tokens:
			continue
		if x == sep:
			nts.append('')
		elif nts[-1] in vs or x in vs:
			if nts[-1]=='':
				nts[-1] = x
			else:
				nts.append(x)
		else:
			nts[-1] += ' ' + x
	return nts

def readConfig(f):
	global debug
	cfg = ET.parse(f)
	for e in cfg.findall('*'):
		if e.tag == 'mask':
			if e.findall('terminal'):
				masked[e.findtext('token')] = e.findtext('terminal')
			elif e.findall('epsilon'):
				masked[e.findtext('token')] = 'EPSILON'
			else:
				print('Unknown masked token:',e.findtext('token'))
		elif e.tag == 'nonterminals-may-contain':
			for x in e.text:
				nonterminals_alphabet.append(x)
		elif e.tag == 'nonterminals-may-start-with':
			for x in e.text:
				nonterminals_start.append(x)
		elif e.tag == 'ignore':
			#config[e.tag] = ''
			for x in e.findall('*'):
				if x.tag == 'newline':
					ignore_tokens.append('\n')
					ignore_tokens.append('@@@0-0')
				elif x.tag == 'space':
					ignore_tokens.append(' ')
				elif x.tag == 'lines-containing':
					ignore_lines.append(x.text)
				elif x.tag == 'same-indentation':
					ignore_tokens.append('@@@1-1')
				else:
					ignore_tokens.append(x.text)
		elif e.tag == 'alias':
			for x in e.findall('*'):
				if x.tag not in aliases.keys():
					aliases[x.tag] = []
				aliases[x.tag].append(x.text)
		elif e.text:
			config[e.tag] = e.text.replace('\\n','\n')
		else:
			config[e.tag] = ''
		if e.tag in ('nonterminal-if-camelcase','nonterminal-if-mixedcase','nonterminal-if-uppercase','nonterminal-if-lowercase','nonterminal-if-contains','nonterminal-if-defined','decompose-symbols'):
			if e.text:
				config[e.tag] = e.text
			else:
				config[e.tag] = ''
			for x in e.findall('except'):
				always_terminals.append(x.text)
		if e.tag in ('terminal-if-camelcase','terminal-if-mixedcase','terminal-if-uppercase','terminal-if-lowercase','terminal-if-undefined'):
			config[e.tag] = ''
			for x in e.findall('except'):
				always_nonterminals.append(x.text)
	if debug:
		print('Ok',config)

# bucket sort
def calculateFrequencies(arr):
	fs = {}
	for x in arr:
		if x not in fs.keys():
			fs[x] = 1
		else:
			fs[x] += 1
	return fs

def bestFrequency(fs):
	bestv = max(fs.values())
	for x in fs.keys():
		if fs[x] == bestv:
			bestk = x
			break
	return bestk,bestv

# Use terminator symbol to distinguish productions
def useTerminatorSymbol(ts,t):
	ps = [[]]
	for x in ts:
		if x == t:
			ps.append([])
		else:
			ps[-1].append(x)
	#ps = list(filter(lambda x:x!=[],ps))
	if 'start-label-symbol' in config.keys() or 'end-label-symbol' in config.keys():
		recordMsg('4: guessing defining-symbol in a grammar with labels not implemented yet!')
		return None,None
	dss = list(map(lambda x:'' if len(x)<2 else x[1],filter(lambda x:x!=[],ps)))
	bestds,bestdsvalue = bestFrequency(calculateFrequencies(filter(lambda x:x!='',dss)))
	prob = bestdsvalue * 100.0 / len(dss)
	return prob,bestds

# Use defining symbol to distinguish productions
def useDefiningSymbol(ts,d):
	global debug
	poss = []
	prods = []
	for i in range(0,len(ts)):
		if ts[i] == d:
			j = i-1
			while j>-1 and ts[j] in ignore_tokens:
				j -= 1
			if isAlphaNum(ts[j]) \
			or (ts[j][0]==config['start-terminal-symbol'] and ts[j][-1]==config['end-terminal-symbol'] and isAlphaNum(ts[j][1:-1]))\
			or 'start-nonterminal-symbol' in config.keys() and 'end-nonterminal-symbol' in config.keys() and (ts[j][0]==config['start-nonterminal-symbol'] and ts[j][-1]==config['end-nonterminal-symbol'] and isAlphaNum(ts[j][1:-1])):
				poss.append(i)
	poss.append(len(ts)+1)
	#print('POSSSSS:',poss)
	if debug:
		print('Positions:',poss)
	for i in range(0,len(poss)-1):
		if 'end-label-symbol' in config.keys():
			if ts[poss[i]-2] == config['end-label-symbol']:
				if 'start-label-symbol' in config.keys():
					# todo: now works only with one-token labels!
					if ts[poss[i]-4] == config['start-label-symbol']:
						# everything is fine
						p = [ts[poss[i]-3],ts[poss[i]-1]]
					else:
						recordMsg('4 problem: start-label-symbol mismatch!')
						# todo: recover
				else:
					# no starting symbol for the label
					j = poss[i]-1
					while ts[j] in ignore_tokens:
						j -= 1
					k = j-2
					while ts[k] in ignore_tokens:
						k -= 1
					p = [ts[k],ts[j]]
			else:
				# no label this time
				j = poss[i]-1
				while ts[j] in ignore_tokens:
					j -= 1
				p = ['',ts[j]]
		else:
			# no labels at all
			j = poss[i]-1
			while ts[j] in ignore_tokens:
				j -= 1
			p = ['',ts[j]]
		end = poss[i+1]-1
		while -1<end<len(ts) and ts[end] in ignore_tokens:
			end -= 1
		if 'end-label-symbol' in config.keys() and ts[end-1] == config['end-label-symbol']:
			end -= 2
			if 'start-label-symbol' in config.keys() and ts[end-1] == config['start-label-symbol']:
				end -= 1
		p.extend(ts[poss[i]+1:end])
		prods.append(p)
	# all left hand sides need to be nonterminals!
	for i in range(0,len(prods)):
		if prods[i][1][0] == config['start-terminal-symbol'] and prods[i][1][-1] == config['end-terminal-symbol']:
			recordMsg('4 warning: terminal on the left hand side of a production, fixed by turning it back into a nonterminal.')
			prods[i][1] = prods[i][1][1:-1]
	return prods

def useDefinitionSeparatorSymbol(ts,d):
	poss = [-1]
	alts = []
	levels = [0,0,0]
	for i in range(0,len(ts)):
		if 'start-group-symbol' in config.keys() and ts[i]==config['start-group-symbol']:
			levels[0] += 1
		if 'end-group-symbol' in config.keys() and ts[i]==config['end-group-symbol']:
			levels[0] -= 1
		if 'start-repeat-symbol' in config.keys() and ts[i]==config['start-repeat-symbol']:
			levels[1] += 1
		if 'end-repeat-symbol' in config.keys() and ts[i]==config['end-repeat-symbol']:
			levels[1] -= 1
		if 'start-option-symbol' in config.keys() and ts[i]==config['start-option-symbol']:
			levels[2] += 1
		if 'end-option-symbol' in config.keys() and ts[i]==config['end-option-symbol']:
			levels[2] -= 1
		if ts[i] == d:
			if levels == [0,0,0]:
				poss.append(i)
			else:
				ts[i] = 'DEFINITION-SEPARATOR-SYMBOL'
	poss.append(len(ts)+1)
	for i in range(0,len(poss)-1):
		alts.append(ts[poss[i]+1:poss[i+1]])
	return alts

def findMostProbableTail(ps,known):
	# bucket sort
	ss = calculateFrequencies(map(lambda x:x[-1],ps))
	# at least 80% has the same end symbol?
	# TODO: describe the heuristic
	vs = list(ss.values())
	m = max(vs)
	vs.remove(m)
	m2 = max(vs)
	for k in ss.keys():
		if ss[k] == m:
			break
	#print('m=',m,'m2=',m2,'len(ps)=',len(ps))
	if k != known and m < max(0.25*len(ps),2*m2):
		possibles = []
		for i in ss.keys():
			if ss[i]>1:
				possibles.append((i,ss[i]))
		print('Candidates were:',possibles,'with total',len(ps))
		return None,None,None
	n2f = []
	fps = []
	cx = 0
	for p in ps:
		if p[-1] == k:
			fps.append(p)
		else:
			n2f.append(cx)
		cx += 1
	return n2f,findCommonTail(fps),100*m/len(ps)

def findCommonTail(ps):
	tail = []
	for i in range(1,len(ps[0])):
		if [x[-i] for x in ps] == [ps[0][-i]]*len(ps):
			tail.append(ps[0][-i])
		else:
			break
	tail.reverse()
	return tail

def assembleBracketedSymbols(ts,start,end,preserveSpace):
	tss = []
	inside = False
	i = 0
	while (i<len(ts)):
		if inside:
			if preserveSpace and ts[i] != end and tss[-1][-1] != start:
				tss[-1] += ' '
			tss[-1] += ts[i]
			if ts[i] == end:
				inside = False
			elif ts[i] == start:
				# we do not allow nested bracketed symbols
				recordMsg('x ERROR: unbalanced bracketed metasymbols',repr(start),'and',repr(end))
				if preserveSpace:
					last = tss[-1].split(' ')
					tss[-1] = last[0]
					tss[-1] += end
					tss.extend(last[1:])
					#tss.append(ts[i])
				else:
					tss[-1] += end
					tss.append(ts[i])
		else:
			tss.append(ts[i])
			if ts[i] == start:
				inside = True
		i += 1
	return tss

def findGroups(ats,start,end):
	global debug
	poss = [[],[]]
	i = j = level = 0
	lp = {}
	for i in range(0,len(ats)):
		for j in range(0,len(ats[i])):
			if ats[i][j] == start:
				poss[0].append((i,j))
				poss[1].append(None)
				lp[level] = len(poss[0])-1
				level += 1
			if ats[i][j] == end:
				level -= 1
				poss[1][lp.pop(level)] = (i,j)
	if len(poss[0]) != len(poss[1]):
		recordMsg('8 deadlock: number of start-group-symbol and end-group-symbol occurrences do not match.')
		return ats
	if debug and poss[0]:
		print('poss >>>>>',poss)
		print('ats >>>>>>',ats)
	for i in range(0,len(poss[0])):
		if poss[0][i][0] == poss[1][i][0]:
			# same alternative
			tmp = ats[poss[0][i][0]][:poss[0][i][1]]
			tmp.append([ats[poss[0][i][0]][poss[0][i][1]:poss[1][i][1]][1:]])
			tmp.extend(ats[poss[0][i][0]][poss[1][i][1]+1:])
			ats[poss[0][i][0]] = tmp
			#print '-->',tmp
		else:
			newats = []
			for j in range(0,len(ats)):
				if j<poss[0][i][0]:
					newats.append(ats[j])
				elif j>poss[1][i][0]:
					newats.append(ats[j])
				elif j==poss[0][i][0]:
					before = ats[j][:poss[0][i][1]]
					tmp = [ats[j][poss[0][i][1]+1:]]
				elif j==poss[1][i][0]:
					tmp.append(ats[j][:poss[1][i][1]])
					before.append(tmp)
					before.extend(ats[j][poss[1][i][1]+1:])
					newats.append(before)
				else:
					tmp.append(ats[j])
			ats = newats
			#print '--> not implemented yet'
	if len(ats) == 1 and len(ats[0]) == 1 and type(ats[0][0]) == type([]):
		ats = ats[0][0]
	return ats

def findSpecialGroups(ats,start,end):
	global debug
	poss = [[],[]]
	i = j = level = 0
	lp = {}
	for i in range(0,len(ats)):
		for j in range(0,len(ats[i])):
			if ats[i][j] == start:
				poss[0].append((i,j))
				poss[1].append(None)
				lp[level] = len(poss[0])-1
				level += 1
			if ats[i][j] == end:
				level -= 1
				poss[1][lp.pop(level)] = (i,j)
	if len(poss[0]) != len(poss[1]):
		recordMsg('8 deadlock: number of start-?-symbol and end-?-symbol occurrences do not match.')
		return ats
	if debug and poss[0]:
		print('poss >>>>>',poss)
		print('ats >>>>>>',ats)
	for i in range(0,len(poss[0])):
		if poss[0][i][0] != poss[1][i][0]:
			newats = []
			for j in range(0,len(ats)):
				if j<poss[0][i][0]:
					newats.append(ats[j])
				elif j>poss[1][i][0]:
					newats.append(ats[j])
				elif j==poss[0][i][0]:
					before = ats[j][:poss[0][i][1]+1]
					tmp = [ats[j][poss[0][i][1]+1:]]
				elif j==poss[1][i][0]:
					tmp.append(ats[j][:poss[1][i][1]])
					before.append(tmp)
					before.extend(ats[j][poss[1][i][1]:])
					newats.append(before)
				else:
					tmp.append(ats[j])
			ats = newats
	if len(ats) == 1 and len(ats[0]) == 1 and type(ats[0][0]) == type([]):
		ats = ats[0][0]
	return ats

def mapsymbols(ts,symbol,special):
	for i in range(0,len(ts)):
		if ts[i] == symbol:
			ts[i] = special
		elif type(ts[i]) == type([]):
			ts[i] = mapsymbols(ts[i],symbol,special)
	return ts

def endOfContext(a,i,e):
	s = a[i]
	level = 1
	j = i+1
	#print('Searching in a context of',a[i:],'for',e)
	while level>0 and j<len(a):
		if a[j]==s:
			level += 1
		elif a[j]==e:
			level -= 1
		j += 1
	if level != 0:
		#print('Cannot find end of context in:',a[i:],'@',level)
		return -level
	return j

def startOfContext(a,i,s):
	e = a[i]
	level = 1
	j = i-1
	#print('Searching in a context of',a[:i+1],'for',s)
	while level>0 and j>=0:
		if a[j] == e:
			level += 1
		elif a[j] == s:
			level -= 1
		j -= 1
		#print('j=',j,'a[j]=',a[j],'level=',level)
	if level != 0:
		#print('Cannot find end of context in:',a[:i+1],'@',level)
		return -level
	return j

def map2expr(ss):
	#global debug
	debug = False
	ess = []
	es = []
	i = 0
	while i<len(ss):
		if ss[i] == 'START-REPETITION-STAR-SYMBOL':
			j = endOfContext(ss,i,'END-REPETITION-STAR-SYMBOL')
			if j<0:
				print('Unbalanced bracketing, please fix first:',ss[i:],'Level:',-j)
				j = len(ss)
			if debug:
				print('>>>context>>>',ss[i:j])
			e = BGF3.Star()
			e.setExpr(map2expr(ss[i+1:j-1]))
			es.append(e)
			i = j
		elif ss[i] == 'START-REPETITION-PLUS-SYMBOL':
			j = endOfContext(ss,i,'END-REPETITION-PLUS-SYMBOL')
			if j<0:
				print('Unbalanced bracketing, please fix first:',ss[i:],'Level:',-j)
				j = len(ss)
			if debug:
				print('>>>context>>>',ss[i:j])
			e = BGF3.Plus()
			e.setExpr(map2expr(ss[i+1:j-1]))
			es.append(e)
			i = j
		elif ss[i] == 'START-OPTION-SYMBOL':
			j = endOfContext(ss,i,'END-OPTION-SYMBOL')
			if j<0:
				print('Unbalanced bracketing, please fix first:',ss[i:],'Level:',-j)
				j = len(ss)
			if debug:
				print('>>>context>>>',ss[i:j])
			e = BGF3.Optional()
			e.setExpr(map2expr(ss[i+1:j-1]))
			es.append(e)
			i = j
		elif ss[i] == 'START-SEPLIST-STAR-SYMBOL':
			j = endOfContext(ss,i,'END-SEPLIST-STAR-SYMBOL')
			if j<0:
				print('Unbalanced bracketing, please fix first:',ss[i:],'Level:',-j)
				j = len(ss)
			if j-i != 4:
				print('Incorrect separator list:',ss[i:j])
			if debug:
				print('>>>context>>>',ss[i:j])
			# {x y}* => (x (yx)*)?
			e = BGF3.SepListStar()
			e.setItem(map2expr([ss[i+1]]))
			e.setSep(map2expr([ss[i+2]]))
			es.append(e)
			### if no bare seplist desired in BGF, uncomment the following instead
			# e = BGF3.Sequence()
			# x = map2expr([ss[i+1]])
			# y = map2expr([ss[i+2]])
			# e.add(x)
			# e2 = BGF3.Sequence()
			# e2.add(y)
			# e2.add(x)
			# s = BGF3.Star()
			# s.setExpr(BGF3.Expression(e2))
			# e.add(BGF3.Expression(s))
			# e2 = BGF3.Optional()
			# e2.setExpr(e)
			# es.append(e2)
			i = j
		elif ss[i] == 'START-SEPLIST-PLUS-SYMBOL':
			j = endOfContext(ss,i,'END-SEPLIST-PLUS-SYMBOL')
			if j<0:
				print('Unbalanced bracketing, please fix first:',ss[i:])
				j = len(ss)
			if j-i != 4:
				print('Incorrect separator list:',ss[i:j])
			if debug:
				print('>>>context>>>',ss[i:j])
			# {x y}+ => (x (yx)*)
			e = BGF3.SepListPlus()
			e.setItem(map2expr([ss[i+1]]))
			e.setSep(map2expr([ss[i+2]]))
			es.append(e)
			### if no bare seplist desired in BGF, uncomment the following instead
			# e = BGF3.Sequence()
			# x = map2expr([ss[i+1]])
			# y = map2expr([ss[i+2]])
			# e.add(x)
			# e2 = BGF3.Sequence()
			# e2.add(y)
			# e2.add(x)
			# s = BGF3.Star()
			# s.setExpr(BGF3.Expression(e2))
			# e.add(BGF3.Expression(s))
			# es.append(e)
			i = j
		elif ss[i] == 'START-GROUP-SYMBOL':
			j = endOfContext(ss,i,'END-GROUP-SYMBOL')
			if j<0:
				print('Unbalanced bracketing, please fix first:',ss[i:])
				j = len(ss)
			if i == 0 and j == len(ss):
				ss = ss[1:-1]
				continue
			if debug:
				print('>>>context>>>',ss[i:j])
			es.append(map2expr(ss[i+1:j-1]))
			i = j
		elif ss[i] == 'DEFINITION-SEPARATOR-SYMBOL':
			if debug:
				print('It is a bar! It is a choice!')
			ess.append(es)
			es = []
			i += 1
		elif ss[i][0] == config['start-terminal-symbol']:
			if debug:
				print('TERMINAL',ss[i][1:-1])
			e = BGF3.Terminal()
			if ss[i][1:-1] == '':
				print('Serialisation error: empty terminal, replaced with ""!')
				e.setName('""')
			elif ss[i] == config['start-terminal-symbol']+'EPSILON'+config['end-terminal-symbol']:
				e = BGF3.Epsilon()
			else:
				e.setName(ss[i][1:-1])
			es.append(e)
			i += 1
		elif ss[i] in metasymbols:
			recordMsg('9:',ss[i],'found untouched at serialisation stage, turned into a terminal symbol "'+config[ss[i].lower()]+'"')
			print(ss)
			ss[i] = config['start-terminal-symbol'] + config[ss[i].lower()] + config['end-terminal-symbol']
			continue
		else:
			if debug:
				print('NONTERMINAL',ss[i])
			e = BGF3.Nonterminal()
			n = ss[i]
			if 'start-nonterminal-symbol' in config.keys() or 'end-nonterminal-symbol' in config.keys():
				if n[:len(config['start-nonterminal-symbol'])] == config['start-nonterminal-symbol']:
					n = n[len(config['start-nonterminal-symbol']):]
				if n[-len(config['end-nonterminal-symbol']):] == config['end-nonterminal-symbol']:
					n = n[:-len(config['end-nonterminal-symbol'])]
			e.setName(n)
			es.append(e)
			i += 1
	ess.append(es)
	if len(ess) == 0:
		print('Serialisation error: empty output sequence!')
		return
	elif len(ess) == 1:
		if len(ess[0]) == 0:
			print('Serialisation warning: empty internal output sequence, treating like epsilon!')
			return BGF3.Expression(BGF3.Epsilon())
		elif len(ess[0]) == 1:
			return BGF3.Expression(ess[0][0])
		else:
			e = BGF3.Sequence()
			for x in ess[0]:
				e.add(BGF3.Expression(x))
			return BGF3.Expression(e)
	else:
		e = BGF3.Choice()
		for es in ess:
			if len(es) == 0:
				print('Serialisation warning: empty internal output sequence, treating like epsilon!')
				e.add(BGF3.Expression(BGF3.Epsilon()))
			elif len(es) == 1:
				e.add(BGF3.Expression(es[0]))
			else:
				ee = BGF3.Sequence()
				for x in es:
					ee.add(BGF3.Expression(x))
				e.add(BGF3.Expression(ee))
		return BGF3.Expression(e)
	print('Dead code reached!')
	return

def mapglue(b,s):
	a = b[:]
	i = 0
	while i < len(a):
		if a[i][0] != s[0]:
			# first char mismatch
			i += 1
			continue
		combined = a[i]
		j = i+1
		while j<len(a) and len(combined)<len(s):
			combined += a[j]
			j += 1
		if combined == s:
			# success
			b = a[:i]
			b.append(''.join(a[i:j]))
			b.extend(a[j:])
			#i = j
			i += 1
			a = b
		else:
			# fail
			i += 1
	return a

def filterNewlines(s):
	i = 0
	while i<len(s):
		if type(s[i])==type([]):
			s[i] = filterNewlines(s[i])
			i += 1
		elif s[i]=='\n':
			z = s[:i]
			z.extend(s[i+1:])
			s = z
		else:
			i += 1
	return s

def glueTerminals(p):
	q = []
	#print('>g>l>u>e>',p)
	for y in p[2:]:
		if y[0] != config['start-terminal-symbol'] or len(q) == 0 or q[-1][0] != config['start-terminal-symbol']:
			q.append(y)
			continue
		x = y[1:-1]
		if len(q)>0 and not isAlphaNum(q[-1][1:-1]) and not isAlphaNum(x):
			# addition on terminals
			q[-1] = q[-1][:-1] + y[1:]
		else:
			q.append(y)
	r = p[:2]
	r.extend(q)
	if debug and p != r:
		print('>>>in>>>>',p)
		print('>>>out>>>',r)
	return r

def assembleQualifiedNumbers(ts):
	ds = []
	for x in ts:
		if len(ds)>0 and (isQNumber(x) or x=='.') and isQNumber(ds[-1]):
			ds[-1] += x
		else:
			ds.append(x)
	return ds

def splitString(s,kw,defd):
	# split s according to any kws while preserving them
	global debug
	if debug:
		print('Searching for a split in',s,'with',kw)
	if len(kw)==0:
		return [s]
	elif s in defd:
		return [s]
	elif s.find(kw[0])<0:
		return splitString(s,kw[1:],defd)
	else:
		ss = s.split(kw[0])
		done = []
		for x in ss:
			if x:
				done.append(x)
			done.append(kw[0])
		done.pop()
		if done[-1]=='':
			done.pop()
		if done[0] =='':
			done = done[1:]
		res = []
		for a in done:
			res.extend(splitString(a,kw[1:],defd))
		reject = False
		if min(map(len,res))<2:
			reject = True
		if 'nonterminal-if-contains' in config.keys():
			for y in res:
				if y in defd or y.find(config['nonterminal-if-contains']) < 0:
					continue
				else:
					reject = True
					if debug:
						print('!!!!! Have to reject',res,'of',s)
					#print(kw)
		if reject:
			return splitString(s,kw[1:],defd)
		else:
			return res

def decomposeSymbols(p,defd):
	global debug
	# [label, nt, ...]
	q = p[:2]
	# nested-name-specifiertemplateopt
	for x in p[2:]:
		match = False
		# expanded for better readability
		if x in defd:
			# defined nonterminal
			q.append(x)
			continue
		if x in always_terminals:
			# configured exception
			q.append(x)
			continue
		if x[0] == config['start-terminal-symbol'] and x[-1] == config['end-terminal-symbol']:
			# terminal
			q.append(x)
			continue
		#if 'nonterminal-if-contains' in config.keys() and x.find(config['nonterminal-if-contains']) > -1:
		#	# we have a way of telling nonterminals from terminals, and this is not it
		#	q.append(x)
		#	continue
		# none of the above: it is a nonterminal, it's not defined and we have no way to dismiss it
		var = splitString(x,defd,defd)
		if debug and len(var)>1:
			print('Going to split',x,'to',var)
		if len(var)==1:
			q.append(x)
			continue
		#print(x,'-->',var)
		pos = True
		# TODO: inspection! strange dead code follows
		#for y in var:
		#	if y in defd:
		#		# or ('nonterminal-if-contains' in config.keys() and y.find(config['nonterminal-if-contains']) > -1):
		#		continue
		#	else:
		#		pos = False
		if pos:
			recordMsg('8:',x,'matches as',var)
			q.extend(var)
			# todo: need to be adjusted if the order of phases is changed
			#q.append(config['start-terminal-symbol']+t+config['end-terminal-symbol'])
			match = True
			continue
		if not match:
			q.append(x)
	return q

def convert2terminal(x,defd):
	# unfolded for better readability
	if x =='' or x=='\n':
		# probably an error, let's not deal with it here
		return x
	if x in defd:
		# defined nonterminal
		return x
	if x[0] == config['start-terminal-symbol'] and x[-1] == config['end-terminal-symbol']:
		# already a terminal
		return x
	if x in metasymbols:
		# pseudo-meta-symbol
		return x
	if x in always_nonterminals:
		# configured exception
		return x
	if x in always_terminals:
		# configured exception
		return config['start-terminal-symbol'] + x + config['end-terminal-symbol']
	if 'nonterminal-if-contains' in config.keys() and x.find(config['nonterminal-if-contains'])>-1:
		# undefined nonterminal, but banned by configuration from being converted to a terminal
		return x
	if 'nonterminal-if-uppercase' in config.keys() and len(x)>1 and isUpperCase(x):
		# configuration claims that UPPERCASE is a nonterminal
		return x
	if 'nonterminal-if-lowercase' in config.keys() and len(x)>1 and isLowerCase(x):
		# configuration claims that UPPERCASE is a nonterminal
		return x
	if 'nonterminal-if-camelcase' in config.keys() and len(x)>1 and isCamelCase(x):
		# configuration claims that CamelCase is a nonterminal
		return x
	if 'nonterminal-if-mixedcase' in config.keys() and len(x)>1 and isMixedCase(x):
		# configuration claims that mixedCase is a nonterminal
		return x
	# none of the above
	return config['start-terminal-symbol'] + x + config['end-terminal-symbol']

def convert2nonterminal(x,defd):
	# unfolded for better readability
	if len(x) < 4:
		# an tiny terminal or even less than that
		return x
	if x[0] != config['start-terminal-symbol'] or x[-1] != config['end-terminal-symbol']:
		# not a terminal at all
		return x
	y = x[1:-1]
	if y in always_terminals:
		# configured exception
		return x
	if 'terminal-if-uppercase' in config.keys() and isUpperCase(y):
		# configuration claims that UPPERCASE is a terminal
		return x
	if 'terminal-if-lowercase' in config.keys() and isLowerCase(y):
		# configuration claims that UPPERCASE is a terminal
		return x
	if 'terminal-if-camelcase' in config.keys() and isCamelCase(y):
		# configuration claims that CamelCase is a terminal
		return x
	if 'terminal-if-mixedcase' in config.keys() and isMixedCase(y):
		# configuration claims that mixedCase is a terminal
		return x
	if y in defd:
		# The moment of truth: could it be a defined nonterminal? 
		return y
	# none of the above
	return x

def balanceProd(p):
	global debug
	i = 2
	if debug:
		print('Balancing forward...')
	# balance forward
	while i<len(p):
		if p[i].find('START') != 0:
			i += 1
			continue
		j = endOfContext(p,i,p[i].replace('START','END'))
		if j<0:
			# endeavour 1: maybe there is another metasymbol with the same concrete representation?
			amb = []
			for k in config.keys():
				if config[k] == config[p[i].lower()] and k != p[i].lower():
					amb.append(k.upper())
			oldpi = p[i]
			fail = False
			for a in amb:
				p[i] = a
				j = endOfContext(p,i,p[i].replace('START','END'))
				if j>0:
					break
				else:
					fail = True
			if fail:
				recordMsg('7: Cannot forward balance a production',p,'- reverting',oldpi,'to a terminal.')
				p[i] = config['start-terminal-symbol']+config[oldpi.lower()]+config['end-terminal-symbol']
				i += 1
			elif p[i] == oldpi:
				recordMsg('7: Problem at',oldpi,'in',p[1],'- converted back to a terminal symbol.')
				p[i] = config['start-terminal-symbol']+config[oldpi.lower()]+config['end-terminal-symbol']
				i += 1
			else:
				recordMsg('7: Rebalanced ambiguity of',oldpi,'with',p[i],'in',p[1])
				i = j
		else:
			i = j
	if debug:
		print('Balancing backward...')
	# balancing backward
	i = len(p)-1
	while i>1:
		if p[i].find('END-')<0 or p[i] in ignore_tokens:
			i -= 1
			continue
		#print('>>>>>> Found',p[i])
		j = startOfContext(p,i,p[i].replace('END','START'))
		while j>-1 and p[j] in ignore_tokens:
			j -= 1
		if j<0:
			# endeavour 1: maybe there is another metasymbol with the same concrete representation?
			if debug:
				print('Endeavour 1: maybe there is another metasymbol with the same concrete representation as',repr(p[j]),'?')
				print(p)
			amb = []
			for k in config.keys():
				if config[k] == config[p[i].lower()] and k != p[i].lower():
					amb.append(k.upper())
			oldpi = p[i]
			fail = False
			if not amb:
				fail = True
			for a in amb:
				p[i] = a
				j = startOfContext(p,i,p[i].replace('END','START'))
				if j>0:
					break
				else:
					fail = True
			# if fail:
			# 	# endeavour 2: maybe there is another metasymbol with the same concrete representation as a matching start symbol?
			# 	amb = []
			# 	st = p[i].lower().replace('end','start')
			# 	for k in config.keys():
			# 		if config[k] == config[st]:
			# 			amb.append(k.upper())
			# 	print('>>>',amb)
			if fail:
				recordMsg('7: Cannot backward balance a production',p,'- reverting',oldpi,'to a terminal.')
				p[i] = config['start-terminal-symbol']+config[oldpi.lower()]+config['end-terminal-symbol']
				i -= 1
			elif p[i] == oldpi:
				print(p)
				recordMsg('7: Problem at',oldpi,'in',p[1],'- converted back to a terminal symbol.')
				p[i] = config['start-terminal-symbol']+config[oldpi.lower()]+config['end-terminal-symbol']
				i -= 1
			else:
				recordMsg('7: Rebalanced ambiguity of',oldpi,'with',p[i],'in',p[1])
				i = j
		else:
			i = j
	return p

def postfix2confix(p):
	global debug
	#print('>>>postfix>>>>confix>>>',p)
	for s in ('POSTFIX-REPETITION-PLUS-SYMBOL','POSTFIX-REPETITION-STAR-SYMBOL','POSTFIX-OPTION-SYMBOL'):
		while s in p:
			w = p.index(s)
			if w == 0:
				recordMsg('7: Impossible place for postfix operator at',p[1],'- converted to a terminal.')
				p[w] = config['start-terminal-symbol']+p[w]+config['end-terminal-symbol']
				continue
			if p[w-1] == 'END-GROUP-SYMBOL':
				# groups cease to exist if a postfix operator follows them
				# i.e., (a b)? will be [a b], not [(a b)]
				j = startOfContext(p,w-1,'START-GROUP-SYMBOL')
				if j<0:
					recordMsg('7: Impossible to balance the group preceding a postfix operator at',p[1],'- converted it to a terminal')
					p[w] = config['start-terminal-symbol']+p[w]+config['end-terminal-symbol']
					continue
				else:
					# nice message to get, but clutters the output
					if debug:
						recordMsg('7: Converted postfix metasymbol to confix notation.')
					#print('<<<p<<<',p)
					p[w-1] = s.replace('POSTFIX','END')
					p[j+1] = s.replace('POSTFIX','START')
					q = p[:w]
					q.extend(p[w+1:])
					p = q
					#print('>>>p>>>',p)
			elif p[w-1][:4] == 'END-' and p[w-1][-7:] == '-SYMBOL':
				# different groupings continue to exist if a postfix operator follows them
				# i.e., {a b}? will be [{a b}], not [a b]
				j = startOfContext(p,w-1,p[w-1].replace('END','START'))
				if j<0:
					recordMsg('7: Impossible to balance the group preceding a postfix operator, converted it to a terminal')
					p[w] = config['start-terminal-symbol']+p[w]+config['end-terminal-symbol']
					continue
				else:
					# nice message to get, but clutters the output
					if debug:
						recordMsg('7: Converted postfix metasymbol to confix notation.')
					print('<<<p<<<',p)
					q = p[:j+1]
					q.append(s.replace('POSTFIX','START'))
					q.extend(p[j+1:w])
					q.append(s.replace('POSTFIX','END'))
					q.extend(p[w+1:])
					p = q
					print('>>>p>>>',p)
			else:
				# nice message to get, but clutters the output
				if debug:
					recordMsg('7: Converted postfix metasymbol to confix notation.')
				# single element
				q = p[:w-1]
				q.append(s.replace('POSTFIX','START'))
				q.append(p[w-1])
				q.append(s.replace('POSTFIX','END'))
				q.extend(p[w+1:])
				p = q
	return p

def useTerminatorToFixProds(ps,ts):
	# TODO: will not work with labels
	nps = []
	for p in ps:
		if ts not in p:
			#recordMsg('4 warning: a production is disregarded due to the lack of terminator symbol:',p)
			recordMsg('4 warning: a production for '+p[1].strip()+' without terminator-symbol, appended one.')
			p.append(ts)
			print(p)
		while ts in p:
			i = p.index(ts)
			nps.append(p[:i])
			np = [nps[-1][0]]
			if config['defining-symbol'] not in p[i+1:]:
				tail = p[i+1:]
				for x in ignore_tokens:
					while x in tail:
						tail.remove(x)
				if len(tail)>0:
					recordMsg('4 problem: terminator-symbol without proper defining-symbol context.',tail)
					return nps
				else:
					p = tail
					continue
			else:
				nt = p[i+1:p.index(config['defining-symbol'])]
				for x in ignore_tokens:
					while x in nt:
						nt.remove(x)
				if len(nt) != 1:
					recordMsg('4 problem: cannot determine nonterminal name from',nt)
					nt = ' '.join(nt)
				else:
					nt = nt[0]
				np.append(nt)
			np.extend(p[p.index(config['defining-symbol'])+1:])
			#print('!!!<<<p<<<',p)
			p = np
			#print('!!!>>>p>>>',p)
	return nps

def considerIndentation(ts):
	nts = ['@@@0-']
	oldlevel = level = 0
	# ['A', '\n', '\t', 'B', '\n', '\t', 'C', 'D', 'E', 'F', '\n', '\t', '\t', 'G', '\n', '\t', 'H', '\n', 'X', '\n', '\t', 'Y', '\n', '\t', 'Z', '\n', 'B', '\n', '\t', 'K', '\n', 'C', '\n', '\t', 'L', '\n']
	for t in ts:
		if t == '\n':
			if nts[-1][:3] == '@@@':
				nts[-1] += '0'
				oldlevel = level
			nts.append('@@@'+str(oldlevel)+'-')
			level = 0
		elif t == '\t':
			level += 1
		elif nts[-1][:3]=='@@@':
			nts[-1] += str(level)
			oldlevel = level
			nts.append(t)
		else:
			nts.append(t)
	if nts[-1][:3] == '@@@':
		nts[-1] += '0'
	return nts

def convertNonalphanumerics2Terminals(p):
	q = p[:2]
	for x in p[2:]:
		#print('Checking',repr(x))
		if x == '' or x in ignore_tokens:
			# if we end up here, it probably indicates a bug elsewhere
			continue
		if 'consider-indentation' in config.keys():
			# TODO: make compatible with consider-indentation
			q.append(x)
			continue
		if x[:len(config['start-terminal-symbol'])] == config['start-terminal-symbol'] or x == '\n':
			# TODO: make compatible with consider-indentation
			q.append(x)
			continue
		if x in config.values():
			# if it's a possible metasymbol, let it be
			q.append(x)
			continue
		if 'start-nonterminal-symbol' in config.keys() and 'end-nonterminal-symbol' in config.keys() and x[0] == config['start-nonterminal-symbol'] and x[-1] == config['end-nonterminal-symbol']:
			# we know nonterminal name delimiters
			q.append(x)
			continue
		if isAlphaNum(x) and x[0].isalpha() and x[-1].isalnum():
			# a good alphanumeric word
			q.append(x)
			continue
		# none of the above
		if x[0]==' ' or x[-1]==' ':
			x = x.strip()
		if x in ('_','-') or x.isdigit():
			recordMsg('5 warning:',repr(x),'is assumed to be an invalid nonterminal name, converted to a terminal symbol.')
			q.append(config['start-terminal-symbol'] + x + config['end-terminal-symbol'])
			continue
		string = x[0]
		alpha = isAlphaNum(x[0])
		if alpha and not (x[0].isalpha() or x[0] in nonterminals_start):
			recordMsg('5 warning: the first letter of',x,'does not seem right, will be separated.')
			alpha = False
		for s in x[1:]:
			if alpha == isAlphaNum(s):
				string += s
			else:
				if string:
					if isAlphaNum(string):
						q.append(string)
					else:
						q.append(config['start-terminal-symbol'] + string + config['end-terminal-symbol'])
				string = s
				alpha = isAlphaNum(s)
		if string:
			if isAlphaNum(string):
				q.append(string)
			else:
				q.append(config['start-terminal-symbol'] + string + config['end-terminal-symbol'])
	return q

def splitTokenStreamByWhitespace(lines):
	ts = [''.join(lines)]
	specials = []
	for x in metasymbols:
		if x.lower() in config.keys() and config[x.lower()] not in ('',' '):
			specials.append(config[x.lower()])
	#specials = list(filter(lambda x:x not in ('',' '),config.values()))
	specials.sort(key=len)
	specials.reverse()
	#print('SPECIALS:',specials)
	i = j = 0
	while i<len(ts):
		#print('i=',i,'ts[i]=',repr(ts[i]))
		for x in specials:
			if ts[i].find(x) < 0 or ts[i] in specials:
				continue
			else:
				nts = ts[:i]
				nts.append(ts[i][:ts[i].index(x)])
				nts.append(x)
				nts.append(ts[i][ts[i].index(x)+len(x):])
				nts.extend(ts[i+1:])
				#print('old ts:',ts)
				ts = nts
				#print('new ts:',ts)
		i += 1
	nts = []
	for x in ts:
		if x in specials:
			nts.append(x)
		else:
			nts.extend(x.split(' '))
	#print('TS=',nts)
	return list(filter(lambda x:x!='',nts))

def nt2t(tokens,check):
	return [config['start-terminal-symbol']+x+config['end-terminal-symbol']
				if  len(x)>1
				and x[0]!=config['start-terminal-symbol']
				and check(x)
				and x not in always_nonterminals
			else x
				for x in tokens]

def t2nt(tokens,check):
	return [x[1:-1]
				if  len(x)>2
				and x[0]==config['start-terminal-symbol']
				and x[-1]==config['end-terminal-symbol']
				and check(x[1:-1])
				and x[1:-1] not in always_terminals
			else x
				for x in tokens]

def processLine(line,inside,chunks):
	if inside:
		if line.find(config['end-grammar-symbol'])>-1:
			inside = False
			line = line[:line.index(config['end-grammar-symbol'])]
			if line != '':
				line,inside,chunks = processLine(line,True,chunks)
				return line,False,chunks
		else:
			chunks.append(line)
	else:
		if line.find(config['start-grammar-symbol'])>-1:
			inside = True
			line = line[line.index(config['start-grammar-symbol'])+len(config['start-grammar-symbol']):]
			if line != '':
				return processLine(line,inside,chunks)
	return (line,inside,chunks)

if __name__ == "__main__":
	if len(sys.argv) != 4:
		print('Usage:')
		print('	extract.py input.txt config.edd output.bgf')
		sys.exit(-1)
	readConfig(sys.argv[2])
	# vis.write('<html xmlns="http://www.w3.org/1999/xhtml" xmlns:xhtml="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>')
	# vis.write('<title>Grammar Recovery, visualised: '+sys.argv[1]+'</title><link href="recovery.css" rel="stylesheet" type="text/css"/></head><body>')
	vis.add(RPL.Title('Grammar Recovery, visualised: '+sys.argv[1]))
	# default values for some metasymbols
	for x in defaults.keys():
		if x not in config.keys():
			config[x] = defaults[x]
	f = open(sys.argv[1],'r')
	# STEP 0: read the file, remove whitespace (?)
	recordStep('0: reading the input file.')
	lines = f.readlines()
	# vis.write('<pre>'+''.join(lines)+'</pre>')
	ts = RPL.TokenSeq()
	for l in lines:
		ts.add(RPL.Line(l.strip()))
	vis.add(ts)
	f.close()
	if 'start-grammar-symbol' in config.keys() and 'end-grammar-symbol' in config.keys():
		chunks = []
		inside = False
		for line in lines:
			(line,inside,chunks) = processLine(line,inside,chunks)
		lines = chunks
		recordMsg('0 found',len(lines),'in grammar chunks between designated delimiters.')
		if debug:
			print('Perceived lines:',lines)
	if len(lines) == 0:
		print('FINAL STEP: premature exit due to the lack of any grammar chunks.')
		sys.exit(0)
	if 'line-continuation-symbol' in config.keys():
		if 'concatenate-symbol' in config.keys():
			sep = config['concatenate-symbol']
		else:
			sep = ' '
		cx = 0
		nlines = []
		for line in lines:
			if line[:len(config['line-continuation-symbol'])] == config['line-continuation-symbol']:
				if debug:
					recordMsg('0: concatenating',repr(nlines[-1]),'and',repr(line))
				# [:-1] because there is still the \n character at the end of the last line
				nlines[-1] = nlines[-1][:-1] + sep + line
				cx += 1
			else:
				nlines.append(line)
		lines = nlines
		if cx > 0:
			recordMsg('0: found',cx,'line continuations.')
		else:
			recordMsg('0: line continuation specified, but not encountered.')
	# STEP 1: assemble terminal symbols
	#print(ignore_lines)
	for sign in ignore_lines:
		lines = list(filter(lambda x:x.find(sign)<0,lines))
	if 'break-tokens-at-whitespace' in config.keys():
		# only at whitespace
		tokens = splitTokenStreamByWhitespace(lines)
	else:
		# at any change from alphanumeric to non-alphanumeric
		tokens = list(''.join(lines))
		if debug:
			print('Token stream:',tokens)
			reporttokens(tokens)
		recordStep('1: removing whitespace and comments, assembling terminal symbols.')
		for k in masked.keys():
			if len(k)>1 and k.find('@@@')<0:
				recordMsg('1: going to glue tokens that resemble masked terminal', repr(k))
				tokens = mapglue(tokens,k)
				if debug:
					print('Token stream:',tokens)
					reporttokens(tokens)
		tokens = splitTokenStreamByAlphas(tokens)
	if debug:
		print('Token stream after splitting it:',tokens)
		reporttokens(tokens)
	if 'start-comment-symbol' in config.keys() and 'end-comment-symbol' in config.keys():
		# remove comments
		# assumption: comments are never nested!
		tokens = removeComments(mapglue(mapglue(tokens,config['start-comment-symbol']),config['end-comment-symbol']),config['start-comment-symbol'],config['end-comment-symbol'])
	if debug:
		print('Token stream:',tokens)
		reporttokens(tokens)
	if 'start-terminal-symbol' in config.keys() and 'end-terminal-symbol' in config.keys():
		tokens = [config['start-terminal-symbol']+masked[x]+config['end-terminal-symbol'] if x in masked.keys() else x for x in tokens]
		tokens = assembleBracketedSymbols(tokens,config['start-terminal-symbol'],config['end-terminal-symbol'],False)
	else:
		recordMsg('1 was of limited use, sorry: start-terminal-symbol and end-terminal-symbol are not both specified.')
		# technically we still need them to denote terminals in our internal representation
		config['start-terminal-symbol'] = config['end-terminal-symbol'] = '"'
		tokens = [config['start-terminal-symbol']+masked[x]+config['end-terminal-symbol'] if x in masked.keys() else x for x in tokens]
	# if we know special rules for telling terminals from nonterminals, now is the time to use them!
	if 'terminal-if-uppercase' in config.keys():
		recordMsg('1: all UPPERCASE tokens are considered terminals.')
		tokens = nt2t(tokens,isUpperCase)
	if 'terminal-if-lowercase' in config.keys():
		recordMsg('1: all lowercase tokens are considered terminals.')
		tokens = nt2t(tokens,isLowerCase)
	if 'terminal-if-camelcase' in config.keys():
		recordMsg('1: all CamelCase tokens are considered terminals.')
		tokens = nt2t(tokens,isCamelCase)
	if 'terminal-if-mixedcase' in config.keys():
		recordMsg('1: all mixedCase tokens are considered terminals.')
		tokens = nt2t(tokens,isMixedCase)
	# spaces instead of weird occurrences of start-terminal-symbols
	tokens = [config['start-terminal-symbol']+x+config['end-terminal-symbol']
				if x==config['start-terminal-symbol']+config['end-terminal-symbol']
				or x==config['start-terminal-symbol']
			else x
				for x in tokens]
	if debug:
		print('Token stream:',tokens)
		reporttokens(tokens)
	# STEP 2: assemble nonterminal symbols
	recordStep('2: assembling nonterminal symbols.')
	if 'start-nonterminal-symbol' in config.keys() and 'end-nonterminal-symbol' in config.keys():
		tokens = assembleBracketedSymbols(tokens,config['start-nonterminal-symbol'],config['end-nonterminal-symbol'],True)
	else:
		recordMsg('2 skipped, sorry: start-nonterminal-symbol and end-nonterminal-symbol are not both specified.')
	# if we know special rules for telling terminals from nonterminals, now is the time to use them!
	if 'nonterminal-if-uppercase' in config.keys():
		recordMsg('2: all UPPERCASE tokens are considered nonterminals.')
		tokens = t2nt(tokens,isUpperCase)
	if 'nonterminal-if-lowercase' in config.keys():
		recordMsg('2: all lowercase tokens are considered nonterminals.')
		tokens = t2nt(tokens,isLowerCase)
	if 'nonterminal-if-camelcase' in config.keys():
		recordMsg('2: all CamelCase tokens are considered nonterminals.')
		tokens = t2nt(tokens,isCamelCase)
	if 'nonterminal-if-mixedcase' in config.keys():
		recordMsg('2: all mixedCase tokens are considered nonterminals.')
		tokens = t2nt(tokens,isMixedCase)
	if 'nonterminal-if-contains' in config.keys():
		recordMsg('2: all tokens containing',repr(config['nonterminal-if-contains']),'are considered nonterminals.')
		tokens = t2nt(tokens,lambda x:x.find(config['nonterminal-if-contains'])>-1)
	# STEP 3: assembling composite metasymbols together
	if debug:
		print('Token stream:',tokens)
		reporttokens(tokens)
	recordStep('3: assembling metasymbols according to their possible values.')
	tokens = assembleQualifiedNumbers(tokens)
	for k in config.keys():
		if len(config[k])>1 and (config[k].find('\n')<0 or 'consider-indentation' not in config.keys()):
			if k in defaults.keys() and config[k] != defaults[k]:
				recordMsg('3: going to glue tokens that resemble metasymbol', repr(config[k]),'('+k+')')
			tokens = mapglue(tokens,config[k])
	# treat "multiple" productions
	if 'multiple-defining-symbol' in config.keys():
		for i in range(0,len(tokens)):
			if tokens[i] == config['multiple-defining-symbol']:
				recordMsg('3 found a multiple choice production.')
				tokens[i] = config['defining-symbol']
				j = i-1
				while tokens[j] in ignore_tokens:
					j -= 1
				multiples.append(tokens[j])
	if debug:
		print('Token stream:',tokens)
		reporttokens(tokens)
	# STEP 4: slice according to defining-symbol
	recordStep('4: splitting the token stream into productions.')
	if 'consider-indentation' in config.keys():
		# rewrite tokens with tabulation
		tokens = considerIndentation(tokens)
	if debug:
		print('After considering indentation:',tokens)
		reporttokens(tokens)
	if ' ' in nonterminals_alphabet and 'concatenate-symbol' in config.keys():
		# can only treat them together, because spaces in names without concatenation symbol are highly ambiguous
		# and concatenation symbols are never used if nonterminal names do not have spaces
		# TODO: not necessarily true, if you have both start-nonterminal-symbol and end-nonterminal-symbol!
		tokens = reconsiderSpaces(tokens,config['concatenate-symbol'],config.values())
	need2fix = []
	if 'defining-symbol' not in config.keys():
		# STEP 4: we do not have defining-symbol, too bad
		if 'terminator-symbol' in config.keys():
			# STEP 4: at least the terminator-symbol is here, can work with that
			recordMsg('4: using terminator-symbol to slice token stream into productions.')
			prob,ds = useTerminatorSymbol(tokens,config['terminator-symbol'])
			if ds:
				if prob == 100:
					recordMsg('4: inferred defining symbol is',repr(ds)+'.')
				else:
					recordMsg('4: the most probable defining symbol is',repr(ds),'with',str(int(prob))+'% certainty.')
				config['defining-symbol'] = ds
			else:
				recordMsg('4 skipped, sorry: could not infer defining-symbol.')
				print(ds,tokens)
				sys.exit(-1)
		else:
			# STEP 4: we're screwed
			recordMsg('4 in a pinch: neither defining-symbol nor terminator-symbol are specified!')
			popular = calculateFrequencies(tokens)
			highest = max(popular.values())
			solution = ['','',0]
			for sym in popular.keys():
				if popular[sym]>2:
					# TODO: threshold justification
					prob,ds = useTerminatorSymbol(tokens,sym)
					if ds:
						recordMsg('4 could have gone for terminator-symbol',repr(sym),'('+str(int(100.0*popular[sym]/highest))+'%) and defining-symbol',repr(ds),'('+str(int(prob))+'%)...')
						if prob*popular[sym]/highest > solution[2]:
							solution = sym,ds,prob*popular[sym]/highest
			if solution[2] < 50:
				recordMsg('4 skipped, sorry: inference failed.')
				sys.exit(-1)
			else:
				config['defining-symbol'] = solution[1]
				recordMsg('4 assumes defining-symbol is',repr(solution[1])+'.')
	# STEP 4: we do now have defining-symbol, yay!
	recordStep('4: using defining-symbol to slice token stream into productions.')
	prods = useDefiningSymbol(tokens,config['defining-symbol'])
	if debug:
		reportprods(prods,False,False)
	recordMsg('4: inferring terminator-symbol by looking at the productions.')
	if 'terminator-symbol' in config.keys():
		# we do have the terminator, but suppose we also had defining symbol!
		# TODO otherwise
		if len(prods) > 1:
			ts = findCommonTail(prods[:-1])
		else:
			ts = prods[0][-1]
		if ts:
			need2fix = [-1]
			prob = 100
		else:
			(need2fix,ts,prob) = findMostProbableTail(prods,config['terminator-symbol'])
		if ''.join(ts) == config['terminator-symbol']:
			recordMsg('4 confirmed terminator-symbol, congratulations!')
		else:
			recordMsg('4 would have thought that terminator-symbol is '+repr(''.join(ts))+' and not '+repr(config['terminator-symbol']))
		# now let's fix productions that were joined together
		prods = useTerminatorToFixProds(prods,config['terminator-symbol'])
	else:
		ts = findCommonTail(prods[:-1])
		if ts:
			recordMsg('4 successful: inferred terminator-symbol:',ts)
			config['terminator-symbol'] = ts
			need2fix = [-1]
		else:
			(need2fix,ts,prob) = findMostProbableTail(prods,'')
			if ts:
				recordMsg('4 successful: inferred the most probable terminator-symbol:',repr(ts[0]),',','%i'%prob+'% sure')
				config['terminator-symbol'] = ts[0]
			else:
				# ORLY?
				recordMsg('4 unsuccessful, sorry: will assume terminator-symbol to be empty.')
				for p in prods:
					print('%40s'%p[1],'>>>>>>',p[-2:])
				config['terminator-symbol'] = ''
				need2fix = []
	# STEP 4a.1: [sanity check] Infer terminator-symbol
	if debug:
		reportprods(prods,True,False)
	# STEP 4a.2: adjusting the terminator-symbol on the unfit productions
	poststep4 = 0
	for f in need2fix:
		for i in range(0,len(config['terminator-symbol'])):
			if prods[f][-len(config['terminator-symbol'])+i:] == config['terminator-symbol'][:len(config['terminator-symbol'])-i]:
				prods[f] = prods[f][:-len(config['terminator-symbol'])+i]
				prods[f].extend(config['terminator-symbol'])
				poststep4 += 1
				break
		if ''.join(prods[f][-len(config['terminator-symbol'])-1:-1]) == config['terminator-symbol'] and prods[f][-1] == '\n':
			prods[f].pop()
			poststep4 += 1
	#print('ignored tokens',ignore_tokens)
	ii = list(filter(lambda x:x[:3]=='@@@',ignore_tokens))
	if 'consider-indentation' in config.keys() and len(ii)>0:
		# rewrite tokens with tabulation
		#tokens = considerIndentation(tokens)
		for x in ii:
			prods = [list(filter(lambda y:y!=x,p)) for p in prods]
	if poststep4 > 0:
		recordMsg('4 also adjusted '+str(poststep4)+' productions that did not quite fit the expectations.')
	if 'possible-terminator-symbol' in config.keys():
		no = yes = 0
		for i in range(0,len(prods)):
			j = len(prods[i])-1
			while prods[i][j] in ignore_tokens:
				j -= 1
			if prods[i][j] == config['possible-terminator-symbol']:
				yes += 1
				prods[i] = prods[i][:j]
			else:
				no += 1
		recordMsg('4 found '+str(yes)+' productions using possible terminator symbol and '+str(no)+' productions not using it.')
	if debug:
		reportprods(prods,True,False)
	# STEP 4b: splitting the token stream into productions according to terminator-symbol; inferring defining-symbol
	# TODO
	prods = [p[:-(len(config['terminator-symbol']))] if p[-(len(config['terminator-symbol'])):] == config['terminator-symbol'] else p for p in prods]
	# STEP 5: decompose symbols
	defined = [x[1] for x in prods]
	if debug:
		print('Defined are',defined)
	for x in config.values():
		if x and isAlphaNum(x):
			defined.append(x)
	# are there any nonterminals defined as "one of"?
	if multiples:
		nprods = []
		for p in prods:
			if p[1] in multiples:
				q = p[:3]
				for y in p[3:]:
					if y in ignore_tokens:
						continue
					q.append('DEFINITION-SEPARATOR-SYMBOL')
					if config['definition-separator-symbol'] == y:
						q.append(config['start-terminal-symbol']+y+config['end-terminal-symbol'])
					else:
						q.append(y)
				nprods.append(q)
			else:
				nprods.append(p)
		prods = nprods
		if debug:
			print('After treating multiples the grammar is perceived like this:')
			# TODO
			reportprods(prods,True,False)
	# STEP 4 end: removing extra whitespace
	if ' ' in ignore_tokens:
		recordMsg('4 took care of extra spaces that were left unused at this point.')
		prods = [[x.strip() for x in p] for p in prods]
		if debug:
			reportprods(prods,True,False)
	# STEP 5: non-alphanumerics
	recordStep('5 (part of rule 5): converting non-alphanumeric nonterminal symbols to terminals.')
	prods = list(map(convertNonalphanumerics2Terminals,prods))
	# STEP 5: decomposition
	defined.extend(always_nonterminals)
	if 'decompose-symbols' in config.keys():
		recordStep('5 (part of rule 4): decomposing compound symbols.')
		prods = [decomposeSymbols(x,defined) for x in prods]
		if debug:
			reportprods(prods,True,False)
	# STEP 6: slice insides according to definition-separator-symbol
	step6 = False
	for z in metasymbols:
		if z == 'TERMINATOR-SYMBOL':
			continue
		s = z.lower()
		if s in config.keys():
			if s in defaults.keys() and config[s] != defaults[s]:
				recordStep('6: marking',repr(config[s]),'as',s+'.')
			step6 = True
			prods = [[s.upper() if x==config[s] else x for x in p] for p in prods]
			#prods = list(map(lambda p:list(map(lambda x:s.upper() if x==config[s] else x,p)),prods))
	if not step6:
		recordMsg('6 skipped: sorry, no metasymbols specified.')
	# STEP 7: validating metasymbols
	if debug:
		reportprods(prods,True,False)
	prods = list(map(postfix2confix,prods))
	if debug:
		recordStep('X: postfix to confix')
		reportprods(prods,True,False)
	prods = list(map(balanceProd,prods))
	if debug:
		recordStep('X: balance productions')
		reportprods(prods,True,False)
	# STEP 8: various commands
	recordStep('8: executing special extraction commands.')
	step8 = False
	if len(ignore_tokens)>0:
		recordStep('8: ignoring extra tokens.')
		step8 = True
		for x in ignore_tokens:
			prods = [list(filter(lambda y:y!=x,p)) for p in prods]
		#prods = list(map(lambda x:filter(lambda y:y!='\n',x),prods))
		if debug:
			reportprods(prods,True,False)
	# quite usual trick, harmless for most grammars
	if 'terminal-if-undefined' in config.keys():
		recordStep('8 (rule 5): turning undefined nonterminals into terminals.')
		step8 = True
		prods = [[convert2terminal(x,defined) for x in p] for p in prods]
		if debug:
			reportprods(prods,True,False)
	# should be used very carefully because it is common for grammars to have terminals and nonterminals with the "same" name
	if 'nonterminal-if-defined' in config.keys():
		recordStep('8 (rule 6): turning terminals that look like defined nonterminals into nonterminals.')
		step8 = True
		prods = [[convert2nonterminal(x,defined) for x in p] for p in prods]
		if debug:
			reportprods(prods,True,False)
	if 'glue-nonalphanumeric-terminals' in config.keys():
		recordStep('8 (part of rule 3): glueing non-alphanumeric terminal symbols together.')
		step8 = True
		prods = list(map(glueTerminals,prods))
		if debug:
			reportprods(prods,True,False)
	#for p in prods:
	#	print(p[1],'is defined as',p[2:])
	if not step8:
		recordMsg('8 skipped, sorry: no special commands found in the configuration.')
	# STEP X: validating bracketing?
	# ...
	# RESULT
	if ' ' in nonterminals_alphabet:
		# expanded from list comprehension for enhanced readability
		print('LAST STEP: replacing spaces with underscores for BGF compatibility and readability.')
		for i in range(0,len(prods)):
			#print('PRODS[i]=',prods[i])
			#if len(prods[i][0]) > 0 and (prods[i][0][0] == ' ' or prods[i][0][-1] == ' '):
			#	prods[i][0] = prods[i][0].strip()
			#if len(prods[i][1]) > 0 and (prods[i][1][0] == ' ' or prods[i][1][-1] == ' '):
			#	prods[i][1] = prods[i][1].strip()
			for j in range(0,len(prods[i])):
				if len(prods[i][j])<2:
					continue
				#if prods[i][j][0] == config['start-terminal-symbol']:
				#	prods[i][j] = prods[i][j].strip()
				if prods[i][j][0] == config['start-terminal-symbol'] and prods[i][j][-1] == config['end-terminal-symbol']:
					continue
				# len(x)<3 -- why relevant? TODO
				prods[i][j] = prods[i][j].replace(' ','_')
		if debug:
			reportprods(prods,True,False)
	#prods[i] = [x.replace(' ','_') if len(x)<3 or (x[0]!=config['start-terminal-symbol'] and x[-1]!=config['end-terminal-symbol']) else x for x in prods[i]]
	if debug:
		print('RESULT:')
		for p in prods:
			print('\t',p[1],'is defined as:',p[2:])
		reportprods(prods,True,True)
	# FINAL STEP: compose BGF
	bgf = BGF3.Grammar()
	for q in prods:
		p = BGF3.Production()
		if 'disregard-labels' not in config.keys() and q[0]:
			p.setLabel(q[0])
		if 'start-nonterminal-symbol' in config.keys() and 'end-nonterminal-symbol' in config.keys():
			p.setNT(q[1][len(config['start-nonterminal-symbol']):-len(config['end-nonterminal-symbol'])])
		else:
			p.setNT(q[1])
		p.setExpr(map2expr(q[2:]))
		bgf.addProd(p)
	ET.ElementTree(bgf.getXml()).write(sys.argv[3])
	print('FINAL STEP: BGF written.')
	# vis.write('<hr></body></html>')
	# vis.close()
	vis.dump()

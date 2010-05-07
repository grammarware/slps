#!/usr/bin/python
# -*- coding: utf-8 -*-
import sys

nt2t = 0

lines = []
grammar = {}
double = {}
current = ''
keys=[]
ignored = ['identifier','keyword','literal','string-literal']
reported = []
punctuators = [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]]


#bannedLines = ('44','45','46',"Annex A","SPECIFICATION","A.2.")
bannedLines = []
knownTerminals = []

def quote(x):
	return '"'+x+'"'

def unquote(x):
	if x[0]=='"' and x[-1]=='"':
		return x[1:-1]
	else:
		return x

def assignNewCurrent(c):
	global current
	d = performReplacements(c)
	if d not in keys:
		keys.append(d)
	current = d

def readBannedLinesList(f):
	lst = open(f,'r')
	for line in lst.readlines():
		if line.strip():
			bannedLines.append(line.strip())
	lst.close()
	
def readTerminalsList(f):
	lst = open(f,'r')
	for kw in ' '.join(lst.readlines()).split():
		knownTerminals.append(kw)
	lst.close()
	#print knownTerminals
	for kw in knownTerminals:
		if not kw.isalpha():
			try:
				punctuators[len(kw)-1].append(kw)
			except IndexError,e:
				print 'index error with',kw,len(kw)
	punctuators.reverse()
	#print 'Punctuators:',punctuators

knownPostfixes = ('+','*','?')

knownReplacements = \
	(
		('opt',' OPTIONALITYMETASYMBOL'),
		('–','"-"'),
		('˜','"~"'),
		('ﬁ','fi'),
		('[',' ['),
		('(',' ('),
		(']',' ]'),
		(')',' )'),
	)

oneof = False

def processline(line):
	global current
	global oneof
	rline = line.strip()
	if rline == '':
		return ''
	if rline[-1]==':' and rline[-2].isalpha():
		oneof = False
		# getting rid of leading stuff (perhaps labels)
		assignNewCurrent(rline[:-1].split()[-1])
		if current in grammar.keys():
			#print 'Warning: double declaration of',current
			double[current] = grammar[current][:]
		grammar[current] = []
		return
	if rline.find('one of')>0:
		oneof = True
		assignNewCurrent(rline.replace('one of','').strip()[:-1].split()[-1])
		if current in grammar.keys():
			#print 'Warning: double declaration of',current,': the first one',grammar[current],'discarded'
			double[current] = grammar[current][:]
		grammar[current] = []
		return
	if oneof:
		for t in rline.split():
			grammar[current].append(' '.join(processLineTokens(t)))
		#for t in processLineTokens(rline):
		#	grammar[current].append(t)
	else:
		grammar[current].append(' '.join(processLineTokens(rline)))
	return

def performReplacements(line):
	for x,y in knownReplacements:
		line = line.replace(x,y)
	return line

def processLineTokens(rline):
	iline = performReplacements(rline)
	tokens = iline.split()
	for i in range(0,len(tokens)):
		if tokens[i] in knownTerminals:
			tokens[i] = quote(tokens[i])
		tokens[i] = splitLeading(tokens[i],punctuators)
	return tokens
	
def splitLeading(t,arrays):
	for ps in arrays:
		for p in ps:
			if t.find(p)==0:
				t = quote(p)+' '+' '.join(processLineTokens(t[len(p):]))
	return t

def splitTrailing(t,array):
	for p in array:
		if t.find(p)>-1 and t.find(p)==len(t)-len(p):
			#print 'Found',p,'in',t,'at',t.find(p),'- result at {'+(t[:-len(p)])+' "'+p+'"'+'}'
			t = t[:-len(p)]+' '+quote(p)
	return t

def readLines(f):
	print 'Reading the PDF lines...'
	pdf = open(f,'r')
	cx = 0
	for line in pdf.readlines():
		cx += 1
		include = True
		for x in bannedLines:
			if line.find(x)>-1:
				include = False
		if include:
			if line[0] == ' ' and line[1] == ' ':
				lines[-1] = lines[-1][:-1] + line
				print 'Line continuation encountered on line', cx
			else:
				lines.append(line)
	pdf.close()

def readGrammar(f):
	print 'Reading the PDF contents...'
	for line in lines:
		#print line
		processline(line)

def writeGrammar(f):
	lll = open(f,'w')
	# we could've taken grammar.keys() here, but we want to see grammar productions order preserved
	for t in keys:
		lll.write(t+':\n')
		lll.write('\t'+grammar[t][0]+'\n')
		for x in grammar[t][1:]:
			lll.write('\t| '+x+'\n')
		lll.write(';\n\n')
	lll.close()

def massageGrammarRule(context,nt):
	global nt2t
	for i in range(0,len(context[nt])):
		tokens = context[nt][i].split()
		# special case: a postfix metasymbol (e.g., *) occurs in the beggining of the line
		if tokens[0] in knownPostfixes:
			tokens[0] = quote(tokens[0])
		# special case: arithmetic operations versus context metasymbols
		if len(tokens) == 3 and tokens[1] == '*' and tokens[0]+' "/" '+tokens[2] in context[nt]:
			print 'A suspicious metasymbol * converted to an arithmetic operator'
			tokens[1] = '"*"'
		if len(tokens) == 3 and tokens[1] == '+' and tokens[0]+' "-" '+tokens[2] in context[nt]:
			print 'A suspicious metasymbol + converted to an arithmetic operator'
			tokens[1] = '"+"'
		for j in range(0,len(tokens)):
			# putting back the optionality metasymbol
			if tokens[j] == 'OPTIONALITYMETASYMBOL':
				tokens[j] = '?'
				continue
			# NOT converting undefined nonterminals to terminals
			# REPORTING undefined nonterminals
			if tokens[j][0] != '"'\
			and tokens[j] not in grammar.keys()\
			and tokens[j] not in ignored\
			and tokens[j] not in reported:
				ts = splitLeading(tokens[j],[knownTerminals])
				if unquote(ts.split()[-1]) in grammar.keys():
					# false positive in nonterminal -> terminal conversion
					tss = ts.split()
					tss[-1] = unquote(tss[-1])
					ts = ' '.join(tss)
				if ts.find(' ')>-1 and (ts.split()[-1] in grammar.keys() or ts.split()[-1] in ignored):
					print 'L-Splitting',tokens[j],'into',ts
					tokens[j] = ts
				else:
					ts = splitTrailing(tokens[j],knownTerminals)
					if ts.find(' ')>-1 and (ts.split()[0] in grammar.keys() or ts.split()[-1] in ignored):
						print 'T-Splitting',tokens[j],'into',ts
						tokens[j] = ts
					else:
						print 'Warning: nonterminal',tokens[j],'undefined, but used in',nt
						reported.append(tokens[j])
				#if tokens[j] not in knownNonterminals:
				#	tokens[j]='"'+tokens[j]+'"'
				#	nt2t += 1
		context[nt][i] = ' '.join(tokens)
	return

def massageGrammar():
	global nt2t
	# massaging the main grammar
	for nt in grammar.keys():
		massageGrammarRule(grammar,nt)
	# massaging the double rules (for matching purposes)
	for nt in double.keys():
		massageGrammarRule(double,nt)
	if nt2t:
		print 'Warning:',nt2t,'undefined nonterminals were converted to terminals.'
	# matching double rules
	for nt in double.keys():
		if double[nt]!=grammar[nt]:
			print 'Warning: double definition of',nt
			print '\t(1)',double[nt]
			print '\t(2)',grammar[nt]
			for s in double[nt]:
				if s not in grammar[nt]:
					grammar[nt].append(s)
			print 'Opted for the union of them:',grammar[nt]
	# add keywords!!!
	if 'keyword' not in grammar.keys():
		keys.append('keyword')
		grammar['keyword'] = []
		for kw in knownTerminals:
			if kw.isalpha():
				grammar['keyword'].append(quote(kw))
	return

if __name__ == "__main__":
	print 'PDF (rather txt copy-pasted from a PDF) pre-processor: produces an LLL grammar suitable to be fed into an LLL2BGF extractor.'
	if len(sys.argv) == 5:
		readBannedLinesList(sys.argv[3])
		readTerminalsList(sys.argv[4])
		readLines(sys.argv[1])
		readGrammar(lines)
		massageGrammar()
		writeGrammar(sys.argv[2])
		sys.exit(0)
	else:
		print 'Usage:'
		print ' ',sys.argv[0],'''<input-txt> <output-lll> <list-of-banned-lines> <list-of-known-keywords>'''
		sys.exit(1)

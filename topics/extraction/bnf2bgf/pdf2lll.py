#!/usr/bin/python
# -*- coding: utf-8 -*-
import sys

lines = []
grammar = {}
double = {}
current = ''
keys=[]

def assignNewCurrent(c):
	global current
	if c not in keys:
		keys.append(c)
	current = c

forbiddedLines = ('44','45','46',"Annex A","SPECIFICATION","A.2.")

knownNonterminals = ('identifier','literal','right-shift-assignment','right-shift','keyword')

knownTerminalsBefore = \
	(
		'.',',','++','--','-','!','~','/','%','??','?','<<',':','::',
		'[',']','(',')','{','}','<','>',
		'&','^','|','&&','||',
		'+=','-=','*=','/=','%=','&=','|=','^=','<<=','<=','>=','==','!=','='
	)

knownPostfixes = ('+','*','?')

knownTerminalsAfter = \
	(
		';'
	)
	# * +
	
knownReplacements = \
	(
		('opt',' ?'),
		('–','"-"')
	)

screenedTerminals = \
	(
		(';','SEMICOLON'),
		(':','COLON'),
		('**','DOUBLESTAR'),
		('*=','MULTIPLICATIONASSIGNMENT'),
		('*','STAR'),
		('++','DOUBLEPLUS'),
		('+=','ADDITIONASSIGNMENT'),
		('+','PLUS'),
		('?','QUESTION'),
		('(','LEFTPARENTHESIS'),
		(')','RIGHTPARENTHESIS'),
		('{','LEFTCURLYBRACKET'),
		('}','RIGHTCURLYBRACKET')
	)

# these special symbols get transformed into HTML entities
htmlEntities = \
	(
		('&','amp'),
		('<','lt'),
		('>','gt')
	)

fresh = 0
# 0 - the first production
# 1 - the first line of a production
# 2 - more lines in a production
# 3 - the first line in a one-of production
# 4 - more lines in a one-of production
oneof = False

def processline(line):
	global oneof
	global current
	rline = line.strip()
	if rline == '':
		return ''
	if rline[-1]==':' and rline[-2].isalpha():
		oneof = False
		assignNewCurrent(rline[:-1])
		if current in grammar.keys():
			#print 'Warning: double declaration of',current
			double[current] = grammar[current][:]
		grammar[current]=[]
		return
	if rline.find('one of')>0:
		oneof = True
		assignNewCurrent(rline.replace('one of','').strip()[:-1])
		if current in grammar.keys():
			#print 'Warning: double declaration of',current,': the first one',grammar[current],'discarded'
			double[current] = grammar[current][:]
		grammar[current]=[]
		return
	if oneof:
		for t in processLineTokens(rline):
			grammar[current].append(t)
	else:
		grammar[current].append(' '.join(processLineTokens(rline)))
	return

def processLineTokens(rline):
	tokens = rline.split()
	for i in range(0,len(tokens)):
		if tokens[i] in knownTerminalsBefore:
			tokens[i] = '"'+tokens[i]+'"'
	iline = ' '.join(tokens)
	for x,y in knownReplacements:
		iline = iline.replace(x,y)
	tokens = iline.split()
	for i in range(0,len(tokens)):
		if tokens[i] in knownTerminalsAfter:
			tokens[i] = '"'+tokens[i]+'"'
	return tokens

def readLines(f):
	print 'Reading the PDF lines...'
	pdf = open(f,'r')
	cx = 0
	for line in pdf.readlines():
		cx += 1
		include = True
		for x in forbiddedLines:
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

def massageGrammar():
	#print len(keys),'vs',len(grammar.keys())
	nt2t = 0
	for nt in grammar.keys():
		for i in range(0,len(grammar[nt])):
			tokens = grammar[nt][i].split()
			# special case: a postfix metasymbol (e.g., *) occurs in the beggining of the line
			if tokens[0] in knownPostfixes:
				tokens[0] = '"'+tokens[0]+'"'
			# special case: arithmetic operations versus grammar metasymbols
			if len(tokens) == 3 and tokens[1] == '*' and tokens[0]+' "/" '+tokens[2] in grammar[nt]:
				print 'A suspicious metasymbol * converted to an arithmetic operator'
				tokens[1] = '"*"'
			if len(tokens) == 3 and tokens[1] == '+' and tokens[0]+' "-" '+tokens[2] in grammar[nt]:
				print 'A suspicious metasymbol + converted to an arithmetic operator'
				tokens[1] = '"+"'
			# converting undefined nonterminals to terminals
			for j in range(0,len(tokens)):
				if tokens[j][0] != '"'\
				and tokens[j] not in grammar.keys()\
				and tokens[j] not in knownPostfixes:
					#print 'Warning: nonterminal',tokens[j],'undefined!'
					if tokens[j] not in knownNonterminals:
						tokens[j]='"'+tokens[j]+'"'
						nt2t += 1
			grammar[nt][i] = ' '.join(tokens)
	if nt2t:
		print 'Warning:',nt2t,'undefined nonterminals were converted to terminals.'
	for nt in double.keys():
		if double[nt]!=grammar[nt]:
			print 'Warning: double definition of',nt
			print '\t(1)',double[nt]
			print '\t(2)',grammar[nt]
			for s in double[nt]:
				if s not in grammar[nt]:
					grammar[nt].append(s)
			print 'Opted for the union of them:',grammar[nt]
	return

if __name__ == "__main__":
	print 'PDF (rather txt copy-pasted from a PDF) pre-processor: produces an LLL grammar suitable to be fed into an LLL2BGF extractor.'
	if len(sys.argv) == 3:
		readLines(sys.argv[1])
		readGrammar(lines)
		massageGrammar()
		writeGrammar(sys.argv[2])
		sys.exit(0)
	else:
		print 'Usage:'
		print ' ',sys.argv[0],'''<input-txt> <output-lll>'''
		sys.exit(1)

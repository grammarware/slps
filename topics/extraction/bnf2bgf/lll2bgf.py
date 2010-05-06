#!/usr/bin/python
import sys

# for debugging purposes search for 'BREAKPOINT' and insert the name of the interested nonterminal symbol

# these terminals get wrapped with writespace added on both sides
wrappedTerminals = ('(',')','{','}','*','+','?',':',';')

# these terminals are screened by giving them internal names (basically we fold them before processing and unfold afterwards)
screenedTerminals = \
	(
		(';','SEMICOLON'),
		('::','DOUBLECOLON'),
		(':','COLON'),
		('**','DOUBLESTAR'),
		('*=','MULTIPLICATIONASSIGNMENT'),
		('*','STAR'),
		('++','DOUBLEPLUS'),
		('+=','ADDITIONASSIGNMENT'),
		('+','PLUS'),
		('??','DOUBLEQUESTION'),
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

def bgffriendly(s):
	r = s
	for x,y in htmlEntities:
		r = r.replace(x,'&'+y+';')
	return r
	
def preparelll(s):
	r = s
	for x,y in screenedTerminals:
		r = r.replace('"'+x+'"','TERMINAL'+y)
	for x in wrappedTerminals:
		r = r.replace(x,' '+x+' ')
	return r

def serialiseExpression(ts,debug):
	#print ts
	s = []
	i = 0
	while i<len(ts):
		if debug:
			print 'Processing token',ts[i],'(',i,')'
		if ts[i][0] == '"':
			s.append('<bgf:expression><terminal>'+bgffriendly(ts[i][1:-1])+'</terminal></bgf:expression>')
			i += 1
		elif ts[i][0] == '?':
			s[-1]='<bgf:expression><optional>'+s[-1]+'</optional></bgf:expression>'
			i += 1
		elif ts[i][0] == '*':
			s[-1]='<bgf:expression><star>'+s[-1]+'</star></bgf:expression>'
			i += 1
		elif ts[i][0] == '+':
			s[-1]='<bgf:expression><plus>'+s[-1]+'</plus></bgf:expression>'
			i += 1
		elif ts[i][0].isalpha():
			flag = False
			for x,y in screenedTerminals:
				if not flag and ts[i]=='TERMINAL'+y:
					s.append('<bgf:expression><terminal>'+x+'</terminal></bgf:expression>')
					flag = True
			if not flag:
				s.append('<bgf:expression><nonterminal>'+ts[i]+'</nonterminal></bgf:expression>')
			i += 1
		elif ts[i] == '{':
			if i+4>len(ts) or ts[i+3]!='}' or ts[i+4] not in ['+','*']:
				print 'Error: incomplete separator sequence detected, processed as a terminal symbol!'
				s.append('<bgf:expression><terminal>{</terminal></bgf:expression>')
				i += 1
				continue
			symbol    = serialiseExpression([ts[i+1]],debug)
			separator = serialiseExpression([ts[i+2]],debug)
			if ts[i+4] == '+':
				s.append('<bgf:expression><sequence>'+symbol+'<bgf:expression><star><bgf:expression><sequence>'+separator+symbol+
						'</sequence></bgf:expression></star></bgf:expression></sequence></bgf:expression>')
			elif ts[i+4] == '*':
				s.append('<bgf:expression><optional><bgf:expression><sequence>'+symbol+'<bgf:expression><star><bgf:expression><sequence>'+
						separator+symbol+'</sequence></bgf:expression></star></bgf:expression></sequence></bgf:expression></optional></bgf:expression>')
			i += 5
		elif ts[i] == '(':
			rbr = i+ts[i:].index(')')
			if debug:
				print 'Brackets found: from',i,'to',rbr,'in',ts
			s.append('\t'+serialiseExpression(ts[i+1:rbr],debug))
			i = rbr+1
		else:
			s.append('<unknown>'+ts[i]+'</unknown>')
			i += 1
	# return result
	if debug:
		print s
	if len(s)==1:
		return '\t'+s[0]
	exprs = ''.join(s)
	if exprs.find('<unknown>|</unknown>')<0:
		# sequence
		return '<bgf:expression><sequence>'+exprs+'</sequence></bgf:expression>'
	else:
		# choice
		choices = exprs.split('<unknown>|</unknown>')
		for i in range(0,len(choices)):
			if choices[i].count('<bgf:expression>')>1:
				# remember about possibility of choices of sequences!
				choices[i] = '<bgf:expression><sequence>'+choices[i]+'</sequence></bgf:expression>'
		return '<bgf:expression><choice>'+''.join(choices)+'</choice></bgf:expression>'
	print 'Warning: default output'
	return '\n\t'.join(s)

def serialiseFormula(name,tokens):
	# Useful yet annoying
	#print 'Processing',name,'...'
	if name=='BREAKPOINT':
		print tokens
		return '<bgf:production><nonterminal>'+name+'</nonterminal>'+serialiseExpression(tokens,True)+'</bgf:production>'
	return '<bgf:production><nonterminal>'+name+'</nonterminal>'+serialiseExpression(tokens,False)+'</bgf:production>'

def lll2lines(f1,f2):
	lll = open(f1,'r')
	bgf = open(f2,'w')
	bulk = preparelll(' '.join(filter(lambda x:x[0]!='#',lll.readlines()))).split()
	bgf.write('<bgf:grammar xmlns:bgf="http://planet-sl.org/bgf">')
	while bulk!=[] and bulk.index(';'):
		if bulk[1] != ':':
			print 'error'
			print bulk
			break
		bgf.write(serialiseFormula(bulk[0],bulk[2:bulk.index(';')]))
		bulk = bulk[(bulk.index(';')+1):]
	bgf.write('</bgf:grammar>')
	lll.close()
	bgf.close()
	return

if __name__ == "__main__":
 print 'LLL to BNF-like Grammar Format automated extractor'
 if len(sys.argv) == 3:
  print 'Reading the LLL...'
  lll2lines(sys.argv[1],sys.argv[2])
  sys.exit(0)
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'''<input> <output>'''
  sys.exit(1)

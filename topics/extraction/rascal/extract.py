#!/usr/local/bin/python
import os
import sys
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import slpsns
import BGF
import elementtree.ElementTree as ET

names   = []
we_want_sl = False

def parseGroup(g):
	# ['(',a,'|',b,'|',c,'|',d')']
	if g[0] != '(' or g[-1] != ')':
		print 'Unexprected group:',g
		return []
	gram = [[]]
	for e in g[1:-1]:
		if e == '|':
			grqm.append([])
		else:
			gram[-1].append(e)
	return

if __name__ == "__main__":
	if len(sys.argv) not in [3,4]:
		print 'This tool extracts a Rascal grammar.'
		print 'Usage:'
		print '      rsc2bgf <rsc-input> <bgf-output> SL?'
		sys.exit(1)
	rsc = open(sys.argv[1],'r')
	if len(sys.argv) == 4 and sys.argv[3] == 'SL':
		we_want_sl = True
	start = []
	grammar = {}	# in tokens
	nt = ''
	cx = 0
	lines = rsc.readlines()
	discardBracket = 0
	grammarkeys = []
	lexicals = []
	layouts = []
	endOfAlt = False
	while cx < len(lines):
		line = lines[cx].strip()
		cx += 1
		if line == '':
			continue
		if line[0] == '@':
			continue
		elif line[0] == '=' or line[0] == '|' or line[0] == '>':
			# "all", treat as alternatives
			# "first", treat as an alternative for now
			if nt == '':
				print '['+str(cx)+']','Found first line, but of what nonterminal?'
				continue
			tokens = line[1:].split()
			if len(tokens) == 0:
				continue
			endOfAlt = True
			if len(tokens[-1])>0 and tokens[-1][-1] == ';':
				tokens[-1] = tokens[-1][:-1]
				tokens.append(';')
			for i in range(0,len(tokens)):
				if tokens[i].find('[')>-1 and tokens[i].find(']')>-1:
					# treating a parametrised nonterminal as a base nonterminal
					tokens[i] = tokens[i][:tokens[i].index('[')] + tokens[i][tokens[i].index(']')+1:]
			#grammar[nt].append(tokens)
			#continue
			# fall down
		elif line[0] == '#':
			# "follow"
			continue
		elif line[0] == '-':
			# "reject"
			continue
		elif line[0:2] == '//':
			# comment
			continue
		else:
			tokens = line.split()
		# main branching down, fall down point here
		if len(tokens) == 0:
			continue
		if tokens[0] == 'module':
			print '['+str(cx)+']','Parsing module',tokens[1]
			# do something with the module name!
			continue
		if tokens[0] == 'import':
			print '['+str(cx)+']','Ignoring import',tokens[1:]
			# do something with imports?
			continue
		if tokens[0] == 'start':
			print 'Assumed',tokens[2],'to be a root.'
			start.append(tokens[2])
			tokens = tokens[1:]
			# fall through
		if tokens[0] in ('syntax','layout','lexical'):
			nt = tokens[1]
			if nt[0] == '"':
				print '['+str(cx)+']','Cannot include lexical restriction information about',nt
				nt = ''
				while cx < len(lines) and lines[cx].strip() != ';':
					cx += 1
				continue
			if nt.find('[')>-1 and nt.find(']')>-1:
				# treating a parametrised nonterminal as a base nonterminal
				nt = nt[:nt.index('[')] + nt[nt.index(']')+1:]
			print '['+str(cx)+']','Starting to treat nonterminal',nt
			if nt in grammarkeys:
				print '['+str(cx)+']','Duplicate or partial definition of nonterminal',nt
				endOfAlt = True
			else:
				grammar[nt] = []
				grammarkeys.append(nt)
				if tokens[0] == 'lexical':
					lexicals.append(nt)
				elif tokens[0]=='layout':
					layouts.append(nt)
			# in case there are more tokens on the same line, we proceed
			if len(tokens) > 3:
				tokens = tokens[3:]
			else:
				continue
		if len(tokens) == 1 and tokens[0] == ';':
			print "Done with nonterminal",nt
			continue
		while len(tokens) > 0 and tokens[0] in ('left','right','non-assoc'):
			tokens = tokens[1:]
			print 'Skipped a modifier',tokens[0],'at',nt
		if len(tokens) > 0:
			if tokens[0] == '(':
				discardBracket += 1
				tokens = tokens [1:]
			elif tokens[0] == ')' and discardBracket > 0:
				discardBracket -= 1
				tokens = tokens [1:]
		if len(tokens) == 0:
			continue
		# give up
		if nt:
			print '['+str(cx)+']','Treating',tokens,'as a part of',nt
		else:
			print '['+str(cx)+']','Disregarding',tokens,'completely.'
		if nt:
			if endOfAlt:
				grammar[nt].append(tokens)
				endOfAlt = False
			elif len(grammar[nt]) > 0:
				grammar[nt][-1].extend(tokens)
			else:
				grammar[nt].append(tokens)
		pass
		if tokens[-1][-1]==';':
			if nt:
				print '['+str(cx)+']','Done with nonterminal',nt
			nt = ''
	# NOW TO PROCESS TOKENS
	#print 'Command:'
	#for s in grammar['Command']:
	#	print '	',s
	bgf = BGF.Grammar()
	bgf.roots = start
	prevline = []
	curly = 0
	# going through the sorted list of nonterminals
	print grammarkeys
	for nt in grammarkeys:
		print nt,'::=',grammar[nt]
	for nt in grammarkeys:
		if nt in lexicals:
			print 'Lexical nonterminal',nt,'is disregarded'
			continue
		if nt in layouts:
			print 'Layout-related nonterminal',nt,'is disregarded'
			continue
		for alt in grammar[nt]:
			while alt[0][0]=='@':
				print 'Disregarding annotation',alt[0]
				alt = alt[1:]
			if prevline:
				# dead code yet
				prevline.append('|')
				prevline.extend(alt)
				alt = prevline[:]
				prevline = []
			prod = BGF.Production()
			prod.setNT(nt)
			while alt and alt[0] in ('bracket','left','right','non-assoc','lex','(',')'):
				print 'Skipped a modifier',alt[0],'at',nt
				if alt[0] == 'lex':
					alt = []
				else:
					alt = alt[1:]
				print 'left:',alt
			if not alt:
				continue
			while len(alt)>0 and alt[-1] in (';',''):
				alt = alt[:-1]
			while len(alt)>0 and alt[0] == '':
				alt = alt[1:]
			#print '---',alt
			if not alt:
				continue
			if alt[0][-1] == ':':
				prod.setLabel(alt[0][:-1])
				alt = alt[1:]
			if len(alt)>1 and alt[1] == ':':
				# if there is whitespace between the label and the :
				prod.setLabel(alt[0])
				alt = alt[2:]
			if alt and len(alt)>1 and alt[0][:2] == '/*':
				if alt[0].find('*/')>0:
					alt[0] = alt[0][:alt[0].index('/*')] + alt[0][alt[0].index('*/')+2:]
					if alt[0] == '':
						alt = alt[1:]
			# had to do it the second time - bad sign
			if alt and len(alt)>0 and alt[0][-1] == ':':
				prod.setLabel(alt[0][:-1])
				alt = alt[1:]
			cx = 0
			seq = BGF.Sequence()
			sym = None
			#print '['+str(cx)+']',alt
			while cx<len(alt):
				if alt[cx][-1] == ';':
					alt[cx] = alt[cx][:-1]
				print '['+str(cx)+']','@',alt
				if curly>0:
					if alt[cx] == '{':
						curly += 1
						#print 'MORE CURLY'
					elif alt[cx] == '}':
						curly -= 1
						#print 'LESS CURLY'
					#print 'Skipped over',alt[cx]
					cx += 1
					continue
				# comments
				if alt[cx][:2] == '/*':
					if alt[cx].find('*/')>0:
						alt[cx] = alt[cx][:alt[cx].index('/*')] + alt[cx][alt[cx].index('*/')+2:]
						if alt[cx] == '':
							cx += 1
							continue
					else:
						print '['+str(cx)+']','TODO: a comment spanning several tokens'
				# groups - TODO
				if alt[cx] == ')':
					cx += 1
					continue
				#if alt[cx] == '(':
				#	if alt.__contains__(')'):
				#		# the end is near
				#		parseGroup(alt[cx:alt.index(')')+1])
				#		# ???
				#	else:
				#		prevline = alt[cx:]
				#		cx = len(alt)+10
				#		break
				# nonterminals
				if alt[cx] == '{':
					curly += 1
					cx += 1
					continue
				if alt[cx][0].isupper() or alt[cx][0]=='&':
					if sym:
						seq.add(sym)
					if alt[cx][-1] == '+':
						sym = BGF.Plus()
						sym.setExpr(BGF.Nonterminal())
						sym.data.setName(alt[cx][:-1])
					elif alt[cx][-1] == '*':
						sym = BGF.Star()
						sym.setExpr(BGF.Nonterminal())
						sym.data.setName(alt[cx][:-1])
					elif alt[cx][-1] == '?':
						sym = BGF.Optional()
						sym.setExpr(BGF.Nonterminal())
						sym.data.setName(alt[cx][:-1])
					elif alt[cx] in layouts:
						print 'Occurrence of layout nonterminal',alt[cx],'is disregarded.'
						sym = BGF.Epsilon()
					else:
						sym = BGF.Nonterminal()
						sym.setName(alt[cx])
					cx += 1
					continue
				if alt[cx] == 'lex':
					cx += 1
					continue
				if alt[cx][0].islower():
					typ = sym
					sym = BGF.Selectable()
					sym.setExpr(typ)
					sym.setName(alt[cx])
					cx += 1
					continue
				if alt[cx][0] == '"':
					if sym:
						seq.add(sym)
					sym = BGF.Terminal()
					term = alt[cx][1:-1].replace('\\\\','\\').replace('\\>','>').replace('\\<','<').replace('\\\'','\'').replace('\\"','"')
					sym.setName(term)
					cx +=1
					print 'Found a terminal',term
					continue
				if alt[cx][0] == '[' or alt[cx][:2] == '![':
					# not quite correct
					#print alt
					if sym:
						seq.add(sym)
					if alt[cx][-1] == ']':
						sym = BGF.Terminal()
						sym.setName(alt[cx][1:-1])
					elif alt[cx][-2] == ']' and alt[cx][-1] == '+':
						sym = BGF.Plus()
						part = BGF.Terminal()
						part.setName(alt[cx][1:-2])
						sym.setExpr(part)
					elif alt[cx][-2] == ']' and alt[cx][-1] == '*':
						sym = BGF.Star()
						part = BGF.Terminal()
						part.setName(alt[cx][1:-2])
						sym.setExpr(part)
					elif alt[cx][-2] == ']' and alt[cx][-1] == '?':
						sym = BGF.Optional()
						part = BGF.Terminal()
						part.setName(alt[cx][1:-2])
						sym.setExpr(part)
					else:
						alt[cx+1] = alt[cx] + ' ' + alt[cx+1]
						sym = None
					cx +=1
					continue
				if alt[cx][0] == '{':
					if sym:
						seq.add(sym)
					# change the bool below if you want no separator lists in your output BGF
					if we_want_sl:
						if alt[cx+1][-1] == '+':
							sym = BGF.SepListPlus()
						else:
							sym = BGF.SepListStar()
						sym.item = BGF.Nonterminal()
						sym.item.setName(alt[cx][1:])
						sym.sep = BGF.Terminal()
						sym.sep.setName(alt[cx+1][1:-3])
					else:
						# a hack (OTF refactoring)
						base = BGF.Nonterminal()
						base.setName(alt[cx][1:])
						sep = BGF.Terminal()
						sep.setName(alt[cx+1][1:-3])
						plus = BGF.Sequence()
						plus.add(base)
						part = BGF.Sequence()
						part.add(sep)
						part.add(base)
						star = BGF.Star()
						star.setExpr(part)
						plus.add(star)
						if alt[cx+1][-1] == '+':
							sym = plus
						else:
							sym = BGF.Optional()
							sym.setExpr(plus)
					cx +=2
					continue
			if cx == len(alt)+10:
				continue
			if sym:
				#print 'Adding',sym,'to',seq
				seq.add(sym)
				prod.setExpr(seq)
				bgf.addProd(prod)
			elif curly == 0 and (len(seq.data)>0 or prod.label != ''):
				#print 'Adding2',seq
				prod.setExpr(seq)
				#print str(prod)
				bgf.addProd(prod)
	#for p in bgf.prods:
	#	print str(p)
	ET.ElementTree(bgf.getXml()).write(sys.argv[2])
	sys.exit(0)

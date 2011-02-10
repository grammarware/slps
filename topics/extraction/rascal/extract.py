#!/usr/local/bin/python
import os
import sys
sys.path.append('../../../shared/python')
import slpsns
import BGF
import elementtree.ElementTree as ET

names   = []

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
	if len(sys.argv) != 3:
		print 'This tool extracts a Rascal grammar.'
		print 'Usage:'
		print '      rsc2bgf <rsc-input> <bgf-output>'
		sys.exit(1)
	rsc = open(sys.argv[1],'r')
	start = []
	grammar = {}	# in tokens
	nt = ''
	cx = 0
	for line in rsc.readlines():
		line = line.strip()
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
			if len(tokens[-1])>1 and tokens[-1][-1] == ';':
				tokens[-1] = tokens[-1][:-1]
				tokens.append(';')
			for i in range(0,len(tokens)):
				if tokens[i].find('[')>-1 and tokens[i].find(']')>-1:
					# treating a parametrised nonterminal as a base nonterminal
					tokens[i] = tokens[i][:tokens[i].index('[')] + tokens[i][tokens[i].index(']')+1:]
			grammar[nt].append(tokens)
			continue
		elif line[0] == '#':
			# "follow"
			pass
		elif line[0] == '-':
			# "reject"
			pass
		elif line[0:2] == '//':
			# comment
			continue
		else:
			tokens = line.split()
			if tokens[0] == 'module':
				print '['+str(cx)+']','Parsing module',tokens[1]
				# do something with the module name!
				continue
			if tokens[0] == 'start':
				start.append(tokens[-1])
				tokens = tokens[1:]
				# fall through
			if tokens[0] in ('syntax','layout'):
				nt = tokens[1]
				if nt[0] == '"':
					print '['+str(cx)+']','Cannot include lexical restriction information about',nt
					nt = ''
					continue
				if nt.find('[')>-1 and nt.find(']')>-1:
					# treating a parametrised nonterminal as a base nonterminal
					nt = nt[:nt.index('[')] + nt[nt.index(']')+1:]
				print '['+str(cx)+']','Starting to treat nonterminal',nt
				grammar[nt] = []
				continue
			if tokens[0] == ';' and len(tokens) == 1:
				continue
			# give up
			print '['+str(cx)+']','What is',line.split(),'?'
			if nt:
				grammar[nt].append(tokens)
			pass
	# NOW TO PROCESS TOKENS
	#print 'Command:'
	#for s in grammar['Command']:
	#	print '	',s
	bgf = BGF.Grammar()
	prevline = []
	curly = 0
	for nt in grammar.keys():
		for alt in grammar[nt]:
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
				#print '['+str(cx)+']',alt
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
					# a hack (OTF refactoring)
					if sym:
						seq.add(sym)
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
	#print str(bgf)
	ET.ElementTree(bgf.getXml()).write(sys.argv[2])
	sys.exit(0)

#!/usr/local/bin/python
import os
import sys
sys.path.append('../../../shared/python')
import slpsns
import BGF
import elementtree.ElementTree as ET

names   = []

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
		elif line[0] == '=' or line[0] == '|':
			# "all", treat as alternatives
			if nt == '':
				print '['+str(cx)+']','Found first line, but of what nonterminal?'
				continue
			tokens = line[1:].split()
			grammar[nt].append(tokens)
			continue
		elif line[0] == '#':
			# "follow"
			pass
		elif line[0] == '-':
			# "reject"
			pass
		elif line[0] == '>':
			# "first"
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
			if tokens[0] == 'syntax':
				nt = tokens[1]
				if nt[0] == '"':
					print '['+str(cx)+']','Cannot include lexical restriction information about',nt
					nt = ''
					continue
				print '['+str(cx)+']','Starting to treat nonterminal',nt
				grammar[nt] = []
				continue
			if tokens[0] == ';' and len(tokens) == 1:
				continue
			# give up
			print '['+str(cx)+']','What is',line.split(),'?'
			pass
	# NOW TO PROCESS TOKENS
	#print 'Literal:'
	#for s in grammar['Literal']:
	#	print '	',s
	bgf = BGF.Grammar()
	for nt in grammar.keys():
		for alt in grammar[nt]:
			prod = BGF.Production()
			prod.setNT(nt)
			if alt[-1] == ';':
				alt = alt[:-1]
			if alt[0][-1] == ':':
				prod.setLabel(alt[0][:-1])
				alt = alt[1:]
			cx = 0
			seq = BGF.Sequence()
			sym = None
			print '['+str(cx)+']',alt
			while cx<len(alt):
				if alt[cx][0].isupper():
					if sym:
						seq.add(sym)
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
					term = alt[cx][1:-1].replace('\\\\','\\').replace('\\>','>').replace('\\\'','\'').replace('\\"','"')
					sym.setName(term)
					cx +=1
					continue
				if alt[cx][0] == '[':
					# not quite correct
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
			if sym:
				seq.add(sym)
			prod.setExpr(seq)
			bgf.addProd(prod)
	ET.ElementTree(bgf.getXml()).write(sys.argv[2])
	sys.exit(0)
		
		
	slpsns.init(ET)
	bgf = ET.parse(sys.argv[1])
	for prod in bgf.findall('//'+slpsns.bgf_('production')):
		nt = prod.findtext('nonterminal')
		if nt in grammar.keys():
			grammar[nt].append(prod)
		else:
			grammar[nt]=[prod]
	newBgf = ET.Element(slpsns.bgf_('grammar'))
	nts = [sys.argv[2]]
	ET.SubElement(newBgf,'root').text = nts[0]
	oldnts = []
	while nts:
		for prod in grammar[nts[0]]:
			newBgf.append(prod)
		for nt in prod.findall('.//nonterminal'):
			if (nt.text not in oldnts) and (nt.text not in nts):
				nts.append(nt.text)
		oldnts.append(nts[0])	# car
		nts = nts[1:]			# cdr
	ET.ElementTree(newBgf).write(sys.argv[3])
	sys.exit(0)

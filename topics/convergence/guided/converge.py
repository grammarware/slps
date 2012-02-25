#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
from functools import reduce
import os,sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF3
import XBGF3
import CBGF3
import slpsns
import metrics3

# globals
bgfs = {}
cbgfs = {}
dicts = {}
master = BGF3.Grammar()
# current version dictionary
cvdict = {}

def bind(key,nt1,nt2):
	global dicts
	if nt1:
		if nt1 in dicts[key]:
			if nt2 in dicts[key][nt1].split('+'):
				print('~~~Confirmed binding',nt1,'to',nt2,'in',key)
			else:
				print('~~~Contradicting binding',nt1,'to',dicts[key][nt1],'or',nt2,'in',key)
				dicts[key][nt1] += '+'+nt2
				#print('■')
				#sys.exit(1)
		else:
			dicts[key][nt1] = nt2
			if nt1 != nt2:
				renameNin(nt2,nt1,key)
	else:
		dicts[key][None].append(nt2)

def makeSignature(d,ss):
	# Input: sequence
	sign = {}
	if ss.who() == 'Sequence':
		for x in ss.data:
			if x.wrapped.who() == 'Nonterminal':
				nt = x.wrapped.data
				q = '1'
			elif x.wrapped.who() == 'Star':
				nt = x.wrapped.data.wrapped.data
				q = '*'
			elif x.wrapped.who() == 'Plus':
				nt = x.wrapped.data.wrapped.data
				q = '+'
			elif x.wrapped.who() == 'Optional':
				nt = x.wrapped.data.wrapped.data
				q = '?'
			elif x.wrapped.who() == 'Value':
				nt = str(x.wrapped.data)
				q = '1'
			else:
				print('Unknown case:',x)
				return None
			if nt.__class__.__name__ == 'list':
				print('ERROR: A list is found where one value is expected!')
				nt = ' '.join(map(str,nt))
			if nt == d:
				# we assume q could not be anything but 1 before
				if q == '1':
					q = '0'
				elif q == '+':
					q = '⊕'
				elif q == '*':
					q = '⊛'
				else:
					print('ERROR: Please rewrite the code of makeSignature!')
			if nt in sign:
				sign[nt].append(q)
			else:
				sign[nt] = [q]
	elif ss.who() in ('Nonterminal','Value'):
		if ss.data == d:
			return [('0',ss.data)]
		else:
			return [('1',ss.data)]
	elif ss.who() == 'Expression':
		return makeSignature(d,ss.wrapped)
	elif ss.who() == 'Plus':
		return [('+',str(ss.data.wrapped))]
	else:
		print('!!! Signatures not implemented for',ss.who())
	return [(joinsort('',sign[k]),k) for k in sign]

def findOneMatch(x,ysign):
	return (x[0],x[1],list(map(lambda a:a[1],filter(lambda a:a[0]==x[0],ysign))))

def fst(xs):
	return list(map(lambda x:x[0],xs))

def snd(xs):
	return list(map(lambda x:x[1],xs))

def appnd(xs,ys):
	zs = []
	for x in xs:
		if x not in zs:
			zs.append(x)
	for y in ys:
		if y not in zs:
			zs.append(y)
	return zs

def fetch(xs,k):
	l = list(filter(lambda a:a[0]==k,xs))
	return list(map(lambda a:a[1],l))

def cartesian(ss):
	# [(a,b)] ⇒ [(a,b)]
	# [(a,b),(x),(1,2)] ⇒ [(a,x,1),(a,x,2),(b,x,1),(b,x,2)]
	res = []
	if len(ss)==1:
		return ss
	for x in ss[0]:
		for y in ss[1]:
			res.append([x,y])
	for z in ss[2:]:
		res2 = []
		for x in res:
			for y in z:
				t = x[:]
				t.append(y)
				res2.append(t)
		res = res2
	return res

def setmin(bs,cs):
	xs = bs[:]
	for c in cs:
		if c in xs:
			xs.remove(c)
	return xs

def getSign(xsign,x):
	return list(map(lambda a:a[0],filter(lambda a:a[1]==x,xsign)))[0]

def projectSymbols(xs,ss):
	if xs.who() == 'Sequence':
		e = BGF3.Sequence()
		for x in xs.data:
			# TODO: now works only for nonterminals
			if x.wrapped.who() == 'Nonterminal' and x.wrapped.data in ss:
				m = BGF3.Marked()
				m.setExpr(BGF3.Expression(x))
				e.add(m)
			else:
				e.add(x)
		return e
	else:
		print('projectSymbols not implemented for',xs.who())
		print('■')
		sys.exit(1)

def match(ident,key,xnt,xs,ynt,ys):
	global cvdict
	# Input: sequences
	xsign = makeSignature(xnt,xs)
	ysign = makeSignature(ynt,ys)
	#print('    ~>',xsign)
	#print('    ~>',ysign)
	matches = {}
	alltriplets = []
	for k in appnd(fst(xsign),fst(ysign)):
		triplets = []
		if k in fst(xsign):
			xk = fetch(xsign,k)
		else:
			xk = [None]
		if k in fst(ysign):
			yk = fetch(ysign,k)
		else:
			yk = [None]
		for x in xk:
			for y in yk:
				triplets.append((k,x,y))
		alltriplets.append(triplets)
	alltriplets = cartesian(alltriplets)
	# some sort of verification/robustness check:
	# making sure each version touches all nonterminals of both sides
	xnts = snd(xsign)
	ynts = snd(ysign)
	for version in alltriplets:
		xntsl = xnts[:]
		yntsl = ynts[:]
		for triplet in version:
			if triplet[1] in xntsl:
				xntsl.remove(triplet[1])
			if triplet[2] in yntsl:
				yntsl.remove(triplet[2])
		for x in xntsl:
			version.append((getSign(xsign,x),x,None))
		for y in yntsl:
			version.append((getSign(ysign,y),None,y))
	#
	if len(alltriplets) == 1:
		# only one version to assume
		print(ident,'√ Only one version:')
		version = alltriplets[0]
		cvdict = {m[1]:m[2] for m in version}
		unmatched = list(map(lambda a:a[2],filter(lambda a:a[1]==None,version)))
		disregard = appnd(setmin(snd(xsign),snd(version)),list(map(lambda a:a[1],filter(lambda a:a[2]==None,version))))
		#print('      ~version~>',version)
		for un in unmatched:
			#print('     ! Unmatched',un)
			# TODO: now works with only one candidate (cannot make more version out of one)
			for dis in disregard:
				sign_un  = list(filter(lambda a:a[1]==un,ysign))[0]
				sign_dis = list(filter(lambda a:a[1]==dis,xsign))[0]
				#print('sign_un=',sign_un,'sign_dis=',sign_dis)
				if moreLiberalSign(sign_dis[0],sign_un[0]):
					print(ident,'☯ Disregarding more liberal signature,')
					ver2 = []
					for m in version:
						if m[2]==un:
							ver2.append((sign_un[0] + ' <: ' + sign_dis[0],dis,un))
						elif m[1]==dis:
							#version.remove(m)
							pass
						else:
							ver2.append(m)
					version = ver2
					disregard = appnd(setmin(snd(xsign),snd(version)),list(map(lambda a:a[1],filter(lambda a:a[2]==None,version))))
		for dis in disregard:
			print(ident,'☯ Disregarding',dis+',')
		#print('DICTS:',dicts[key])
		for match in version:
			if match[2]:
				print(ident,' ⇒ in',key+':',match[2],'maps to',match[1],'with signature',match[0])
				bind(key,match[2],match[1])
				#renameNin(match[1],match[2],key)
			else:
				bind(key,None,match[1])
		#print('DICTS:',dicts[key])
		cvdict = {}
	else:
		print(ident,'⇒ Multiple mapping versions:')
		cx = 0
		for version in alltriplets:
			cvdict = {m[1]:m[2] for m in version}
			cx +=1
			print(ident,'? Version',cx,':',joinsort(', ',[str(t[2])+' is '+str(t[1]) for t in version]))
			unmatched = list(map(lambda a:a[2],filter(lambda a:a[1]==None,version)))
			disregard = appnd(setmin(snd(xsign),snd(version)),list(map(lambda a:a[1],filter(lambda a:a[2]==None,version))))
			disq = False
			#print(ident,' ~version~>',version)
			for match in version:
				good = checkCandidates(ident+' ',key,match[2],[match[1]],master.getProdsOfN(match[2]))
				if len(good)!=1:
					print(ident,'  ✗ stop checking')
					disq = True
					break
			if disq:
				#print('unmatch=',unmatched,'disregarded=',disregard)
				# TODO: should be any combination
				cs1 = cs2 = ''
				for y in unmatched:
					cs1 += getSign(ysign,y)
				for x in disregard:
					cs2 += getSign(xsign,x)
				# extra normalisation
				cs1 = joinsort('',list(cs1))
				cs2 = joinsort('',list(cs2))
				if cs1 == cs2:
					ver2 = [(cs1,'+'.join(disregard),'+'.join(unmatched))]
					for m in version:
						if m[2] not in unmatched and m[1] not in disregard:
							ver2.append(m)
					alltriplets.append(ver2)
					print(ident,' ✗ version disqualified, an adapted variant is proposed')
				else:
					print(ident,' ✗ version disqualified')
			else:
				print(ident,' √ version approved')
				#print('VERSION:',version)
				disregardednow = setmin(disregard,'+'.join(list(map(lambda x:x[1],filter(lambda x:x[1] and x[1].find('+')>-1,version)))).split('+'))
				for dis in disregardednow:
					print(ident,'☯ Disregarding',dis+',')
				#print('DICTS:',dicts[key])
				nxs = projectSymbols(xs,disregardednow)
				step = CBGF3.Step('project-inject')
				p = BGF3.Production()
				p.nt = nt
				p.expr = nxs
				step.addParam(p)
				cbgfs[key].addStep(step)
				for match in version:
					if match[2]:
						if match[1].find('+')>-1:
							print(ident,'  ⇒',match[2],'maps to',match[1].replace('+',' and '),'with signature',match[0])
							bind(key,match[2],match[1])#.split('+')
							#renameNin(match[1],match[2],key)
						else:
							print(ident,'  ⇒',match[2],'maps to',match[1],'with signature',match[0])
							bind(key,match[2],match[1])
							#renameNin(match[1],match[2],key)
					else:
						bind(key,None,match[1])
				#print('DICTS:',dicts[key])
			cvdict = {}

def ppseplist(s,z):
	l = list(z)
	if len(l) == 0:
		return ''
	elif len(l) == 1:
		return l[0]
	else:
		return (s+' ').join(l)

def sameThing(xkey,x,ykey,y):
	global master,bgfs,cvdict
	# x and y are the same thing
	if x.wrapped.who() == y.wrapped.who():
		if y.wrapped.who() in ('Plus','Star','Optional'):
			# we don't check for contents for now (TODO)
			return True
		elif y.wrapped.who() in ('Any','Empty','Epsilon'):
			# we never check for contents
			return True
		elif x.wrapped.who() == 'Nonterminal':
			# only if nonterminal names are equal
			if x.wrapped.data in cvdict.keys():
				return cvdict[x.wrapped.data] == y.wrapped.data
			else:
				return x.wrapped.data == y.wrapped.data
		elif x.wrapped.who() == 'Value':
			# only if values are equal
			return x.wrapped.data == y.wrapped.data
		elif x.wrapped.who() == 'Sequence' and len(x.wrapped.data) == len(y.wrapped.data):
			res = True
			for i in range(0,len(x.wrapped.data)):
				res &= sameThing(xkey,x.wrapped.data[i],ykey,y.wrapped.data[i])
			return res
	elif x.wrapped.who() == 'Nonterminal':
		# unfold x
		if xkey:
			prods = bgfs[xkey].getProdsOfN(x.wrapped.data)
		else:
			# impossible in the current version, but programmed for extra robustness
			prods = master.getProdsOfN(x.wrapped.data)
		if prods:
			#print('...unfolding',x.wrapped.data,'of',xkey,'to',ppseplist(';',map(lambda a:str(a.expr),prods)))
			# TODO: work with multiple rules when unfolding
			return sameThing(xkey,prods[0].expr,ykey,y)
	elif y.wrapped.who() == 'Nonterminal':
		# unfold y
		# since the Y-part always comes from the master grammar, this should be dead code
		if ykey:
			prods = bgfs[ykey].getProdsOfN(y.wrapped.data)
		else:
			prods = master.getProdsOfN(y.wrapped.data)
		if prods:
			print('...unfolding',y.wrapped.data,'of',ykey,'to',ppseplist(';',map(lambda a:str(a.expr),prods)))
			# TODO: work with multiple rules when unfolding
			return sameThing(xkey,x,ykey,prods[0].expr)
	return False

def moreLiberalSign(x,y):
	# x is more liberal than y
	# very hacky: only covers certain cases, but at least robust (will be no false positives)
	if y.replace('+','*')==x:
		return True
	if joinsort('/',map(lambda a:a.replace('⊕','⊛'),x.split('/'))) == joinsort('/',map(lambda a:a.replace('⊕','⊛'),y.split('/'))):
		return True
	return False

def moreLiberal(x,y):
	# x is more liberal than y
	xc = x.expr.wrapped.who()
	yc = y.expr.wrapped.who()
	if xc == 'Star' and yc in ['Plus','Optional']:
		return True
	# TODO: other cases
	return False

def checkCandidates(indent,key,nt,candidates,masterprods):
	good = []
	for c in candidates:
		myprods = bgf.getProdsOfN(c)
		if myprods:
			print(indent,'√',c,'is defined as',ppseplist(';',map(lambda x:str(x.expr),myprods)))
		elif c in ['string','int']:
			# value => compose a fake rule that "defines" value as value
			# the condition above is hacky (what if there is a nonterminal called "string" or "int"?)
			print(indent,'√',c,'is a built-in')
			p = BGF3.Production()
			p.setNT(c)
			v = BGF3.Value()
			v.data = c
			p.setExpr(BGF3.Expression(v))
		else:
			print(indent,'√',c,'is undefined')
			if masterprods:
				print(indent,' ⇒ not a good candidate because its master counterpart is defined')
				continue
			else:
				print(indent,' ⇒ good candidate because both are undefined')
				good.append(c)
				continue
		#print('###my    prods:',ppseplist(';',map(lambda x:str(x.expr),myprods)))
		#print('###masterprods:',ppseplist(';',map(lambda x:str(x.expr),masterprods)))
		if masterprods:
			if len(myprods)==1 and len(masterprods)==1:
				if sameThing(key,myprods[0].expr,None,masterprods[0].expr):
					print(indent,' ⇒ good candidate because both definitions are identical')
					good.append(c)
				elif moreLiberal(myprods[0],masterprods[0]):
					print(indent,' ⇒ good candidate because it is defined more liberally')
					good.append(c)
				else:
					print(indent,' ⇒ not a good candidate because two definitions do not match')
			else:
				cx1 = cx2 = 0
				for p in masterprods:
					for q in myprods:
						if sameThing(key,q.expr,None,p.expr):
							cx1 += 1
						elif moreLiberal(q,p):
							cx2 += 1
				if cx1 and cx2:
					print(indent,' ⇒ good candidate because at least',cx1,'rules are identical to and at least',cx2,' rules are more liberal than the master\'s')
					good.append(c)
				elif cx1:
					print(indent,' ⇒ good candidate because at least',cx1,'rules are identical to the master\'s')
					good.append(c)
				elif cx2:
					print(indent,' ⇒ good candidate because at least',cx2,' rules are more liberal than the master\'s')
					good.append(c)
				else:
					print(indent,' ⇒ not a good candidate because multiple unrelated rules are found')
		elif not nt and len(myprods) == 1 and myprods[0].expr.wrapped.who() == 'Value':
			# TODO: check this code
			print(indent,' ⇒ good candidate because',c,'is defined as a built-in')
			good.append(c)
		else:
			print(indent,' ⇒ not a good candidate because',nt,'is undefined')
	return good

def joinsort(sep,xs):
	ys = list(xs)
	ys.sort()
	return sep.join(ys)

def acceptBinding(key,xs,ys):
	global dicts
	# ys is from the master grammar
	print('        ∑',xs.who(),ys.who())
	if xs.who() in ('Value','Nonterminal') and ys.who() in ('Value','Nonterminal'):
		if ys.data in dicts[key]:
			# know
			if dicts[key][ys.data] == xs.data:
				return
			else:
				print('          ∆',ys.data,'was thought to be mapped to',dicts[key][ys.data],'yet now it maps to',xs.data)
				print('■')
				sys.exit(1)
		else:
			# don't know
			dicts[key][ys.data] = xs.data
			return
	elif xs.who() == 'Sequence' and ys.who() == 'Sequence':
		if len(xs.data) != len(ys.data):
			print('          ∆ Directly matching symbol sequences of different length is impossible.')
			print('■')
			sys.exit(1)
		for i in range(0,len(xs.data)):
			acceptBinding(key,xs.data[i].wrapped,ys.data[i].wrapped)
		return
	elif xs.who() in ('Plus','Star','Optional') and ys.who() in ('Plus','Star','Optional'):
		acceptBinding(key,xs.data.wrapped,ys.data.wrapped)
		return
	print('          ∆ Undecisive.')
	print('■')
	sys.exit(1)
	pass

def renameNin(nt1,nt2,key):
	global cbgfs
	step = CBGF3.Step('rename-rename')
	step.addParam(CBGF3.NonterminalFT(nt1,nt2))
	cbgfs[key].addStep(step)

def renameNinExpr(namemap,expr):
	if expr.who() == 'Sequence':
		e = BGF3.Sequence()
		for el in expr.data:
			e.add(BGF3.Expression(renameNinExpr(namemap,el.wrapped)))
		return e
	elif expr.who() == 'Nonterminal':
		e = BGF3.Nonterminal()
		if expr.data in namemap:
			e.setName(namemap[expr.data])
		else:
			e.setName(expr.data)
		return e
	else:
		print('Unfinished implementation of renameNin for',expr.who())
		print('■')
		sys.exit(1)

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print('This tool does guided grammar convergence.')
		print('Usage:')
		print('      '+sys.argv[0]+' <master-bgf-input> <all-bgf-directory>')
		sys.exit(1)
	master.parse(sys.argv[1])
	for infile in os.listdir(sys.argv[2]):
		if infile.endswith('.bgf'):
			bgf = BGF3.Grammar()
			bgf.parse(os.path.join(sys.argv[2],infile))
			bgfs[infile.split('.')[0]] = bgf
			cbgfs[infile.split('.')[0]] = CBGF3.Sequence()
			dicts[infile.split('.')[0]] = {None:[]}
	print('Parsing the master grammar and the grammarbase of',len(bgfs),'is done.')
	print('Starting with the root.')
	nt = master.roots[0]
	print('  √ In the master grammar the root is called',nt)
	for key in bgfs:
		bgf = bgfs[key]
		if len(bgf.roots) == 1:
			print('  ⇒ In',key+':',nt,'maps to',bgf.roots[0])
			bind(key,nt,bgf.roots[0])
			#renameNin(bgf.roots[0],nt,key)
		else:
			print('  ⇒ Unconclusive for',key,'— looking ahead at definitions:')
			good = checkCandidates('   ',key,nt,bgf.roots,master.getProdsOfN(nt))
			if len(good) == 1:
				print('    ⇒ Hence, in',key+':',nt,'maps to',good[0])
				bind(key,nt,good[0])
				#renameNin(nt.good[0],nt,key)
				# TODO: needed in cases with errorneous roots indicated (i.e., used elsewhere)
				# step = CBGF3.Step('reroot-reroot')
				# step.addParam(CBGF3.Roots('from',bgf.roots))
				# step.addParam(CBGF3.Roots('to',[nt]))
				# cbgfs[key].addStep(step)
			else:
				print('  ⇒ Utterly unconclusive for',key)
				print('■')
				sys.exit(1)
	nts2go = [nt]
	ntsdone = []
	while len(nts2go) > 0:
		nt = nts2go[0]
		nts2go = nts2go[1:]
		print('Working with',nt,'...')
		masterprods = master.getProdsOfN(nt)
		if masterprods:
			print('  √ Defined in the master grammar as',ppseplist(';',map(lambda x:str(x.expr),masterprods)))
		else:
			print('  √ Undefined in the master grammar.')
		msigs = [makeSignature(p.nt,p.expr) for p in masterprods]
		mprodsig = list(map(lambda a:joinsort('/',fst(a)),msigs))
		print('  √ Master prodsig:',mprodsig)
		for key in bgfs:
			bgf = bgfs[key]
			if nt not in dicts[key]:
				#print('  √ A grammar still',key,'has multiple roots:',bgf.roots)
				print('  ✗ Suddenly,',key,'has no match for',nt)
				# TODO?
				continue
				#print('■')
				#sys.exit(1)
			print('  √ Called',dicts[key][nt],'in',key)
			# now it should be mapped
			myprods = bgf.getProdsOfN(dicts[key][nt])
			if myprods:
				print('  √ Defined in',key,'as',ppseplist(';',map(lambda x:str(x.expr),myprods)))
			else:
				print('  √ Undefined in',key)
			if len(myprods) == 0:
				if len(masterprods) == 0:
					print('    ☯ Both are undefined.')
				else:
					# TODO
					print('    ✗ Expected definition of',nt,'('+dicts[key][nt]+')','is not found in',key)
					print('■')
					sys.exit(1)
			elif len(myprods) == 1:
				if masterprods:
					# TODO: how about master having multiple rules where key grammar has one?
					# Answer: a case of addV-equivalence, which is “more liberal” anyway
					if sameThing(key,myprods[0].expr,None,masterprods[0].expr):
						print('    ⇒',masterprods[0].expr.wrapped.data,'maps to',myprods[0].expr.wrapped.data,'in',key)
						nnt = str(masterprods[0].expr.wrapped.data)
						bind(key,nnt,str(myprods[0].expr.wrapped.data))
						#renameNin(str(myprods[0].expr.wrapped.data),nnt,key)
						if nnt not in ntsdone and nnt not in nts2go and nnt not in ['string','int']:
							nts2go.append(nnt)
					elif moreLiberal(myprods[0],masterprods[0]):
						print('    ☯ Disregarding more liberal specification,')
						print('     ⇒',masterprods[0].expr.wrapped.data,'maps to',myprods[0].expr.wrapped.data)
						bind(key,str(masterprods[0].expr.wrapped.data),str(myprods[0].expr.wrapped.data))
						#renameNin(str(myprods[0].expr.wrapped.data),str(masterprods[0].expr.wrapped.data),key)
					else:
						match('    ',key,dicts[key][nt],myprods[0].expr.wrapped,nt,masterprods[0].expr.wrapped)
				else:
					# undefined in the master grammar
					if sameThing(key,myprods[0].expr,None,BGF3.Expression(BGF3.Empty())):
						print('    ☯ Strictly speaking, undefined is φ, so')
						print('     ⇒ None maps to',myprods[0].expr.wrapped.data,'in',key)
						print('■TODO')
						sys.exit(1)							
					elif sameThing(key,myprods[0].expr,None,BGF3.Expression(BGF3.Epsilon())):
						# HACK!
						# bind(key,None,nt)
						print('    ☯ Considering undefined as ε,')
						print('     ⇒ Trivial match in',key)
						step = CBGF3.Step('undefine-define')
						p = BGF3.Production()
						p.nt = nt
						p.expr = myprods[0].expr
						p.label = myprods[0].label
						step.addParam(p)
						cbgfs[key].addStep(step)
					else:
						print('    ✗ Desperately unmatched part.')
						print('■')
						sys.exit(1)							
			else:
				sigs = [makeSignature(p.nt,p.expr) for p in myprods]
				prodsig = list(map(lambda a:joinsort('/',fst(a)),sigs))
				#prodsig.sort()
				print('    √ Prodsig: ',prodsig)
				if prodsig == ['1']*len(prodsig):
					oldnt = myprods[0].nt
					newprods = []
					for p in myprods:
						if p.expr.wrapped.who() == 'Nonterminal' and len(bgf.getProdsOfN(p.expr.wrapped.data)) == 1:
							# HACK!
							bind(key,None,p.expr.wrapped.data)
							# singletons shall be unfolded!
							unt = p.expr.wrapped.data
							newprods.extend(bgf.getProdsOfN(unt))
							step = CBGF3.Step('inline-extract')
							# TODO: make it work for multiple productions!
							step.addParam(bgf.getProdsOfN(unt)[0])
							# scope is not enough
							#step.addParam(BGF3.Scope(CBGF3.Nonterminal(dicts[key][nt])))
							cbgfs[key].addFirstStep(step)
							#print('UNFOLD!!!',unt)
						else:
							newprods.append(p)
					myprods = newprods
					sigs = [makeSignature(oldnt,p.expr) for p in newprods]
					prodsig = list(map(lambda a:joinsort('/',fst(a)),sigs))
					print('    √ Unfolded:',prodsig)
				print('    √ Signats:',sigs)
				versions = []
				for i in range(0,len(msigs)):
					versions.append([])
					for j in range(0,len(sigs)):
						if mprodsig[i] == prodsig[j]:
							versions[i].append(myprods[j].expr)
							#versions[i].append(str(myprods[j].expr))
					if len(versions[i]) == 0:
						# print('    ☯ TRY')
						for j in range(0,len(sigs)):
							if moreLiberalSign(mprodsig[i],prodsig[j]):
								versions[i].append(myprods[j].expr)
								print('    ☯ Suggesting a liberation-based version with',mprodsig[i],'<:',prodsig[j])
								#versions[i].append(str(myprods[j].expr))						
				limit = 3
				while max(map(len,versions)) > 1:
					limit -= 1
					print('    √ Versions:',list(map(lambda a:list(map(str,a)),versions)))
					# while we have more than one version...
					i = 0
					while len(versions[i]) <= 1:
						i += 1
					print('    ? Trying to match',' or '.join(map(str,versions[i])),'to',masterprods[i].expr)
					#print('DICTS:',dicts[key])
					for p in versions[i]:
						#print('??? same thing:',p,masterprods[i].expr)
						if sameThing(key,p,None,masterprods[i].expr):
							print('      ⇒',p.wrapped.data,'maps to',masterprods[i].expr.wrapped.data)
							bind(key,masterprods[i].expr.wrapped.data,p.wrapped.data)
							#renameNin(p.wrapped.data,masterprods[i].expr.wrapped.data,key)
							for k in range(0,len(versions)):
								if k==i:
									versions[k] = [p]
								# this relies on overloaded expression equality
								elif p in versions[k]:
									versions[k].remove(p)
						elif masterprods[i].expr.wrapped.data in dicts[key] and dicts[key][masterprods[i].expr.wrapped.data] == p.wrapped.data:
							# this should be generalised in order to work on sequences
							# i.e., matching a b+ to c d+ or e f+
							print('      ≈ It was known that',p.wrapped.data,'maps to',masterprods[i].expr.wrapped.data)
							for k in range(0,len(versions)):
								if k==i:
									versions[k] = [p]
								# this relies on overloaded expression equality
								elif p in versions[k]:
									versions[k].remove(p)
					#good = checkCandidates('   ',key,nt,versions[i],master.getProdsOfN(nt))
					if limit==0:
						print('—————REACHED THE LIMIT—————')
						break
				# recording the result
				if limit:
					#unmatched = [p.expr for p in myprods]
					unmatched = [p for p in myprods]
					print('    √ Experiments are settled.')
					print('    √ Versions:',list(map(lambda a:list(map(str,a)),versions)))
					for i in range(0,len(versions)):
						if versions[i]:
							expr1 = versions[i][0].wrapped
							expr2 = masterprods[i].expr.wrapped
							print('      √ Successfully matched',expr1,'with',expr2)
							print(makeSignature(dicts[key][nt],versions[i][0]))
							print(makeSignature(masterprods[i].nt,masterprods[i].expr))
							# TODO: detect permutations!!!
							match('      ',key,dicts[key][nt],expr1,nt,expr2)
							# the following line does not work directly as expected
							#unmatched.remove(expr1)
							# for e in unmatched:
							# 	if e.wrapped == expr1:
							# 		unmatched.remove(e)
							# 		break
							for p in unmatched:
								if p.expr.wrapped == expr1:
									unmatched.remove(p)
									break
							# detecting permutations
							expr1s = str(expr1)
							if expr1s[0]=='(':
								expr1s = expr1s[1:-1]
							for n in dicts[key]:
								if n:
									expr1s = expr1s.replace(dicts[key][n],n)
							expr2s = str(expr2)
							if expr2s[0]=='(':
								expr2s = expr2s[1:-1]
							if expr1s == expr2s:
								# complete match
								pass
							else:
								expr1s = expr1s.split()
								expr1s.sort()
								expr2s = expr2s.split()
								expr2s.sort()
								if expr1s == expr2s:
									print('       ☯ Permutation detected.')
									expr3 = renameNinExpr({dicts[key][k]:k for k in dicts[key] if k},expr1)
									step = CBGF3.Step('designate-unlabel')
									p = BGF3.Production()
									p.nt = nt
									p.label = 'TRAFO-TARGET'
									p.expr = expr3
									step.addParam(p)
									cbgfs[key].addStep(step)
									step = CBGF3.Step('permute-permute')
									step.addParam(p)
									p = BGF3.Production()
									p.nt = nt
									p.label = 'TRAFO-TARGET'
									p.expr = expr2
									step.addParam(p)
									cbgfs[key].addStep(step)
									step = CBGF3.Step('unlabel-designate')
									step.addParam(p)
									cbgfs[key].addStep(step)
								elif ' '.join(expr1s).replace('STR','string') == ' '.join(expr2s).replace('STR','string'):
									# oops
									pass
								elif ' '.join(expr1s).replace('INT','int') == ' '.join(expr2s).replace('INT','int'):
									# oops
									pass
								else:
									print('       ✗ Assumption failed:',expr1s,expr2s)
									print('■')
									# sys.exit(1)
						else:
							expr2 = masterprods[i].expr.wrapped
							print('      ✗ No matching for',expr2,'found!')
							#print(list(map(str,myprods)))
							#print('■')
							#sys.exit(1)
						# acceptBinding(key,expr1,expr2)
					if unmatched:
						print('    ! Unmatched productions:',list(map(lambda x:str(x.expr),unmatched)))
						for p in unmatched:
							if p.expr.wrapped.who() == 'Nonterminal' and p.nt == p.expr.wrapped.data:
								step = CBGF3.Step('abridge-detour')
								step.addParam(p)
								cbgfs[key].addFirstStep(step)
								print('      √ A reflexive chain production disregarded.')
							else:
								print('      ✗ Disregarding failed.')
				else:
					print('    ✗ Dealing with multiple rules was unsuccessful.')
					print('■')
					sys.exit(1)
			# print('    ',myprods[0].expr.wrapped.who())
			# TODO make it work for more rules
			#print(myprods[0].nt)
			#p.expr.wrapped.who() == 'Choice'
			ntsdone.append(nt)
			# cheap way to say "now do all referenced nonterminals that you haven't done yet"
			for k in dicts[key]:
				if k and k not in ntsdone and k not in nts2go and k not in ['string','int']:
					nts2go.append(k)
	allnts = []
	alllocalnts = {}
	allrealnts = {}
	disregard = {}
	for key in dicts:
		alllocalnts[key] = []
		allrealnts[key] = []
		for nt in dicts[key]:
			if nt and nt not in allnts:
				allnts.append(nt)
			if dicts[key][nt] and dicts[key][nt] not in alllocalnts[key]:
				alllocalnts[key].append(dicts[key][nt])
		for nt in map(lambda x:x.text,bgfs[key].getXml().findall(".//nonterminal")):
			if nt not in allrealnts[key]:
				allrealnts[key].append(nt)
	print('√ Disregarded nonterminals:\n • ',end='')
	for key in dicts:
		disregard[key] = []
		for nt in dicts[key][None]:
			print(key+'.'+nt+', ',end='')
			disregard[key].append(nt)
	print('…')
	for nt in allnts:
		print('√',nt,'maps to:\n • ',end='')
		for key in dicts:
			if nt in dicts[key]:
				for n in dicts[key][nt].split('+'):
					print(key+'.'+n+', ',end='')
			else:
				print(key+'.???, ',end='')
		print()
	# disregard untouched nonterminals
	for key in bgfs.keys():
		for nt in allrealnts[key]:
			if nt not in alllocalnts[key] and nt not in disregard[key]:
				print(' ✗ Also disregarding unused',nt,'in',key)
				step = CBGF3.Step('eliminate-introduce')
				step.params.extend(bgfs[key].getProdsOfN(nt))
				cbgfs[key].addFirstStep(step)
	print('√ Writing ΞBGF files…')
	for key in cbgfs.keys():
		# print('All real NTs:',allrealnts[key])
		for valuetype in ('string','int'):
			if valuetype in allnts and valuetype not in alllocalnts[key]:
				# print(list(map(lambda x:x.text,bgfs[key].getXml().findall(".//nonterminal"))))
				if valuetype not in map(lambda x:x.text,bgfs[key].getXml().findall(".//nonterminal")):
					print('  & Built-in sort',valuetype,'usage detected in',key)
					step = CBGF3.Step('define-undefine')
					p = BGF3.Production()
					p.nt = valuetype
					v = BGF3.Value()
					v.data = valuetype
					p.expr = BGF3.Expression(v)
					step.addParam(p)
					cbgfs[key].addStep(step)
					step = CBGF3.Step('inline-extract')
					step.addParam(p)
					cbgfs[key].addStep(step)
			# print(alllocalnts[key])
			# if valuetype in alllocalnts[key] and used:
			# 	print(list(map(lambda x:x.text,bgfs[key].getXml().findall(".//nonterminal"))))
			# 	if valuetype in map(lambda x:x.text,bgfs[key].getXml().findall(".//nonterminal")):
			# 		print('yES')
			# 	else:
			# 		print('  & Built-in sort',valuetype,'usage detected in the master grammar',key)
			# 		step = CBGF3.Step('define-undefine')
			# 		p = BGF3.Production()
			# 		p.nt = valuetype
			# 		v = BGF3.Value()
			# 		v.data = valuetype
			# 		p.expr = BGF3.Expression(v)
			# 		step.addParam(p)
			# 		cbgfs[key].addStep(step)
			# 		step = CBGF3.Step('inline-extract')
			# 		step.addParam(p)
			# 		cbgfs[key].addStep(step)
		ET.ElementTree(cbgfs[key].getXml()).write(key+'.cbgf')
		print('   √',key+'.ΞBGF')
	for key in (): #['rascal']:
		for nt in allnts:
			if nt in dicts[key]:
				print(nt,'->',dicts[key][nt])
				print('\n'.join(map(str,master.getProdsOfN(nt))))
				print('\n'.join(map(str,bgfs[key].getProdsOfN(dicts[key][nt]))))
			else:
				print(nt,'-> ???')
		print(alllocalnts[key])
	print('■')
	sys.exit(0)

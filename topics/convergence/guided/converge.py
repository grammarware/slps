#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
from functools import reduce
import os,sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF3
import XBGF3
import slpsns
import metrics3

# globals
bgfs = {}
dicts = {}
master = BGF3.Grammar()
# current version dictionary
cvdict = {}

def makeSignature(d,ss):
	# Input: sequence
	sign = {}
	if ss.__class__.__name__ == 'Sequence':
		for x in ss.data:
			if x.wrapped.__class__.__name__ == 'Nonterminal':
				nt = x.wrapped.data
				q = '1'
			elif x.wrapped.__class__.__name__ == 'Star':
				nt = x.wrapped.data.wrapped.data
				q = '*'
			elif x.wrapped.__class__.__name__ == 'Plus':
				nt = x.wrapped.data.wrapped.data
				q = '+'
			elif x.wrapped.__class__.__name__ == 'Optional':
				nt = x.wrapped.data.wrapped.data
				q = '?'
			elif x.wrapped.__class__.__name__ == 'Value':
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
					q = '#'
				elif q == '*':
					q = '•'
				else:
					print('ERROR: Please rewrite the code of makeSignature!')
			if nt in sign:
				sign[nt].append(q)
			else:
				sign[nt] = [q]
	elif ss.__class__.__name__ in ('Nonterminal','Value'):
		if ss.data == d:
			return [('0',ss.data)]
		else:
			return [('1',ss.data)]
	elif ss.__class__.__name__ == 'Expression':
		return makeSignature(d,ss.wrapped)
	elif ss.__class__.__name__ == 'Plus':
		return [('+',str(ss.data.wrapped))]
	else:
		print('!!! Signatures not implemented for',ss.__class__.__name__)
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
	# [(a,b),(x),(1,2)] ⇒ [(a,x,1),(a,x,2),(b,x,1),(b,x,2)]
	# (a,b) x [(x,1),(x,2)]
	# [(a,x),(b,x)] x (1,2)
	res = []
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

def match(key,xnt,xs,ynt,ys):
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
		print('    √ Only one version:')
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
					print('     ☯ Disregarding more liberal signature,')
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
			print('     ☯ Disregarding',dis+',')
		#print('DICTS:',dicts[key])
		for match in version:
			if match[2]:
				print('      ⇒ in',key+':',match[2],'maps to',match[1],'with signature',match[0])
				dicts[key][match[2]] = match[1]
			else:
				dicts[key][None].append(match[1])
		#print('DICTS:',dicts[key])
		cvdict = {}
	else:
		print('     ⇒ Multiple mapping versions:')
		cx = 0
		for version in alltriplets:
			cvdict = {m[1]:m[2] for m in version}
			cx +=1
			print('     ? Version',cx,':',joinsort(', ',[str(t[2])+' is '+str(t[1]) for t in version]))
			unmatched = list(map(lambda a:a[2],filter(lambda a:a[1]==None,version)))
			disregard = appnd(setmin(snd(xsign),snd(version)),list(map(lambda a:a[1],filter(lambda a:a[2]==None,version))))
			disq = False
			#print('      ~version~>',version)
			for match in version:
				good = checkCandidates('      ',key,match[2],[match[1]],master.getProdsOfN(match[2]))
				if len(good)!=1:
					print('       ✗ stop checking')
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
					print('      ✗ version disqualified, an adapted variant is proposed')
				else:
					print('      ✗ version disqualified')
			else:
				print('      √ version approved')
				#print('VERSION:',version)
				for dis in setmin(disregard,'+'.join(list(map(lambda x:x[1],filter(lambda x:x[1] and x[1].find('+')>-1,version)))).split('+')):
					print('     ☯ Disregarding',dis+',')
				#print('DICTS:',dicts[key])
				for match in version:
					if match[2]:
						if match[1].find('+')>-1:
							print('       ⇒',match[2],'maps to',match[1].replace('+',' and '),'with signature',match[0])
							dicts[key][match[2]] = match[1]#.split('+')
						else:
							print('       ⇒',match[2],'maps to',match[1],'with signature',match[0])
							dicts[key][match[2]] = match[1]
					else:
						dicts[key][None].append(match[1])
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
	if x.wrapped.__class__.__name__ == y.wrapped.__class__.__name__:
		if y.wrapped.__class__.__name__ in ('Plus','Star','Optional'):
			# we don't check for contents
			return True
		elif x.wrapped.__class__.__name__ == 'Nonterminal':
			# only if nonterminal names are equal
			if x.wrapped.data in cvdict.keys():
				return cvdict[x.wrapped.data] == y.wrapped.data
			else:
				return x.wrapped.data == y.wrapped.data
		elif x.wrapped.__class__.__name__ == 'Value':
			# only if values are equal
			return x.wrapped.data == y.wrapped.data
		elif x.wrapped.__class__.__name__ == 'Sequence' and len(x.wrapped.data) == len(y.wrapped.data):
			res = True
			for i in range(0,len(x.wrapped.data)):
				res &= sameThing(xkey,x.wrapped.data[i],ykey,y.wrapped.data[i])
			return res
	elif x.wrapped.__class__.__name__ == 'Nonterminal':
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
	elif y.wrapped.__class__.__name__ == 'Nonterminal':
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
	if x.find('/')>-1 and y.find('/')>-1:
		if joinsort('/',map(lambda a:a.replace('#','•'),x.split('/'))) == joinsort('/',map(lambda a:a.replace('#','•'),y.split('/'))):
			return True
	return False

def moreLiberal(x,y):
	# x is more liberal than y
	xc = x.expr.wrapped.__class__.__name__
	yc = y.expr.wrapped.__class__.__name__
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
		elif not nt and len(myprods) == 1 and myprods[0].expr.wrapped.__class__.__name__ == 'Value':
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
			dicts[infile.split('.')[0]] = {None:[]}
	print('Parsing the master grammar and the grammarbase of',len(bgfs),'is done.')
	print('Starting with the root.')
	nt = master.roots[0]
	print('  √ In the master grammar the root is called',nt)
	for key in bgfs:
		bgf = bgfs[key]
		if len(bgf.roots) == 1:
			print('  ⇒ In',key+':',nt,'maps to',bgf.roots[0])
			dicts[key][nt] = bgf.roots[0]
		else:
			print('  ⇒ Unconclusive for',key,'— looking ahead at definitions:')
			good = checkCandidates('   ',key,nt,bgf.roots,master.getProdsOfN(nt))
			if len(good) == 1:
				print('    ⇒ Hence, in',key+':',nt,'maps to',good[0])
				dicts[key][nt] = good[0]
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
				print('  √ A grammar still',key,'has multiple roots:',bgf.roots)
				print('■')
				sys.exit(1)
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
					print('    ✗ One is defined, the other one is not, we have a problem.')
			elif len(myprods) == 1:
				if sameThing(key,myprods[0].expr,None,masterprods[0].expr):
					print('    ⇒',masterprods[0].expr.wrapped.data,'maps to',myprods[0].expr.wrapped.data,'in',key)
					nnt = str(masterprods[0].expr.wrapped.data)
					dicts[key][nnt] = str(myprods[0].expr.wrapped.data)
					if nnt not in ntsdone and nnt not in nts2go:
						nts2go.append(nnt)
				elif moreLiberal(myprods[0],masterprods[0]):
					print('    ☯ Disregarding more liberal specification,')
					print('     ⇒',masterprods[0].expr.wrapped.data,'maps to',myprods[0].expr.wrapped.data)
					dicts[key][str(masterprods[0].expr.wrapped.data)] = str(myprods[0].expr.wrapped.data)
				else:
					match(key,dicts[key][nt],myprods[0].expr.wrapped,nt,masterprods[0].expr.wrapped)
			else:
				sigs = [makeSignature(p.nt,p.expr) for p in myprods]
				prodsig = list(map(lambda a:joinsort('/',fst(a)),sigs))
				#prodsig.sort()
				print('    √ Prodsig: ',prodsig)
				if prodsig == ['1']*len(prodsig):
					oldnt = myprods[0].nt
					newprods = []
					for p in myprods:
						if p.expr.wrapped.__class__.__name__ == 'Nonterminal':
							# singletons shall be unfolded!
							unt = p.expr.wrapped.data
							newprods.extend(bgf.getProdsOfN(unt))
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
					for p in versions[i]:
						#print('??? same thing:',p,masterprods[i].expr)
						if sameThing(key,p,None,masterprods[i].expr):
							print('      ⇒',p.wrapped.data,'maps to',masterprods[i].expr.wrapped.data)
							dicts[key][masterprods[i].expr.wrapped.data] = p.wrapped.data
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
					print('    √ Experiments are settled.')
					for i in range(0,len(versions)):
						if versions[i]:
							expr1 = versions[i][0]
						else:
							expr2 = None
						expr2 = masterprods[i].expr.wrapped
						print('      √ Successfully matched',expr1,'with',expr2)
				else:
					print('    ✗ Dealing with multiple rules was unsuccessful.')
					print('■')
					sys.exit(1)
			# print('    ',myprods[0].expr.wrapped.__class__.__name__)
			# TODO make it work for more rules
			#print(myprods[0].nt)
			#p.expr.wrapped.__class__.__name__ == 'Choice'
			ntsdone.append(nt)
			# cheap way to say "now do all referenced nonterminals that you haven't done yet"
			for k in dicts[key]:
				if k and k not in ntsdone and k not in nts2go and k not in ['string','int']:
					nts2go.append(k)
	print('■')
	sys.exit(0)

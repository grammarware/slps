#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
import os
import sys
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import slpsns
import xml.etree.ElementTree as ET
import BGF3,XBGF3

namemap = {}

def applynamemap(e):
	if e.who()=='Expression':
		applynamemap(e.wrapped)
		return
	elif e.who()=='Nonterminal':
		if e.data in namemap:
			e.data = namemap[e.data]
		return
	print('[MBGF] applynamemap error:',e.who())
	sys.exit(1)

def wrapexp(exp,mod):
	if mod=='1':
		return str(exp)
	else:
		return str(exp)+mod
		# return '('+str(exp)+')'+mod

if __name__ == "__main__":
	if len(sys.argv)!=5:
		print('This tool needs MBGF as an input, XBGF as an output and a number of expected BGF input')
		print('\nUsage:')
		print('	',sys.argv[0],'<input-mbgf>','<in-num>','<out-num>','<output-xbgf>')
		sys.exit(1)
	else:
		mbgf = ET.parse(sys.argv[1])
		innum = int(sys.argv[2])
		outnum = int(sys.argv[3])
		xbgf = XBGF3.Sequence()
		for e in mbgf.findall('*'):
			if e.tag==slpsns.mbgf_('naming'):
				n1 = e.findall('nonterminal')[innum-1].text
				n2 = e.findall('nonterminal')[outnum-1].text
				namemap[e.findtext('name')] = n2 # vice versa?
				if n1==n2:
					print('[MBGF] naming('+n1+','+n2+') ::= id')
				else:
					print('[MBGF] naming('+n1+','+n2+') ::= renameN('+n1+','+n2+')')
					ren = XBGF3.Step('rename')
					w = XBGF3.Wrapping('nonterminal')
					w.addChild(XBGF3.Leaf('from',n1))
					w.addChild(XBGF3.Leaf('to',n2))
					ren.addParam(w)
					xbgf.addStep(ren)
			elif e.tag==slpsns.mbgf_('width'):
				exp = BGF3.Expression([])
				exp.parse(e.findall(slpsns.bgf_('expression'))[0])
				applynamemap(exp)
				e1 = e.findall('expr')[innum-1].text
				e2 = e.findall('expr')[outnum-1].text
				i = e.findall('in')
				# namemap[e.findtext('name')] = n2 # vice versa?
				yes = False
				if e1==e2:
					print('[MBGF] width('+str(exp)+','+e1+','+e2+') ::= id')
				elif (e1,e2) in [('*','+'),('*','?'),('*','1'),('+','1'),('?','1')]:
					ren = XBGF3.Step('narrow')
					yes = True
				elif (e2,e1) in [('*','+'),('*','?'),('*','1'),('+','1'),('?','1')]:
					ren = XBGF3.Step('widen')
					yes = True
				else:
					print('[MBGF] width error: not in narrowing relation!')
					sys.exit(1)
				if yes:
					print('[MBGF] width('+str(exp)+','+e1+','+e2+') ::= '+ren.name+'('+wrapexp(exp,e1)+','+wrapexp(exp,e2)+')')
					if e1=='1':
						ren.addParam(exp)
					elif e1=='+':
						p = BGF3.Plus()
						p.setExpr(exp)
						ren.addParam(BGF3.Expression(p))
					elif e1=='*':
						p = BGF3.Star()
						p.setExpr(exp)
						ren.addParam(BGF3.Expression(p))
					elif e1=='?':
						p = BGF3.Optional()
						p.setExpr(exp)
						ren.addParam(BGF3.Expression(p))
					else:
						print('[MBGF] width error: unknown modifier!')
						sys.exit(1)
					if e2=='1':
						ren.addParam(exp)
					elif e2=='+':
						p = BGF3.Plus()
						p.setExpr(exp)
						ren.addParam(BGF3.Expression(p))
					elif e2=='*':
						p = BGF3.Star()
						p.setExpr(exp)
						ren.addParam(BGF3.Expression(p))
					elif e2=='?':
						p = BGF3.Optional()
						p.setExpr(exp)
						ren.addParam(BGF3.Expression(p))
					else:
						print('[MBGF] width error: unknown modifier!')
						sys.exit(1)
					xbgf.addStep(ren)
			elif e.tag==slpsns.mbgf_('unification'):
				n0 = e.findtext('name')
				n1 = e.findall('add')[innum-1].findtext(slpsns.bgf_('production/nonterminal'))
				n2 = e.findall('add')[outnum-1].findtext(slpsns.bgf_('production/nonterminal'))
				if n0 in namemap:
					n3 = namemap[n0]
				else:
					n3 = n0
				if n1:
					print('[MBGF] unification('+n1+','+n0+') ::= unite('+n1+','+n3+')')
					ren = XBGF3.Step('unite')
					ren.addParam(XBGF3.Leaf('add',n1))
					ren.addParam(XBGF3.Leaf('to',n3))
					xbgf.addStep(ren)
				elif n2:
					print('[MBGF] unification('+n2+','+n0+') ::= split('+n3+','+n0+')')
					ren = XBGF3.Step('split')
					ren.addParam(XBGF3.Leaf('nonterminal',n3))
					for q in e.findall('add')[outnum-1].findall(slpsns.bgf_('production')):
						p = BGF3.Production()
						p.parse(q)
						ren.addParam(p)
					attrs = e.findall('add')[outnum-1].attrib
					if 'labels' in attrs:
						for l in attrs['labels'].split(','):
							ren.addParam(BGF3.LabelText(l))
					# print('!!!',e.findall('add')[outnum-1].attrib)
					# ren.addParam(XBGF3.Leaf('to',n3))
					xbgf.addStep(ren)
				else:
					# TODO: check for the situation when both n1 and n2 are not empty
					print('[MBGF] unification(∅,'+n2+') ::= id')
			elif e.tag==slpsns.mbgf_('iteration'):
				l = e.findtext('label')
				n = e.findtext('name')
				s = e.findtext('separator')
				k1 = e.findall('kind')[innum-1].text
				k2 = e.findall('kind')[outnum-1].text
				if k1==k2:
					print('[MBGF] iteration('+l+','+n+','+s+','+k1+','+k2+') ::= id')
				elif k1=='iterate' and k2.endswith('assoc'):
					if n in namemap:
						n1 = namemap[n]
					else:
						n1 = n
					if s in namemap:
						s1 = namemap[s]
					else:
						s1 = s
					print('[MBGF] iteration('+l+','+n+','+s+','+k1+','+k2+') ::= '+k2+'(['+l+'] '+n1+' ← '+n1+' '+s1+' '+n1+')')
					ren = XBGF3.Step(k2)
					p = BGF3.Production()
					p.setLabel(l)
					p.setNT(n1)
					e = BGF3.Sequence()
					e2 = BGF3.Nonterminal()
					e2.setName(n1)
					e.add(BGF3.Expression(e2))
					if s:
						e2 = BGF3.Nonterminal()
						e2.setName(s1)
						e.add(BGF3.Expression(e2))
					e2 = BGF3.Nonterminal()
					e2.setName(n1)
					e.add(BGF3.Expression(e2))
					p.setExpr(BGF3.Expression(e))
					ren.addParam(p)
					xbgf.addStep(ren)
				elif k1.endswith('assoc') and k2=='iterate':
					if n in namemap:
						n1 = namemap[n]
					else:
						n1 = n
					if s in namemap:
						s1 = namemap[s]
					else:
						s1 = s
					if s1:
						ass = n1+' ('+s1+' '+n1+')*'
					else:
						ass = n1+'+'
					print('[MBGF] iteration('+l+','+n+','+s+','+k1+','+k2+') ::= iterate(['+l+'] '+n1+' ← '+ass+')')
					ren = XBGF3.Step('iterate')
					p = BGF3.Production()
					p.setLabel(l)
					p.setNT(n1)
					if s1:
						# N (S N)*
						e = BGF3.Sequence()
						e2 = BGF3.Nonterminal()
						e2.setName(n1)
						e.add(BGF3.Expression(e2))
						e3 = BGF3.Sequence()
						e4 = BGF3.Nonterminal()
						e4.setName(s1)
						e3.add(BGF3.Expression(e4))
						e3.add(BGF3.Expression(e2))
						e5 = BGF3.Star()
						e5.setExpr(BGF3.Expression(e3))
						e.add(BGF3.Expression(e5))
					else:
						# N+
						e = BGF3.Plus()
						e2 = BGF3.Nonterminal()
						e2.setName(n1)
						e.setExpr(BGF3.Expression(e2))
					p.setExpr(BGF3.Expression(e))
					ren.addParam(p)
					xbgf.addStep(ren)
				else:
					print('[MBGF] iteration PROBLEM')
					sys.exit(1)
			else:
				print('[MBGF] Unknown command:',e.tag)
		ET.ElementTree(xbgf.getXml()).write(sys.argv[4])
		print('Success')
        
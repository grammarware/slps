#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os,sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF3
import XBGF3
import slpsns
import metrics3

def anon(e):
	#print('Visiting',e.tag)
	if e.tag in ('terminal','nonterminal','epsilon','empty','any','value'):
		return e
	if e.tag == 'selectable':
		ne = ET.Element('marked')
		se = ET.SubElement(ne,slpsns.bgf_('expression'))
		ie = ET.SubElement(se,'selectable')
		ET.SubElement(ie,'selector').text = e.findtext('selector')
		ie.append(anon(e.findall(slpsns.bgf_('expression'))[0]))
		return ne
	ne = ET.Element(e.tag)
	for child in e.findall('*'):
		ne.append(anon(child))
	return ne

def abstr(e):
	#print('Visiting',e.tag)
	if e.tag == 'terminal':
		ne = ET.Element('marked')
		se = ET.SubElement(ne,slpsns.bgf_('expression'))
		se.append(e)
		return ne
	if e.tag in ('nonterminal','epsilon','empty','any','value'):
		return e
	if e.tag == 'selectable':
		return abstr(e.findall(slpsns.bgf_('expression'))[0].findall('*')[0])
	ne = ET.Element(e.tag)
	for child in e.findall('*'):
		ne.append(abstr(child))
	return ne

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print('This tool generates an XBGF script to horizontalize all vertical nonterminals present in the grammar.')
		print('Usage:')
		print('      '+sys.argv[0]+' <bgf-input> <xbgf-output>')
		sys.exit(1)
	bgf = BGF3.Grammar()
	bgf.parse(sys.argv[1])
	xbgf = XBGF3.Sequence()
	#xbgf.write('<?xml version="1.0" encoding="UTF-8"?>\n<xbgf:sequence xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:bgf="http://planet-sl.org/bgf">\n')
	for p in bgf.prods:
		if p.label:
			print("Unlabel",p.label)
			s = XBGF3.Step('unlabel')
			s.addParam(XBGF3.Label(p.label))
			xbgf.addStep(s)
			#xbgf.write('<xbgf:unlabel><label>'+p.label+'</label></xbgf:unlabel>\n')
		pxml = p.getXml()
		if pxml.findall('.//selectable'):
			print("Anonymize in",p.nt)
			s = XBGF3.Step('anonymize')
			ip = BGF3.Production()
			ip.parse(anon(p.getXml()))
			ip.setLabel('')
			s.addParam(ip)
			xbgf.addStep(s)
			#xbgf.write('<xbgf:anonymize><bgf:production>')
			#ET.ElementTree(anon(p.expr.getXml())).write(xbgf)
			#xbgf.write('</bgf:production></xbgf:anonymize>\n')
		if pxml.findall('.//terminal'):
			print("Abstractize in",p.nt)
			s = XBGF3.Step('abstractize')
			ip = BGF3.Production()
			ip.parse(abstr(p.getXml()))
			ip.setLabel('')
			s.addParam(ip)
			xbgf.addStep(s)
		# does not work because some choices will disappear by in-place normalisation that XBGF does
		# if p.expr.wrapped.__class__.__name__ == 'Choice':
		# 	print('Verticalize',p.nt)
		# 	s = XBGF3.Step('vertical')
		# 	n = XBGF3.Nonterminal()
		# 	n.setName(p.nt)
		# 	s.addParam(n)
		# 	xbgf.addStep(s)
	#xbgf.write('</xbgf:sequence>')
	#xbgf.close()
	top = metrics3.top(bgf)
	bot = metrics3.bot(bgf)
	lea = metrics3.leaves(bgf)
	s = XBGF3.Step('reroot')
	for n in top:
		if n not in lea:
			s.addParam(XBGF3.Root(n))
	if len(s.params)>0:
		x = list(map(str,s.params))
		x.sort()
		y = bgf.roots
		y.sort()
		if x != y:
			# reroot only if not vacuous
			xbgf.addStep(s)
			#print('Tops:',top,', bottoms:',bot,', leaves:',lea)
			print("Reroot to",x)
	ET.ElementTree(xbgf.getXml()).write(sys.argv[2])
	sys.exit(0)

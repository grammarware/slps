#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os, sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF3, slpsns
from functools import reduce
import xml.etree.ElementTree as ET

debug = False
#debug = True

defaults = {'definition-separator-symbol':'|||||'}
config = {}
masked = {}
always_terminals = []
always_nonterminals = []
ignore_tokens = []
ignore_lines = []
nonterminals_alphabet = ['-','_']
nonterminals_start = []
multiples = []
aliases = {}

metasymbols = \
	[
		'DEFINING-SYMBOL',
		'TERMINATOR-SYMBOL',
		'MULTIPLE-DEFINING-SYMBOL',
		'DEFINITION-SEPARATOR-SYMBOL',
		'START-GROUP-SYMBOL',
		'END-GROUP-SYMBOL',
		'START-OPTION-SYMBOL',
		'END-OPTION-SYMBOL',
		'START-REPETITION-STAR-SYMBOL',
		'END-REPETITION-STAR-SYMBOL',
		'START-REPETITION-PLUS-SYMBOL',
		'END-REPETITION-PLUS-SYMBOL',
		'START-SEPLIST-STAR-SYMBOL',
		'END-SEPLIST-STAR-SYMBOL',
		'START-SEPLIST-PLUS-SYMBOL',
		'END-SEPLIST-PLUS-SYMBOL',
		'POSTFIX-OPTION-SYMBOL',
		'POSTFIX-REPETITION-STAR-SYMBOL',
		'POSTFIX-REPETITION-PLUS-SYMBOL',
	]
specials = \
	[
		'POSSIBLE-TERMINATOR-SYMBOL',
		'CONCATENATE-SYMBOL',
		'LINE-CONTINUATION-SYMBOL'
		'START-TERMINAL-SYMBOL',
		'END-TERMINAL-SYMBOL',
		'START-NONTERMINAL-SYMBOL',
		'END-NONTERMINAL-SYMBOL',
		'NONTERMINAL-IF-CONTAINS',
		'NONTERMINAL-IF-DEFINED',
		'NONTERMINAL-IF-UPPERCASE',
		'NONTERMINAL-IF-LOWERCASE',
		'NONTERMINAL-IF-CAMELCASE',
		'NONTERMINAL-IF-MIXEDCASE',
		'TERMINAL-IF-UNDEFINED',
		'TERMINAL-IF-UPPERCASE',
		'TERMINAL-IF-LOWERCASE',
		'TERMINAL-IF-CAMELCASE',
		'TERMINAL-IF-MIXEDCASE',
		'IGNORE-EXTRA-NEWLINES',
		'GLUE-NONALPHANUMERIC-TERMINALS',
	]
specials.extend(metasymbols)

def readConfig(f):
	global debug
	cfg = ET.parse(f)
	for e in cfg.findall('*'):
		if e.tag == 'mask':
			if e.findall('terminal'):
				masked[e.findtext('token')] = e.findtext('terminal')
			elif e.findall('epsilon'):
				masked[e.findtext('token')] = 'EPSILON'
			else:
				print('Unknown masked token:',e.findtext('token'))
		elif e.tag == 'nonterminals-may-contain':
			for x in e.text:
				nonterminals_alphabet.append(x)
		elif e.tag == 'nonterminals-may-start-with':
			for x in e.text:
				nonterminals_start.append(x)
		elif e.tag == 'ignore':
			#config[e.tag] = ''
			for x in e.findall('*'):
				if x.tag == 'newline':
					ignore_tokens.append('\n')
					ignore_tokens.append('@@@0-0')
				elif x.tag == 'space':
					ignore_tokens.append(' ')
				elif x.tag == 'lines-containing':
					ignore_lines.append(x.text)
				elif x.tag == 'same-indentation':
					ignore_tokens.append('@@@1-1')
				else:
					ignore_tokens.append(x.text)
		elif e.tag == 'alias':
			for x in e.findall('*'):
				if x.tag not in aliases.keys():
					aliases[x.tag] = []
				aliases[x.tag].append(x.text)
		elif e.text:
			config[e.tag] = e.text.replace('\\n','\n')
		else:
			config[e.tag] = ''
		if e.tag in ('nonterminal-if-camelcase','nonterminal-if-mixedcase','nonterminal-if-uppercase','nonterminal-if-lowercase','nonterminal-if-contains','nonterminal-if-defined','decompose-symbols'):
			if e.text:
				config[e.tag] = e.text
			else:
				config[e.tag] = ''
			for x in e.findall('except'):
				always_terminals.append(x.text)
		if e.tag in ('terminal-if-camelcase','terminal-if-mixedcase','terminal-if-uppercase','terminal-if-lowercase','terminal-if-undefined'):
			config[e.tag] = ''
			for x in e.findall('except'):
				always_nonterminals.append(x.text)
	if debug:
		print('Ok',config)

def writeConfig(f):
	global debug
	cfg = ET.Element(slpsns.edd_('config'))
	for ms in config.keys():
		ET.SubElement(cfg,ms).text = config[ms]
		#print(ms,'processed.')
	ig = ET.Element('ignore')
	igy = False
	for i in ignore_tokens:
		if i == '\n':
			ET.SubElement(ig,'newline')
			igy = True
		elif i.find('@@@') == 0:
			pass
		else:
			print('Ignore %s?'%i)
	for i in ignore_lines:
		ET.SubElement(ig,'lines-containing').text = i
		igy = True
	if igy:
		cfg.append(ig)
	ET.ElementTree(cfg).write(f)

def quoted(x):
	return '"'+x+'"'

def grammarNameOf(x):
	if x.find('/') < 0:
		return x.split('.')[0]
	else:
		return x.split('/')[-1].split('.')[0]

if __name__ == "__main__":
	if len(sys.argv) != 6:
		print('Usage:')
		print('	     xedd.py input.xedd input.edd output.edd coupled-grammar.cbgf coupled-grammarbase.cbgf')
		sys.exit(-1)
	slpsns.init(ET)
	readConfig(sys.argv[2])
	cbgf = ET.Element(slpsns.cbgf_('relationship'))
	cbgfr = ET.Element(slpsns.cbgf_('relationship'))
	# renaming ALWAYS happens if the name of the notation is changed
	# if we do not call two different entities two different names, we cannot tell them apart
	edd1 = grammarNameOf(sys.argv[2])
	edd2 = grammarNameOf(sys.argv[3])
	for s in ('Grammar', 'Production', 'Definition', 'Symbol','Nonterminal','Terminal'):
		r = ET.SubElement(cbgf,slpsns.cbgf_('rename-rename'))
		n = ET.SubElement(r,'nonterminal')
		ET.SubElement(n,'from').text = edd1 + s
		ET.SubElement(n,'to').text = edd2 + s
	# processing commands
	for cmd in ET.parse(sys.argv[1]).findall('*'):
		if cmd.tag == slpsns.xedd_('rename'):
			ms = cmd.findtext('metasymbol')
			x = cmd.findtext('from')
			y = cmd.findtext('to')
			print('xedd:rename-metasymbol('+ms+', '+quoted(x)+', '+quoted(y)+');')
			ms += '-symbol'
			if ms in config.keys():
				if config[ms] == x:
					config[ms] = y
					r = ET.SubElement(cbgf,slpsns.cbgf_('rename-rename'))
					t = ET.SubElement(r,'terminal')
					ET.SubElement(t,'from').text = x
					ET.SubElement(t,'to').text = y
				else:
					print('Impossible to rename since %s is "%s" and not "%s" as expected!'%(ms,config[ms],x))
					sys.exit(-1)
			else:
				print('Impossible to rename since %s is not defined!'%ms)
				sys.exit(-1)
		elif cmd.tag == slpsns.xedd_('introduce'):
			ms = cmd.findtext('metasymbol')
			x = cmd.findtext('start')
			y = cmd.findtext('end')
			print('xedd:introduce-metasymbol('+ms+', '+quoted(x)+', '+quoted(y)+');')
			ms1 = 'start-'+ms+'-symbol'
			ms2 = 'end-'+ms+'-symbol'
			if ms1 in config.keys():
				print('Cannot introduce metasymbol that is already defined: %s.' % ms1)
				sys.exit(1)
			elif ms2 in config.keys():
				print('Cannot introduce metasymbol that is already defined: %s.' % ms2)
				sys.exit(1)
			else:
				config[ms1] = x
				config[ms2] = y
				if ms == 'group':
					p = ET.SubElement(cbgf,slpsns.cbgf_('add-remove'))
					p = ET.SubElement(p,'vertical')
					p = ET.SubElement(p,slpsns.bgf_('production'))
					ET.SubElement(p,'label').text = ms
					ET.SubElement(p,'nonterminal').text = edd2 + 'Symbol'
					e = ET.SubElement(p,slpsns.bgf_('expression'))
					s = ET.SubElement(e,'sequence')
					ET.SubElement(ET.SubElement(s,slpsns.bgf_('expression')),'terminal').text = x
					if 'definition-separator-symbol' in config:
						slp = ET.SubElement(ET.SubElement(s,slpsns.bgf_('expression')),'seplistplus')
						ET.SubElement(ET.SubElement(slp,slpsns.bgf_('expression')),'nonterminal').text = edd2 + 'Definition'
						ET.SubElement(ET.SubElement(slp,slpsns.bgf_('expression')),'terminal').text = config['definition-separator-symbol']
					else:
						ET.SubElement(ET.SubElement(s,slpsns.bgf_('expression')),'nonterminal').text = edd2 + 'Definition'
					ET.SubElement(ET.SubElement(s,slpsns.bgf_('expression')),'terminal').text = y
				elif ms in ('seplist-plus','seplist-star'):
					p = ET.SubElement(cbgf,slpsns.cbgf_('add-remove'))
					p = ET.SubElement(p,'vertical')
					p = ET.SubElement(p,slpsns.bgf_('production'))
					ET.SubElement(p,'label').text = ms.replace('-','')
					ET.SubElement(p,'nonterminal').text = edd2 + 'Symbol'
					e = ET.SubElement(p,slpsns.bgf_('expression'))
					s = ET.SubElement(e,'sequence')
					ET.SubElement(ET.SubElement(s,slpsns.bgf_('expression')),'terminal').text = x
					ET.SubElement(ET.SubElement(s,slpsns.bgf_('expression')),'nonterminal').text = edd2 + 'Symbol'
					ET.SubElement(ET.SubElement(s,slpsns.bgf_('expression')),'nonterminal').text = edd2 + 'Symbol'
					ET.SubElement(ET.SubElement(s,slpsns.bgf_('expression')),'terminal').text = y
				else:
					print('No coupled transformation added, please teach me how to do it first.')
						
		else:
			print(cmd.tag,'is unknown')
	writeConfig(sys.argv[3])
	print('Coupled notation grammar transformation: %i steps' % len(cbgf))
	ET.ElementTree(cbgf).write(sys.argv[4])
	print('Coupled grammarbase transformation: %i steps' % len(cbgfr))
	ET.ElementTree(cbgfr).write(sys.argv[5])

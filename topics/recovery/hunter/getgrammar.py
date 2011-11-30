#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os, sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF3
from functools import reduce

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

def isAlpha(x):
	return reduce(lambda a,b:a and (b.isalpha() or b in nonterminals_alphabet),x,True)

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

if __name__ == "__main__":
	if len(sys.argv) != 4:
		print('Usage:')
		print('	getgrammar.py input.txt config.edd output.src')
		sys.exit(-1)
	readConfig(sys.argv[2])
	# default values for some metasymbols
	if 'terminator-symbol' not in config.keys():
		config['terminator-symbol'] = ';'
		print('Terminator metasymbol not specified, we use the default value of ";".')
	if 'defining-symbol' not in config.keys():
		config['defining-symbol'] = ':'
		print('Defining metasymbol not specified, we use the default value of ":".')
	f = open(sys.argv[1],'r')
	fragment = False
	lines = []
	for line in f.readlines():
		tline = line.strip()
		if fragment:
			lines.append(line)
			if tline == config['terminator-symbol']:
				fragment = False
				#lines.append('')
			if tline == '':
				print('An empty line inside a production rule: suspect that a terminal metasymbol has been forgotten.')
				fragment = False
		else:
			if tline[-len(config['defining-symbol']):] == config['defining-symbol'] and isAlpha(tline[:-len(config['defining-symbol'])]):
				#print('Found nonterminal',tline[:-len(config['defining-symbol'])])
				lines.append(line)
				fragment = True
	f.close()
	f = open(sys.argv[3],'w')
	f.writelines(lines)
	f.close()
	print(len(lines),'lines extracted')

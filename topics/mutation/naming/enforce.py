#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os,sys
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import BGF3
import XBGF3
import metrics3
import xml.etree.ElementTree as ET

def isupper(name):
	return name.isupper()

def islower(name):
	return name.islower()

def ismixed(name):
	return name.isalpha() and nt[0].islower();

def iscamel(name):
	return name.isalpha() and nt[0].isupper();

def check(nc,name):
	if nc == 'U':
		return isupper(name)
	elif nc == 'l':
		return islower(name)
	elif nc == 'm':
		return ismixed(name)
	elif nc == 'C':
		return iscamel(name)
	else:
		print('Unknown notation:',nc)
		return False

def convert(nc1,nc2,name):
	if nc1[1] == 's':
		words = name.split()
	elif nc1[1] == '!':
		words = [name]
	else:
		words = name.split(nc1[1])
	# second attempt
	if len(words)==1 and nc1[1]=='!' and nc1[0] in ('m','C'): # nc1[0] in ('-','_','.','s'):
		words = ['']
		cx = 0
		while cx<len(name) and name!='':
			if name[cx].isupper():
				words.append(name[cx])
			else:
				words[-1] += name[cx]
			cx += 1
	nwords = map(lambda x:convertname(nc2,x),words)
	if nc2[1]=='!':
		s = ''.join(nwords)
	elif nc2[1]=='s':
		s = ' '.join(nwords)
	elif nc2[1]=='-':
		s = '-'.join(nwords)
	elif nc2[1]=='_':
		s = '_'.join(nwords)
	elif nc2[1]=='.':
		s = '.'.join(nwords)
	else:
		s = '?????'
	# a tiny distinction between mixedCase and CamelCase
	if s[0].isupper() and nc2[0]=='m':
		s[0] = s[0].lower()
	return s

def convertname(nc2,word):
	if nc2[0] == 'U':
		return word.upper()
	elif nc2[0] == 'l':
		return word.lower()
	elif nc2[0] in ('m','C'):
		return word[0].upper()+word[1:].lower()
	else:
		return '???'

if __name__ == "__main__":
	if len(sys.argv) != 5:
		print('This mutation enforces the naming convention in a given BGF grammar.')
		print('Usage:\n      '+sys.argv[0]+' <from> <to> <bgf-input> <xbgf-output>')
		print('      where <from> and <to> can be any of the following:')
		print('            U = upper case            - = dash-separated')
		print('            l = lower case            _ = underscore-separated')
		print('            m = mixed case            . = dot-separated')
		print('            C = camel case            s = space-separated')
		print('                                      ! = nonseparated')
		sys.exit(1)
	nc1 = sys.argv[1]
	nc2 = sys.argv[2]
	bgf = BGF3.Grammar()
	xbgf = XBGF3.Sequence()
	bgf.parse(sys.argv[3])
	for nt in metrics3.var(bgf):
		if not check(nc1[0],nt):
			print(nt,'does not satisfy convention',sys.argv[1])
		else:
			print(nt,'-->',convert(nc1,nc2,nt))
			s = XBGF3.Step('rename')
			n = XBGF3.Wrapping('nonterminal')
			n.addChild(XBGF3.Leaf('from',nt))
			n.addChild(XBGF3.Leaf('to',convert(nc1,nc2,nt)))
			s.addParam(n)
			xbgf.addStep(s)
	ET.ElementTree(xbgf.getXml()).write(sys.argv[4])
	sys.exit(0)

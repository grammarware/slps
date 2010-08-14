#!/usr/bin/python

import sys

basic_elements = ['e','o','i','s','a','t','n']
unary_modifiers = ['z','m','q','p','k']
binary_modifiers = ['f','c']
names_subsets = ([],['foo'],['foo','bar'])
counter = 0

def main(dir):
	global counter
	# solitary basic elements
	for x in basic_elements:
		dump_file(generate_expression(x),dir+'/'+x+'.bgf')
	print counter,'test cases with basic elements generated successfully'
	counter = 0
	# once wrapped basic elements
	for x in unary_modifiers:
		for y in basic_elements:
			dump_file(wrap_expression(x,generate_expression(y)),dir+'/'+x+y+'.bgf')
	print counter,'test cases with wrapped basic elements generated successfully'
	counter = 0
	# twice wrapped basic elements
	for x in unary_modifiers:
		for y in unary_modifiers:
			for z in basic_elements:
				dump_file(wrap_expression(x,wrap_expression(y,generate_expression(z))),dir+'/'+x+y+z+'.bgf')
	print counter,'test cases with twice wrapped basic elements generated successfully'
	counter = 0
	# sequences and choices
	for x in binary_modifiers:
		for y in basic_elements:
			for z in basic_elements:
				dump_file(wrap_expression(x,generate_expression(y)+generate_expression(z)),dir+'/'+x+y+z+'.bgf')
	print counter,'test cases with sequences and choices of length 2 generated successfully'
	counter = 0
	# longer sequences and choices
	for x in binary_modifiers:
		for a in basic_elements:
			for b in basic_elements:
				for c in basic_elements:
					dump_file(wrap_expression(x,generate_expression(a)+generate_expression(b)+generate_expression(c)),dir+'/'+x+a+b+c+'.bgf')
	print counter,'test cases with sequences and choices of length 3 generated successfully'
	counter = 0
	# nested sequences and choices
	for x in binary_modifiers:
		for y in binary_modifiers:
			for a in basic_elements:
				for b in basic_elements:
					for c in basic_elements:
						dump_file(wrap_expression(x,wrap_expression(y,generate_expression(a)+generate_expression(b))+generate_expression(c)),dir+'/'+x+y+a+b+c+'.bgf')
						dump_file(wrap_expression(x,generate_expression(a)+wrap_expression(y,generate_expression(b)+generate_expression(c))),dir+'/'+x+a+y+b+c+'.bgf')
	print counter,'test cases with sequences and choices of nested depth 2 generated successfully'
	counter = 0
	# specific cases
	for x in names_subsets:
		for y in names_subsets:
			dump_specific_file(x,y,dir+'/r'+str(len(x))+'l'+str(len(y))+'.bgf')
	print counter,'test cases with multiple labelled productions generated successfully'
	return

def dump_file(expr,fname):
	global counter
	f = open(fname,'w')
	f.write('<?xml version="1.0"?><bgf:grammar xmlns:bgf="http://planet-sl.org/bgf"><bgf:production><label>foo</label><nonterminal>bar</nonterminal>')
	f.write(expr)
	f.write('</bgf:production></bgf:grammar>')
	f.close()
	counter += 1

def dump_specific_file(roots,labels,fname):
	global counter
	f = open(fname,'w')
	f.write('<?xml version="1.0"?><bgf:grammar xmlns:bgf="http://planet-sl.org/bgf">')
	for r in roots:
		f.write('<root>'+r+'</root>')
	for l in labels:
		f.write('<bgf:production><label>l'+l+'</label><nonterminal>'+l+'</nonterminal>'+generate_expression('e')+'</bgf:production>')
	f.write('</bgf:grammar>')
	f.close()
	counter += 1

def generate_expression(code):
	# basic_elements = ['e','o','i','s','a','t','n']
	if code == 'e':
		expr = '<epsilon/>'
	elif code == 'o':
		expr = '<empty/>'
	elif code == 'i':
		expr = '<value>int</value>'
	elif code == 's':
		expr = '<value>string</value>'
	elif code == 'a':
		expr = '<any/>'
	elif code == 't':
		expr = '<terminal>x</terminal>'
	elif code == 'n':
		expr = '<nonterminal>x</nonterminal>'
	return '<bgf:expression>'+expr+'</bgf:expression>'

def wrap_expression(code,expr):
	# unary_modifiers = ['z','m','q','p','k']
	if code == 'z':
		expr = '<selectable><selector>x</selector>'+expr+'</selectable>'
	elif code == 'm':
		expr = '<marked>'+expr+'</marked>'
	elif code == 'q':
		expr = '<optional>'+expr+'</optional>'
	elif code == 'p':
		expr = '<plus>'+expr+'</plus>'
	elif code == 'k':
		expr = '<star>'+expr+'</star>'
	elif code == 'f':
		expr = '<sequence>'+expr+'</sequence>'
	elif code == 'c':
		expr = '<choice>'+expr+'</choice>'
	return '<bgf:expression>'+expr+'</bgf:expression>'

if __name__ == "__main__":
	if len(sys.argv) == 2:
		main(sys.argv[1])
	else:
		print '''BGF Test Set Generator

Usage:'''
		print ' ',sys.argv[0],'<input xldf file>','<input ldf file>','<output ldf file>'
		sys.exit(1)

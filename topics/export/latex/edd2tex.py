#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os, sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF3
from functools import reduce

if __name__ == "__main__":
	if len(sys.argv) == 1:
		print('Usage:')
		print('	edd2tex.py <input1.edd> [<input2.edd> ...]')
		sys.exit(-1)
	edds = []
	print('\\begin{center}\\begin{tabular}{|l|'+'c|'*(len(sys.argv)-1)+'}\\hline Name')
	for f in sys.argv[1:]:
		print(' & ',f.split('/')[-1][:-4])
		# print('Parsing',f.replace('.edd',''))
		eddf = ET.parse(f)
		edd = {}
		for e in eddf.findall('*'):
			if e.tag == 'ignore':
				t = ''
				for ign in e.findall('*'):
					if ign.tag == 'newline':
						t += '\\crleft'
					elif ign.tag == 'space':
						t += '\\verb*| |'
					else:
						t += ign.tag
				edd[e.tag] = t
			else:
				edd[e.tag] = e.text
		edds.append(edd)
	done = ['mask','start-grammar-symbol','end-grammar-symbol']
	print('\\\\\\hline')
	for e in edds:
		for k in e.keys():
			if k not in done:
				print(k)
				for edd in edds:
					print(' & ')
					if k in edd.keys():
						if edd[k].find('\\verb')> -1:
							print(edd[k])
						elif edd[k].find(' ')<0:
							print('\\texttt{'+edd[k].replace('}','\\}').replace('{','\\{')+'}')
						else:
							print('\\verb*|'+edd[k]+'|')
				print('\\\\')
				done.append(k)
	print('\\hline\\end{tabular}\\end{center}')


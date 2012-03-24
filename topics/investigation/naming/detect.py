#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os,sys
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import BGF3
import metrics3

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print('This tool detects the naming convention used in a given BGF grammar.')
		print('Usage:\n      '+sys.argv[0]+' <bgf-input>')
		sys.exit(1)
	bgf = BGF3.Grammar()
	bgf.parse(sys.argv[1])
	allnts = metrics3.var(bgf)
	upcase = []		# THE-EXAMPLE
	lowcase = []    # the-example
	mixedcase = []  # theExample
	camelcase = []  # TheExample
	unknown = []    #
	for nt in allnts:
		if nt.isupper():
			upcase.append(nt)
		elif nt.islower():
			lowcase.append(nt)
		elif nt.isalpha() and nt[0].islower():
			mixedcase.append(nt)
		elif nt.isalpha() and nt[0].isupper():
			camelcase.append(nt)
		else:
			print('Cannot decide on',nt)
			unknown.append(nt)
	if upcase:
		print('UPPERCASE:',upcase)
	if lowcase:
		print('lowercase:',lowcase)
	if mixedcase:
		print('mixedCase:',mixedcase)
	if camelcase:
		print('CamelCase:',camelcase)
	if unknown:
		print('Undecisinve:',unknown)
	# print(metrics3.var(bgf))
	sys.exit(0)

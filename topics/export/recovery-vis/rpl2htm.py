#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import RPL
import os, sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
# from functools import reduce

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print('This is the trivial visualiser of RPLs (recovery process logs).')
		print('Usage:')
		print('	rpl2htm.py <input-recovery-process-log> <output-hypertext>')
		sys.exit(-1)
	rpl = RPL.RPL(sys.argv[1])
	rpl.parse()
	html = open(sys.argv[2],'w')
	html.write(rpl.getHTML())
	html.close()
	sys.exit(0)
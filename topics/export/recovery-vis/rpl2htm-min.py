#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import RPL
import os, sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
# from functools import reduce

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print('Usage:')
		print('	rpl2htm.py <input-recovery-process-log> <output-hypertext>')
		sys.exit(-1)
	rpl = RPL.RPL(sys.argv[1])
	rpl.parse()
	html = open(sys.argv[2],'w')
	hypertext = '<html xmlns="http://www.w3.org/1999/xhtml" xmlns:xhtml="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>'+\
			'<title>Grammar Recovery, visualised: '+rpl.name+'</title><link href="recovery.css" rel="stylesheet" type="text/css"/></head><body>'
	for x in rpl.data:
		hypertext += x.getHTML()+'\n'
	hypertext += '<hr></body></html>'
	html.write(hypertext)
	html.close()
	sys.exit(1)
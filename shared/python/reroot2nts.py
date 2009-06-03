#!/usr/bin/python
import os
import sys
import string
import elementtree.ElementTree as ET
import slpsns

def main(bgffile,xbgffile):
 seq = ET.Element(slpsns.xbgf_('sequence'))
 rr = ET.SubElement(seq,slpsns.xbgf_('reroot'))
 for r in ET.XML('<list>'+''.join(open(bgffile,'r').readlines()).strip()+'</list>').findall('*'):
  x = ET.SubElement(rr,'root')
  x.text = r.text
 ET.ElementTree(seq).write(xbgffile)
 return

if __name__ == "__main__":
 if len(sys.argv) == 3:
  slpsns.init(ET)
  apply(main,sys.argv[1:3])
 else:
  print '''Reroot2Nonterminals generator

Usage:'''
  print ' ',sys.argv[0],'<input file>','<output file>'
  sys.exit(1)

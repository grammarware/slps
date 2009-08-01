#!/usr/bin/python
import os
import sys
import string
import slpsns
import elementtree.ElementTree as ET

def identify(e):
 if 'id' in e.attrib:
  return e.tag+'@'+e.get('id')
 else:
  return e.tag

def createCleanElementFrom(e):
 #print 'Copying element',e.tag.replace(slpsns.htm_(''),'')
 e2 = ET.Element(e.tag.replace(slpsns.htm_(''),''))
 if e.text:
  e2.text = e.text
 if e.tail:
  e2.tail = e.tail
 for a in e.attrib:
  e2.set(a,e.get(a))
  #print 'Copying attribute',a
 for se in e.findall('*'):
  e2.append(createCleanElementFrom(se))
 return e2


def repp(localpath,tree,prettyprinter):
 ending = ''
 for e in tree.findall('*'):
  tree.remove(e)
 tree.text = ''
 inputfile = 'printed_for_xldf.tmp'
 if os.system(localpath+prettyprinter+' '+localpath+tree.get('src')+' '+inputfile):
  print '[----] Failed: can''t execute the pretty-printer!'
  return
 for e in ET.parse(inputfile).findall('/*/*'):
  #for e in ET.XML('<t>'+''.join(open(inputfile,'r').readlines())+'</t>').findall('*'):
  tree.append(createCleanElementFrom(e))
 print '[++++] Re-pretty-printed.'
 return


def main(inldffile,ppfile,outldffile):
 tree = ET.parse(inldffile)
 if inldffile.find('/')<0:
  localpath = ''
 else:
  localpath = '/'.join(inldffile.split('/')[:-1])+'/'
 cx = 0
 for s in tree.findall('//sample'):
  print '[????] Found',identify(s),'of',s.get('src')
  repp(localpath,s,ppfile)
  cx += 1
 print 'Total',cx,'samples processed'
 tree.write(outldffile)
 return

if __name__ == "__main__":
 if len(sys.argv) == 4:
  apply(main,sys.argv[1:4])
 else:
  print '''Change the Pretty-Printer automation tool

Usage:'''
  print ' ',sys.argv[0],'<input ldf file>','<new samples pretty-printer>','<output ldf file>'
  sys.exit(1)

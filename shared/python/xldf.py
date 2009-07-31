#!/usr/bin/python
import os
import sys
import string
import slpsns
import elementtree.ElementTree as ET
import xldfCommands

#acceptedtags = ('{'+xsdns+'}complexType','{'+xsdns+'}element','{'+xsdns+'}simpleType','{'+xsdns+'}group')

# Unified interface
def xldf_perform_command(localpath,cmd,ltree):
 commandName = cmd.tag.replace(slpsns.xldf_(''),'')
 if commandName == 'transform-document':
  xldf_transform_document(localpath,cmd,ltree)
  return
 try:
  eval('xldfCommands.xldf_'+commandName.replace('-','_'))(localpath,cmd,ltree)
 except NameError,e:
  print '[----] Unknown XLDF command:',commandName
  print e
  sys.exit(1)

def xldf_transform_document(localpath,cmd,tree):
 print '[XLDF] transform(...,',cmd.findtext('file').split('/')[-1],') starts'
 try:
  xtree = ET.parse(localpath+cmd.findtext('file'))
 except IOError,e:
  print '[----] xldf:transform failed: file',localpath+cmd.findtext('file'),'not found'
  return
 for incmd in xtree.findall('*'):
  xldf_perform_command(localpath,incmd,tree)
 print '[XLDF] transform(...,',cmd.findtext('file').split('/')[-1],') is done'
 return

def identify(e):
 if 'id' in e.attrib:
  return e.tag+'@'+e.get('id')
 else:
  return e.tag

def normalise(tree):
 for e in tree.findall('*'):
  for e2 in e.findall('*'):
   if len(e2.findall('content'))>1:
    print '[????] In',identify(e),'/',identify(e2),'found double content!'
    first = e2.findall('content')[0]
    for e3 in e2.findall('content')[1:]:
     for e4 in e3.findall('*'):
      first.append(e4)
     e2.remove(e3)
 return

def main(xldffile,inldffile,outldffile):
 grammar={}
 if xldffile.find('/')<0:
  localpath = ''
 else:
  localpath = '/'.join(xldffile.split('/')[:-1])+'/'
 xtree = ET.parse(xldffile)
 ltree = ET.parse(inldffile)
 for cmd in xtree.findall('*'):
  xldf_perform_command(localpath,cmd,ltree)
 normalise(ltree)
 ltree.write(outldffile)
 return

if __name__ == "__main__":
 if len(sys.argv) == 4:
  apply(main,sys.argv[1:4])
 else:
  print '''LDF transformation engine

Usage:'''
  print ' ',sys.argv[0],'<input xldf file>','<input ldf file>','<output ldf file>'
  sys.exit(1)

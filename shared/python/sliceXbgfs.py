#!/usr/bin/python
import os
import sys
import string
import elementtree.ElementTree as ET
import slpsns
import slicing

def main(lcffile,outdir):
 ltree = ET.parse(lcffile)
 newLcf = ET.Element(slpsns.lcf_('configuration'))
 dirName = outdir
 if dirName[-1]!='/':
  dirName += '/'
 lcfName = dirName+lcffile.split('/')[-1]
 xbgfDir = '/'.join(lcffile.split('/')[:-1])
 if xbgfDir[-1]=='/':
  xbgfDir += 'xbgf/'
 else:
  xbgfDir += '/xbgf/'
 for el in ltree.findall('*'):
  if el.tag == 'target':
   newLcf.append(slicing.sliceTarget(xbgfDir,dirName+'xbgf/',el))
  else:
   newLcf.append(el)
 ET.ElementTree(newLcf).write(lcfName)
 return

if __name__ == "__main__":
 if len(sys.argv) == 3:
  apply(main,sys.argv[1:3])
 else:
  print '''XBGF slicing tool

Usage:'''
  print ' ',sys.argv[0],'<input lcf file>','<output directory>'
  sys.exit(1)

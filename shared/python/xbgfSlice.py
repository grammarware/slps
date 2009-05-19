#!/usr/bin/python
import os
import sys
import string
import elementtree.ElementTree as ET
import slpsns
import slicing
import metrics

def main(xbgfFile):
 xbgfDir = '/'.join(xbgfFile.split('/')[:-1])
 if xbgfDir == '':
  xbgfDir = './'
 elif xbgfDir[-1]!='/':
  xbgfDir += '/'
 print 'Slicing...'
 sliced = slicing.sliceFile(xbgfDir,xbgfDir,xbgfFile.split('/')[-1].replace('.xbgf',''))
 return

if __name__ == "__main__":
 if len(sys.argv) == 2:
  slpsns.init(ET)
  apply(main,sys.argv[1:6])
 else:
  print '''This tool takes an XBGF file and slices it.

Usage:'''
  print ' ',sys.argv[0],'<xbgf file>'
  sys.exit(1)

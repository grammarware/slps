#!/usr/bin/python
import os
import sys
import string
import elementtree.ElementTree as ET
import slpsns
import slicing
import metrics

def main(xbgfFile,bgfFile,xbgf,gdt,synch):
 xbgfDir = '/'.join(xbgfFile.split('/')[:-1])
 if xbgfDir == '':
  xbgfDir = './'
 elif xbgfDir[-1]!='/':
  xbgfDir += '/'
 print 'Slicing...'
 sliced = slicing.sliceFile(xbgfDir,xbgfDir,xbgfFile.split('/')[-1].replace('.xbgf',''))
 print 'Runnning...'
 pnm,psm = metrics.mismatches(gdt,bgfFile,synch)
 print 'Mismatches originally:',pnm,'+',psm,'...',len(sliced),'steps to go'
 cx = ''
 for p in sliced:
  run = xbgf+' '+xbgfDir+p+'.xbgf '+bgfFile+cx+' '+bgfFile+cx+'_ | grep "XBGF"'#1> /dev/null 2> /dev/null'
  if os.system(run):
   print 'Error running',p,'!'
   print run
   sys.exit(2)
  nm,sm = metrics.mismatches(gdt,bgfFile+cx+'_',synch)
  print 'Mismatches:',nm,'+',sm
  if nm>pnm:
   print 'Observed increase in name mismatches!'
  if sm>psm:
   print 'Observed increase in structural mismatches!'
  if nm+sm>pnm+psm:
   print 'The number of mismatches went up from',pnm+psm,'to',nm+sm
   sys.exit(3)
  cx += '_'
  pnm,psm=nm,sm
 print 'Cleaning up...'
 s = 'rm -f '
 for c in range(1,len(cx)):
  s += bgfFile+'_'*c+' '
 print s
 s = 'rm -f '
 for c in sliced:
  s += xbgfDir+c+'.xbgf '
 if os.system(s):
  print 'Error cleaning up sliced XBGFs!'
 return

if __name__ == "__main__":
 if len(sys.argv) == 6:
  slpsns.init(ET)
  apply(main,sys.argv[1:6])
 else:
  print '''This tool takes an XBGF file, slices it and executes stepwise, asserting the decline in mismatches.

Usage:'''
  print ' ',sys.argv[0],'<xbgf file>','<bgf file>','<transformer>','<comparator>','<synch bgf>'
  sys.exit(1)

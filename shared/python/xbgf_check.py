#!/usr/bin/python
import os
import sys
import slpsns
import slpsXPath
import elementtree.ElementTree as ET

names   = []
targets = {}
results = {}

if __name__ == "__main__":
 if len(sys.argv) != 2:
  print 'This tool generates an overview of an XBGF script.'
  print 'Usage:'
  print '      checkxbgf <file>'
  sys.exit(1)
 xbgfFile = sys.argv[1]
 xbgf = ET.parse(xbgfFile)
 print "%30s" % xbgfFile.split('.')[0]+':',
 cx = slpsXPath.noni(xbgf,slpsXPath.safexbgf) + \
      slpsXPath.nosi(xbgfFile,'BREFACTOR') + \
      slpsXPath.noPartiallySafe(xbgf), \
      slpsXPath.noni(xbgf,slpsXPath.incdecxbgf) + \
      slpsXPath.nosi(xbgfFile,'GENERALITY'), \
      slpsXPath.noni(xbgf,slpsXPath.messyxbgf) + \
      slpsXPath.nosi(xbgfFile,'REVISE') + \
      slpsXPath.noPartiallyUnsafe(xbgf)
 early = slpsXPath.nosi(xbgfFile,'KNOWNBUG'),  slpsXPath.nosi(xbgfFile,'POSTEXTR'),   slpsXPath.nosi(xbgfFile,'INITCORR')
 final = slpsXPath.nosi(xbgfFile,'EXTENSION'), slpsXPath.nosi(xbgfFile,'RELAXATION'), slpsXPath.nosi(xbgfFile,'CORRECTION')
 realSum = slpsXPath.notr(xbgf)
 print "%3i +%3i +%3i =%4i" % (cx[0],cx[1],cx[2],realSum),
 if sum(early):
  print '[',
  if early[0]:
   print 'K'+`early[0]`,
  if early[1]:
   print 'P'+`early[1]`,
  if early[2]:
   print 'I'+`early[2]`,
  print ']',
 if sum(final):
  print '{',
  if final[0]:
   print 'E'+`final[0]`,
  if final[1]:
   print 'R'+`final[1]`,
  if final[2]:
   print 'C'+`final[2]`,
  print '}',
 if sum(early)+sum(final) != cx[1]+cx[2]:
  print 'TAGERROR'
  sys.exit(1)
 if sum(cx) != realSum:
  print 'ERROR'
  sys.exit(1)
 else:
  print
  sys.exit(0)


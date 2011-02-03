#!/usr/bin/python
import os
import sys
import string
import mismatches

if __name__ == "__main__":
 if len(sys.argv) == 4:
  pnm,psm = mismatches.mismatches(sys.argv[3],sys.argv[1],sys.argv[2])
  print 'Mismatches:',pnm,'+',psm,'=',pnm+psm
 else:
  print '''This is the shortest possible version of Grammar Diff Tool.

Usage:'''
  print ' ',sys.argv[0],'<bgf1>','<bgf2>','<comparator>'
  sys.exit(1)

#!/usr/bin/python
import sys

if __name__ == "__main__":
 if len(sys.argv) == 2:
  csv = open(sys.argv[1],'r')
  old = 65535
  for line in csv.readlines():
   values = line.strip().split('\t')
   if int(values[2])>old:
    print line,
   old = int(values[2])
  csv.close()
 else:
  print '''This tool filters suspicious lines from the csv

Usage:'''
  print ' ',sys.argv[0],'<csv file>'
  sys.exit(1)

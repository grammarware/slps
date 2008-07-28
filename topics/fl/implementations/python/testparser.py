#!/usr/bin/python
import sys, parser, prettyprinter

def main(args):
 term = open (args[0],'r')
 resu = open (args[1],'w')
 try:
  resu.write(str(parser.parseProgram(' '.join(term.readlines())))+'\n')
  term.close()
  resu.close()
  print 'Parsing succeeded.'
  sys.exit(0)
 except StandardError:
  term.close()
  resu.close()
  print 'Parsing failed.'
  sys.exit(1)

if __name__ == "__main__":
 if len(sys.argv) == 3:
  main(sys.argv[1:])
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<file to parse>','<output file>'


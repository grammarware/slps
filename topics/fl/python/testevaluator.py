#!/usr/bin/env python
import sys, parser, evaluator, prettyprinter, optimizer

def main(args):
 contextf = open (args[0],'r')
 exprfile = open (args[1],'r')
 result   = args[2]
 try:
  p=parser.parseProgram(' '.join(contextf.readlines())).optimize()
  contextf.close()
 except:
  print 'Parsing of context failed.'
  exprfile.close()
  contextf.close()
  sys.exit(1)
 expr = exprfile.readline().strip().split()
 exprfile.close()
 try:
  r = apply(p[expr[0]],map(int,expr[1:]))
 except:
  print 'Parsing succeeded, evaluation failed.'
  sys.exit(2)
 if result==`r`:
  print 'Evaluation result confirmed.'
  sys.exit(0)
 else:
  print 'Evaluation succeeded with unexpected result:',r
  sys.exit(3)

if __name__ == "__main__":
 if len(sys.argv) == 4:
  main(sys.argv[1:])
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<definitions>','<expression>','<expected result>'


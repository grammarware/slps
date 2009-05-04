#!/usr/bin/python
import os
import sys
import slpsns
import slpsXPath
import elementtree.ElementTree as ET

names   = []
targets = {}
results = {}

def report(keys,key,note):
 print note,
 cx = 0
 for x in keys:
  cx += results[key][x]
  if results[key][x]:
   print '{',results[key][x],'}'
  else:
   print '{---}',
 print '{'+`cx`+'}'

if __name__ == "__main__":
 if len(sys.argv) != 4:
  print 'This tool generates an overview of a bunch of XBGF scripts.'
  print 'Usage:'
  print '      xbgfover <xbgf.xsd> <lcf> <xbgfs-path>'
  sys.exit(1)
 lcfname = sys.argv[2].split('/')[-1].split('.')[0]
 xsd = ET.parse(sys.argv[1])
 for x in xsd.findall('/*'):
  if x.attrib.has_key('name'):
   names.append(x.attrib['name'])
 lcf = ET.parse(sys.argv[2])
 for x in lcf.findall('/target'):
  name = x.findtext('name')
  targets[name] = []
  for y in x.findall('branch/*/perform'):
   targets[name].append(y.text)
 path = sys.argv[3]
 if path[-1] != '/':
  path += '/'
 for x in slpsXPath.rkeys:
  results[x] = {}
 for x in names:
  results[x] = {}
  for y in targets.keys():
   results[x][y] = 0
 for x in targets.keys():
  for y in slpsXPath.rkeys:
   results[y][x] = 0
  for y in targets[x]:
   xbgfFile = path+y+'.xbgf'
   xbgf = ET.parse(xbgfFile)
   results['LOC'][x] += slpsXPath.loc(xbgfFile)
   results['NOI'][x] += slpsXPath.nosi(xbgfFile,'ISSUE')
   results['NI~'][x] += slpsXPath.nosi(xbgfFile,'ISSUE REFACTOR')
   results['NI+'][x] += slpsXPath.nosi(xbgfFile,'EXTEND')
   results['NI!'][x] += slpsXPath.nosi(xbgfFile,'CORRECT')
   results['NI^'][x] += slpsXPath.nosi(xbgfFile,'PERMISSIVENESS')
   results['COR'][x] += slpsXPath.nosi(xbgfFile,'EXTRACTERROR')
   results['SGO'][x] += slpsXPath.countSemanticPreserving(xbgf,xbgfFile)
   results['SID'][x] += slpsXPath.countSemanticIncDec(xbgf,xbgfFile)
   results['SRE'][x] += slpsXPath.countSemanticRevising(xbgf,xbgfFile)
   results['NOX'][x] += slpsXPath.notr(xbgf)
   results['EKB'][x] += slpsXPath.nosi(xbgfFile,'KNOWNBUG')
   results['EPX'][x] += slpsXPath.nosi(xbgfFile,'POSTEXTR')
   results['EIC'][x] += slpsXPath.nosi(xbgfFile,'INITCORR')
   results['EAR'][x] += slpsXPath.nosi(xbgfFile,'KNOWNBUG')+slpsXPath.nosi(xbgfFile,'POSTEXTR')+slpsXPath.nosi(xbgfFile,'INITCORR')
   results['FEX'][x] += slpsXPath.nosi(xbgfFile,'EXTENSION')
   results['FRE'][x] += slpsXPath.nosi(xbgfFile,'RELAXATION')
   results['FCO'][x] += slpsXPath.nosi(xbgfFile,'CORRECTION')
   results['FIN'][x] += slpsXPath.nosi(xbgfFile,'EXTENSION')+slpsXPath.nosi(xbgfFile,'RELAXATION')+slpsXPath.nosi(xbgfFile,'CORRECTION')
   for z in names:
    results[z][x] += len(xbgf.findall('/'+slpsns.xbgf_(z)))
 for x in names[:]:
  used = False
  for y in targets.keys():
   if results[x][y]:
    used = True
  if not used:
   names.remove(x)
 sorted = targets.keys()[:]
 sorted.sort()
 if sorted == ['doc12','doc123','jls1','jls12','jls123','jls2','jls3']:
  sorted = ['jls1','jls2','jls3','jls12','jls123','doc12','doc123']
 elif sorted == ['abstract','concrete','java','limit','topdown']:
  sorted = ['topdown','concrete','java','abstract','limit']
 print '\\begin{tabular}{l|'+('c|'*len(targets))+'|c}'
 for x in sorted:
  print '&\\textbf{'+x+'}',
 print '&\\textbf{Total}\\\\\\hline'
 report(sorted,'LOC','\\'+lcfname+'NumberOfLines')
 print '\\hline'
 report(sorted,'NOX','\\'+lcfname+'NumberOfTransformations')
 report(sorted,'SGO','\\'+lcfname+'NumberOfRefactors')
 report(sorted,'SID','\\'+lcfname+'NumberOfGeneralises')
 report(sorted,'SRE','\\'+lcfname+'NumberOfRevisings')
 print '\\hline'
 print '\\'+lcfname+'NumberOfSteps',
 cx = 0
 for x in sorted:
  cx += len(targets[x])
  print '{',len(targets[x]),'}'
 print '{'+`cx`+'}'
 report(sorted,'NOI','\\'+lcfname+'NumberOfIssues')
 report(sorted,'COR','\\'+lcfname+'IssuesPostX')
 report(sorted,'NI!','\\'+lcfname+'IssuesCorrect')
 report(sorted,'NI+','\\'+lcfname+'IssuesExtend')
 report(sorted,'NI^','\\'+lcfname+'IssuesPermit')
 # report(sorted,'NI~','\\'+lcfname+'IssuesRefactor')
 report(sorted,'EAR','\\'+lcfname+'Early')
 report(sorted,'EKB','\\'+lcfname+'EarlyKnownBugs')
 report(sorted,'EPX','\\'+lcfname+'EarlyPostExtraction')
 report(sorted,'EIC','\\'+lcfname+'EarlyInitialCorrection')
 print '\\hline'
 report(sorted,'FIN','\\'+lcfname+'Final')
 report(sorted,'FEX','\\'+lcfname+'FinalExtension')
 report(sorted,'FRE','\\'+lcfname+'FinalRelaxation')
 report(sorted,'FCO','\\'+lcfname+'FinalCorrection')
 print '\\hline'
 print '\\end{tabular}'
 sys.exit(0)

#!/usr/local/bin/python
# -*- coding: utf-8 -*-

h = [
	'Concepts, languages, techniques',
	'Tools, libraries, functions',
	'Transformation operators'
	]

c = [
	['BGF','XBGF','ΞBGF','EXBGF','MegaL','LDF','XLDF','EDD',
	'ANF','LCI','BTF','LCF','Rascal','Prolog','XML','XSD','Factorial Language','Java Language Specification']
	,
	['ANTLR2BGF', 'AppendXBGF', 'AsFix2BTF', 'BGF2BNF', 'BGF2DMS', 'BGF2dot', 'BGF2HTML', 'BGF2Rsc', 'BGF2SDF', 'BGF2TeX', 'BGF2TXL', 'BGFlc', 'BGFover', 'BGFpp', 'BGFreformat', 'BTF2BGF', 'BTF2src', 'BuildTestSetXml', 'CBGF', 'CBGF2', 'CBGF2CBNF', 'CBGF2XBGF', 'CheckBGF', 'CheckXBGF', 'DCG2BGF', 'Ecore2BGF', 'EDD2Rsc', 'EXBGF', 'EXBGF2XBGF', 'EXBGFover', 'ExtractOver', 'FindHoriz', 'GBTF', 'GDT', 'GDTM', 'GDTS', 'HTML2BGF', 'Java2BGF', 'LCI', 'LDF2BGF', 'LDF2HTML', 'LDF2PDF', 'LDF2SET', 'LDF2TEX', 'LDinc', 'LLL2BGF', 'MergeBGF', 'NIGDT', 'NormBGF', 'PDF2BGF', 'PlotBGF', 'Py2BGF', 'REPP', 'Rsc2BGF', 'RscD2BGF', 'RscS2BGF', 'SDF2BGF', 'ShowBGF', 'ShowG', 'ShowPNF', 'ShowRootProds', 'ShowT', 'ShowX', 'ShowXBGF', 'ShowXSD', 'Spec2BGF', 'SubGrammar', 'TDT', 'TnB', 'TokenOver', 'TXL2BGF', 'Validate', 'XBGF', 'XBGF2CBGF', 'XBGF2HTML', 'XBGF2TeX', 'XBGF2XBNF', 'XBGFI', 'XBGFlc', 'XBGFover', 'XBGFS', 'XBTF', 'XLLL', 'XML2BTF', 'XSD2BGF', 'XSD2LDF']
	,
	['Abridge', 'Abstractize', 'AddH', 'AddV', 'Anonymize', 'Appear', 'Chain', 'Concretize', 'Deanonymize', 'Define', 'Designate', 'Detour', 'Deyaccify', 'Disappear', 'Distribute', 'Downgrade', 'Dump', 'Eliminate', 'Equate', 'Extract', 'Factor', 'Fold', 'Horizontal', 'Import', 'Inject', 'Inline', 'Introduce', 'LAssoc', 'Massage', 'Narrow', 'Permute', 'Project', 'RAssoc', 'Redefine', 'Remove', 'RemoveH', 'RemoveV', 'RenameT', 'RenameL', 'RenameN', 'RenameS', 'Replace', 'Undefine', 'Unfold', 'Unite', 'Unlabel', 'Upgrade', 'Vertical', 'Widen', 'Yaccify']
	]

l = len(h)
w = [max(len(h[i])+4,max(map(len,c[i]))) for i in range(0,l)]
c = map(sorted,c)
d = max(map(len,c))

f = open('texts/Home.md','w')
f.write('|**'+'**|**'.join(h)+'**|\n')
f.write('|'+'|'.join(['-'*v for v in w])+'|\n')
for i in range(0,l):
	while(len(c[i])<d): c[i].append('')
f.write('\n'.join(['|'+'|'.join([c[i][j] and ('[['+c[i][j]+']]').center(w[i]) or ''.center(w[i]) for i in range(0,l)])+'|' for j in range(0,d)]))
f.close()

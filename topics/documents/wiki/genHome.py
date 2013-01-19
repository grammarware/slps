#!/usr/local/bin/python
# -*- coding: utf-8 -*-

h = [
	'Concepts, languages, techniques',
	'Tools, libraries, functions',
	'Transformation operators',
	'Grammar mutations'
	]

c = [
	['BGF','XBGF','ΞBGF','EXBGF','MegaL','LDF','XLDF','EDD',
	'ANF|Abstract Normal Form','LCI','BTF','LCF','Rascal','Prolog','XML','XSD','Factorial Language','Java Language Specification']
	,
	['ANTLR2BGF', 'AppendXBGF', 'AsFix2BTF', 'BGF2BNF', 'BGF2DMS', 'BGF2dot', 'BGF2HTML', 'BGF2Rascal', 'BGF2SDF', 'BGF2TeX', 'BGF2TXL', 'BGFlc', 'BGFover', 'BGFpp', 'BGFreformat', 'BTF2BGF', 'BTF2src', 'BuildTestSetXml', 'CBGF', 'CBGF2', 'CBGF2CBNF', 'CBGF2XBGF', 'CheckBGF', 'CheckXBGF', 'DCG2BGF', 'Ecore2BGF', 'EDD2Rascal', 'EXBGF', 'EXBGF2XBGF', 'EXBGFover', 'ExtractOver', 'FindH', 'GBTF', 'GDT|Grammar Diff Tool', 'HTML2BGF', 'Java2BGF', 'LCI', 'LDF2BGF', 'LDF2HTML', 'LDF2PDF', 'LDF2SET', 'LDF2TEX', 'LDinc', 'LLL2BGF', 'MergeBGF', 'NIGDT', 'NormBGF', 'PDF2BGF', 'PlotBGF', 'Py2BGF', 'REPP', 'Rascal2BGF', 'RascalDataType2BGF', 'RascalSyntax2BGF', 'SDF2BGF', 'ShowBGF', 'ShowG', 'ShowPNF', 'ShowRootProds', 'ShowT', 'ShowX', 'ShowXBGF', 'ShowXSD', 'Spec2BGF', 'TDT', 'TnB', 'TokenOverview', 'TXL2BGF', 'Validate', 'XBGF', 'XBGF2CBGF', 'XBGF2HTML', 'XBGF2TeX', 'XBGF2XBNF', 'XBGFinfo', 'XBGFlc', 'XBGFover', 'XBGFsliced', 'XBTF', 'XLLL', 'XML2BTF', 'XSD2BGF', 'XSD2LDF']
	,
	['abridge', 'abstractize', 'addH', 'addV', 'anonymize', 'appear', 'chain', 'concretize', 'deanonymize', 'define', 'designate', 'detour', 'deyaccify', 'disappear', 'distribute', 'downgrade', 'dump', 'eliminate', 'equate', 'extract', 'factor', 'fold', 'horizontal', 'importG', 'inject', 'inline', 'introduce', 'lassoc', 'massage', 'narrow', 'permute', 'project', 'rassoc', 'redefine', 'remove', 'removeH', 'removeV', 'renameT', 'renameL', 'renameN', 'renameS', 'replace', 'undefine', 'unfold', 'unite', 'unlabel', 'upgrade', 'vertical', 'widen', 'yaccify','bypass','concatT','splitT','splitN']
	,
	['RetireTs', 'RetireSs', 'RetireLs', 'RenameAll', 'Reroot2top', 'Retire top', 'SubGrammar', 'VerticalAll', 'HorizontalAll', 'DistributeAll', 'VerticalDistributeAll', 'AllDeyacc', 'Retire lazy', 'ANF|Abstract Normal Form', 'FoldG', 'Retire seplists', 'Retire iterations', 'UnchainAll']
	]

l = len(h)
w = [4+max(len(h[i]),max(map(len,c[i]))) for i in range(0,l)]
c = map(sorted,c)
d = max(map(len,c))

f = open('texts/Home.md','w')
f.write('|**'+'**|**'.join(h)+'**|\n')
f.write('|'+'|'.join(['-'*v for v in w])+'|\n')
for i in range(0,l):
	while(len(c[i])<d): c[i].append('')
f.write('\n'.join(['|'+'|'.join([c[i][j] and ('[['+c[i][j]+']]').center(w[i]) or ''.center(w[i]) for i in range(0,l)])+'|' for j in range(0,d)]))
f.close()

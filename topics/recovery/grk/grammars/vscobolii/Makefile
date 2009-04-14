# Prolog programs for the several phases
# pl	  = pl -T12345 -L12345 -G23456 -f
pl	  = swipl -f
prepare   = ${pl} tools/prepare.pl                --
extract   = ${pl} tools/extract.pl                --
inline	  = ${pl} tools/inline.pl                 --
parser    = ${pl} tools/parser.pl 	          --
dia2ast	  = ${pl} ../../tools/dia2ast/main.pl     --
many2one  = ${pl} ../../tools/dia2ast/many2one.pl --
fst 	  = ${pl} ../../tools/fst/main.pl         --
ast2ebnf  = ${pl} ../../tools/ast2ebnf.pl         --
ast2lll   = ${pl} ../../tools/ast2lll.pl          --
ast2dcg	  = ${pl} ../../tools/ast2dcg.pl          --
ast2dia   = ${pl} ../../tools/ast2dia/main.pl     --
reduced   = ${pl} ../../tools/reduced.pl          --
nop       = ${pl} ../../tools/nop.pl              --

# Some convenient shortcuts
r	  = recovery
d	  = deployment
p	  = ibm-prepared
t	  = ibm-transformed

# Main target to produce improved document and parser
all:	tag.inline tag.dcg tag.lll

# Test derived Cobol parser
test:
	make tag.inline
	rm -f test.ast
	make test.ast

${p}.html:	ibm.html \
		README.${p} \
		tools/prepare.pl \
		tools/headers.pl \
		tools/fragrefs.pl \
		tools/skip.pl \
		tools/replace.pl
	${prepare} ibm.html ${p}.html README.${p}

# Extract all diagrams and store remaining skeleton
tag.extract:	${p}.html \
		tools/extract.pl \
		tools/headers.pl
	rm -rf scratch
	mkdir scratch
	${extract} ${p}.html scratch/${p}.skeleton scratch
	touch tag.extract

# Parse all extracted diagrams
tag.dia2ast: tag.extract
	make dia2ast-worker1
	make dia2ast-worker2
	touch tag.dia2ast

# Workers for target tag.dia2ast
dia2ast-worker1:
	make ${patsubst %.dia, %.in, ${wildcard scratch/*.dia}}

dia2ast-worker2:
	${many2one} ${wildcard scratch/*.in} > scratch/${t}.init

# Apply transformations to the grammar's `leafs'
scratch/${t}.leafs: tag.dia2ast ${r}/leafs.fst
	${fst} ${r}/leafs.fst scratch/${t}.init $@

# Apply transformations to the grammar's `root'
scratch/${t}.root: scratch/${t}.leafs ${r}/root.fst
	${fst} ${r}/root.fst scratch/${t}.leafs $@

# Apply transformations in the ENVIRONMENT DIVISION
scratch/${t}.ed: scratch/${t}.root ${r}/ed.fst
	${fst} ${r}/ed.fst scratch/${t}.root $@

# Apply transformations to file description entries
scratch/${t}.fd-entries: scratch/${t}.ed ${r}/fd-entries.fst
	${fst} ${r}/fd-entries.fst scratch/${t}.ed $@

# Apply transformations to data description entries
scratch/${t}.dd-entries: scratch/${t}.fd-entries ${r}/dd-entries.fst
	${fst} ${r}/dd-entries.fst scratch/${t}.fd-entries $@

# Apply transformations to statement syntax
scratch/${t}.stats: scratch/${t}.dd-entries ${r}/stats.fst
	${fst} ${r}/stats.fst scratch/${t}.dd-entries $@

# Extract an EBNF from the corrected/completed grammar
${t}.ebnf: scratch/${t}.stats
	${ast2ebnf} $< $@ scratch/${t}.mods scratch out

# Pretty-print all the new diagrams
tag.pp: ${t}.ebnf
	make pp-worker
	touch tag.pp

# Extra target needed for late binding of wildcard
pp-worker:
	make ${patsubst %.out, %.pp, ${wildcard scratch/*.out}}

# Inline new diagrams into skeleton
tag.inline: tag.pp
	${inline} 	scratch/${p}.skeleton \
			scratch/${t}.mods \
			${t}.html \
			scratch \
			README.${t}
	touch tag.inline

# Apply transformations to enable liberal syntax
scratch/${t}.liberal: scratch/${t}.stats ${d}/liberal.fst
	${fst} ${d}/liberal.fst scratch/${t}.stats $@

# Apply transformations to establish (nested) statement sequences
scratch/${t}.seq: scratch/${t}.liberal ${d}/seq.fst
	${fst} ${d}/seq.fst scratch/${t}.liberal $@

# Apply transformations for parsing COPY statements
scratch/${t}.copy: scratch/${t}.seq ${d}/copy.fst
	${fst} ${d}/copy.fst scratch/${t}.seq $@

# Apply transformations to obtain a reduced grammar
scratch/${t}.reduce: scratch/${t}.copy ${d}/reduce.fst
	${fst} ${d}/reduce.fst scratch/${t}.copy $@

# Apply transformations to appeal removal of permutation phrases
scratch/${t}.permute: scratch/${t}.reduce ${d}/permute.fst
	${fst} ${d}/permute.fst scratch/${t}.reduce $@

# Eliminate permutation phrases
scratch/${t}.nop: scratch/${t}.permute
	${nop} $< $@

# Apply transformations to appeal to DCG model of parsing
scratch/${t}.dcg: scratch/${t}.nop ${d}/dcg.fst
	${fst} ${d}/dcg.fst scratch/${t}.nop $@

# Apply transformations to appeal to trivial FLEX scanner used for BTYACC
scratch/${t}.flex: scratch/${t}.nop ${d}/flex.fst
	${fst} ${d}/flex.fst scratch/${t}.nop $@

# Extract a DCG from the corrected/completed grammar
tag.dcg: scratch/${t}.dcg tag.reduced
	${ast2dcg} $< ${t}.pl
	touch tag.dcg

# Extract a LLL grammar for use in BTYACC via GDK
tag.lll: scratch/${t}.flex tag.reduced
	${ast2lll} scratch/${t}.flex ${t}.lll
	touch tag.lll

tag.reduced: scratch/${t}.reduce
	${reduced} $< \
		cobol-source-program \
		alphabetic-user-defined-word \
		character-string \
		cobol-word \
		comment-entry \
		dbcs \
		integer \
		level-number \
		nonnumeric \
		numeric \
		priority-number \
		quoted-pseudo-text \
		system-name \
		user-defined-word
	touch tag.reduced

clean:
	rm -rf *~
	rm -rf output.*
	rm -rf tag.*
	rm -rf scratch
	rm -rf *.ast
	rm -rf .xpce
	rm -rf core
	rm -rf ${p}.*
	rm -rf ${t}.*
	(cd figures; make clean)
	(cd recovery; make clean)
	(cd deployment; make clean)
	(cd tools; make clean)
	(cd btyacc; make clean)

%.in:	%.dia
	${dia2ast} "$<" "$@"

%.pp:	%.out
	${ast2dia} "$<" "$@"

%.ast: %.cbl tag.dcg
	${parser} ${t}.pl $<
	touch $@

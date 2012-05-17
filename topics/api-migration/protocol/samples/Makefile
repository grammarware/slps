
# NB: this Makefile (or more precisely, the scripts used here) assume
# the ASF+SDF Meta-Environment tools are in the PATH.
# Customize variables below to use this Makefile.

# You might have to customize the following two locations.
# For soot: download the complete, all-inclusive tar.gz

# Soot installation directory
SOOT = ...

# Directory containing Java system classes (see soot.sh for how it's used)
SYS = /System/Library/Frameworks/JavaVM.framework/Versions/1.5.0/Classes


# Classpath entries for our API scenarios (+ dependencies).
SRC = ../../benchmarks/src/
LIB = ../../../benchmarks/deps

# The output dir used by Soot
SOOT_OUTPUT = ./output

# The qualified types that are input to Soot
JDOM = org.planet_sl.apimigration.benchmarksjdom.ContactsToDocumentUsingJDOM
DOM = org.planet_sl.apimigration.benchmarks.dom.ContactsToDocumentUsingDOM

# The sourcefiles belonging to the types
JDOM_SRC=${SRC}/$(subst .,/,${JDOM}).java
DOM_SRC=${SRC}/$(subst .,/,${DOM}).java

all: jdom.parsed dom.parsed jdom2dom

# Migrate a small example
jdom2dom: jdom.annot jdom.mapping
	../framework/migrate.sh $^

# Generation of coupled trace
%.mapping: %.trace
	../xo-jdom2dom.sh $< | ../framework/term2trace.sh > $@

# Generation of DOM trace equivalent to JDOM trace
%.mapped: %.trace
	./xo-jdom2dom-mapped.sh $< | ../framework/term2trace.sh > $@

# Parse a trace over DCG grammar
%.parsed: %.trace
	../framework/parse.sh $*.grammar $< > $@

# Annotate Jimple statements with symbolic memory
# NB: works only for makeDocument methods.
%.annot: %.jimple %.patterns
	../framework/annotate.sh makeDocument $^ > $@

# Extract a symbolic trace from Jimple code
# NB: works only for makeDocument methods.
%.trace: %.jimple %.patterns
	../framework/trace.sh makeDocument $^ > $@


# Create Jimple files
dom.jimple: ${DOM_SRC}
	../framework/soot.sh ${SYS} ${SOOT} ${SRC} ${LIB} ${DOM} ${SOOT_OUTPUT}
	cp ${SOOT_OUTPUT}/${DOM}.jimple $@


jdom.jimple: ${JDOM_SRC}
	../framework/soot.sh ${SYS} ${SOOT} ${SRC} ${LIB} ${JDOM} ${SOOT_OUTPUT}
	cp ${SOOT_OUTPUT}/${JDOM}.jimple $@

# Remove anything that's generated.
clean:
	rm -rf ${SOOT_OUTPUT}
	rm -rf *.{annot,trace,jimple,parsed,mapping,mapped}

.PRECIOUS: %.trace


# Required tools and files
lllexport = lllexport # GDK version 1.4.2 tool for .lll -> .y deployment
flex	  = flex      # Tested with version 2.5.4
btyacc    = btyacc    # Tested with version 2.1
cc	  = gcc	      # Tested with version 2.96
libs	  = ${shell gdk-config --libs}   # -L ...
cflags	  = ${shell gdk-config --cflags} # -I ...

# Build the parser
all:
	(cd ..; make)
	make tag.y 
	make libVscii.a
	make a.out

# Test the parser
test: all
	./a.out < ../test.cbl

# Turn .lll into .y
tag.y: ../tag.lll
	${lllexport} -p Vscii -q Vscii -f yacc ../ibm-transformed.lll
	cp edited-VsciiScan.l VsciiScan.l
	touch tag.y

# Give preference to customised scanner
VsciiScan.l: edited-VsciiScan.l
	cp $< $@

# Apply FLEX
VsciiScan.c: VsciiScan.l
	flex -o$@ VsciiScan.l

# Apply BTYACC
VsciiParse.c: VsciiParse.y
	btyacc $<
	mv y_tab.c $@

# C-compile generated parser
VsciiParse.o: VsciiParse.c
	${cc} -c $< -o $@ ${cflags}

# Make parser library
libVscii.a: Vscii.o VsciiFuns.o VsciiScan.o VsciiParse.o
	ar cru libVscii.a Vscii.o VsciiFuns.o VsciiScan.o VsciiParse.o
	ranlib libVscii.a

# C-compile and link test parser
a.out: main.c libVscii.a
	${cc} main.c ${cflags} -L. ${libs} -lVscii -lsdf -lfst -llll -ltt -lgcnot

# Removed all generated and object modules
clean:
	@rm -f *~
	@rm -f tag.*
	@rm -f VsciiFuns.*
	@rm -f VsciiParse.*
	@rm -f VsciiScan.*
	@rm -f Vscii.o
	@rm -f *.a
	@rm -f a.out

# C-compile with special flags
%.o:%.c
	${cc} -c -o $@ $< ${cflags}


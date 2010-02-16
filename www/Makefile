all:

upload:
	scp index.html rlaemmel,slps@web.sourceforge.net:htdocs

sync:
	rsync -avz --exclude=".svn" --exclude="Makefile" * sspider,slps@web.sourceforge.net:htdocs

clean:
	rm -f *~

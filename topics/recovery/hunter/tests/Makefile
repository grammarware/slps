test:
	echo 'Run make test-hunter to test Grammar Hunter or make test-edd to test EDD pretty-printer.'

test-hunter:
	ls -1 *.src | xargs -n1 -I _ basename _ .src | xargs -n1 ./testperform

test-edd:
	ls -1 *.src | grep -v 'nopp' | xargs -n1 -I _ basename _ .src | xargs -n1 ./testperformedd

clean:
	rm -f *.out *.log *.gdt

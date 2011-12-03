test:
	ls -1 tests/*.bgf | xargs -n1  ../../shared/tools/checkxml bgf
	../../shared/tools/gdt tests/foobar.pretty.bgf tests/foobar.ugly.bgf
	! ../../shared/tools/gdt tests/foobar.pretty.bgf tests/foobar.term.bgf
	../../shared/tools/gdt tests/foo.b.a.r.bgf tests/foo.b.r.a.bgf
	../../shared/tools/gdt tests/foo.b.a.r.bgf tests/foo.r.b.a.bgf
	../../shared/tools/nigdt tests/foobarwez.bgf tests/xyz.bgf
	! ../../shared/tools/nigdt tests/foobarwez.bgf tests/xyz.humbug.bgf

clean:
	rm -f *.pyc *~

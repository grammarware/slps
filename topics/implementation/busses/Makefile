all:
	dot -Tpdf Koblenz.dot > Koblenz.pdf

curate:
	perl -pi -w -e 's/ä/&auml;/g;' Koblenz.dot
	perl -pi -w -e 's/ü/&uuml;/g;' Koblenz.dot
	perl -pi -w -e 's/ö/&ouml;/g;' Koblenz.dot
	perl -pi -w -e 's/ß/&szlig;/g;' Koblenz.dot
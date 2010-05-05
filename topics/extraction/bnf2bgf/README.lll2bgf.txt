This extractor is very straightforward and maps any grammar written in
LLL format (used by Grammar Deployment kit) to the BNF-like Grammar
Format.

Since LLL is more powerful than the current BGF - for instance, it allows
to specify separator sequences explicitly - some replacements are done on
the fly. Therefore, the resulting grammar is equivalent to the input one,
but might not be identical.

GDK URI: http://gdk.sourceforge.net/
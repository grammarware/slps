
#include <stdio.h>
#include <sys/types.h>

#include "ttterm.h"
#include "ttbuffer.h"
#include "ttutils.h"
#include "ttparse.h"

#include "VsciiFuns.h"
#include "VsciiScan.h"
#include "VsciiParse.h"

#include "Vscii.h"

#define BUFSZ 1024

extern void yyerror (char *s);
extern TTterm TTparseVscii_cobol_source_program(void);
extern void yy_scan_buffer(const char * base, size_t size);


void
yyerror (char *s)
{
}

TTterm VSCIIparse(TTbuf buf)
{
	Vsciiinit();
	yy_scan_buffer(buf->data, (size_t)(buf->size + 2));
	return TTparse_buffer(TTparseVscii_cobol_source_program, buf);
}


#include <stdio.h>

#include "ttterm.h"
#include "ttbuffer.h"
#include "ttutils.h"

#include "Vscii.h"

int
main(int argc, char **argv)
{
	TTbuf buf;
	TTterm pt;

	buf = TTbuffer_create();
	TTbuffer_read(buf, stdin);
	pt = VSCIIparse(buf);
	TTbuffer_destroy(buf);

	buf = TTbuffer_create();
	TTunparse(buf, pt);
	TTbuffer_write(buf, stdout);
	TTbuffer_destroy(buf);

	return 0;
}




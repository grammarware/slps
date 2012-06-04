@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::Width

import syntax::BGF;

// narrow-equivalence (the reverse, widen-equivalence, is hereby also implicitly defined)
bool narrowing(anything(),_) = true;
bool narrowing(star(e),plus(e)) = true;
bool narrowing(star(e),optional(e)) = true;
bool narrowing(star(e),e) = true;
bool narrowing(plus(e),e) = true;
bool narrowing(optional(e),e) = true;
default bool narrowing(_,_) = false;

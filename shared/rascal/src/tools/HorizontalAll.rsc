@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{HorizontalAll}
module tools::HorizontalAll

import lib::Rascalware;
import mutate::type1::HorizontalAll;
import language::BGF;
import io::ReadBGF;
import io::WriteBGF;

public void main(list[str] as)
{
	writeBGF(mutate::type1::HorizontalAll::HorizontalAll(readBGF(|cwd:///|+as[0])), |cwd:///|+as[1]);
}


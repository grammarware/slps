@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{NormBGF}
module tools::NormBGF

//import language::BGF;
// needed for levels 2+
//import lib::Rascalware;
//import analyse::Metrics;
//import language::XBGF;
//import transform::XBGF;

import normal::BGF;
import io::ReadBGF;
import io::WriteBGF;

public void main(list[str] as)
{
	writeBGF(normalise(readBGF(|cwd:///|+as[0])), |cwd:///|+as[1]);
}
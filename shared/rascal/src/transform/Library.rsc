@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{XBGF}
module transform::Library

extend transform::library::Associativity; // assoc, iterate
extend transform::library::Brutal; // replace
extend transform::library::Chaining; // abridge, detour, chain, unchain
extend transform::library::Factoring; // factor, distribute
extend transform::library::Folding; // fold, unfold, extract, inline, downgrade, upgrade
extend transform::library::Conjunction; // addC
extend transform::library::Disjunction; // addH, removeH, vertical, horizontal
extend transform::library::Labels; // renameL, unlabel, designate; renameS, anonymize, deanonimize
extend transform::library::Massage; // massage
extend transform::library::Nonterminals; // renameN, equate, splitN, clone, reroot, unite
extend transform::library::Productions; // addV, removeV, define, undefine, redefine, eliminate, introduce, import
extend transform::library::Sequential; // appear, disappear, inject, permute, project
extend transform::library::Terminals; // renameT, splitT, concatT, abstractize, concretize
extend transform::library::Width; // narrow, widen
extend transform::library::Yacc; // yaccify, deyaccify
extend transform::library::Util;

%{
#include "ttterm.h"
#include "ttbuffer.h"
#include "ttutils.h"
#include "ttparse.h"
#include "tterror.h"
#include "VsciiFuns.h"
#include "VsciiScan.h"

#define BUFSZ 1024
static char st_buf[BUFSZ];

static void build_token(void)
{
	yylval = TTparse_word((size_t)yyleng);
}

%}
%x PIC
%option noyywrap
WS [ \t\n]
COM (%.*\n)
PP "EJECT"|"SKIP1"|"SKIP2"|"SKIP3"
%%

({WS}|{COM}|(","{WS})|{PP})+ { build_token(); return FLEX_LAYOUT; }

"(" { build_token(); return '('; }
")" { build_token(); return ')'; }
"*" { build_token(); return '*'; }
"**" { build_token(); return FLEX_T_QMULTIPLY_QMULTIPLY; }
"+" { build_token(); return '+'; }
"-" { build_token(); return '-'; }
"." { build_token(); return '.'; }
"/" { build_token(); return '/'; }
":" { build_token(); return ':'; }
"<" { build_token(); return '<'; }
"<=" { build_token(); return FLEX_T_QLT_QEQUAL; }
"=" { build_token(); return '='; }
">" { build_token(); return '>'; }
">=" { build_token(); return FLEX_T_QGT_QEQUAL; }
"ACCEPT" { build_token(); return FLEX_T_ACCEPT; }
"ACCESS" { build_token(); return FLEX_T_ACCESS; }
"ADD" { build_token(); return FLEX_T_ADD; }
"ADDRESS" { build_token(); return FLEX_T_ADDRESS; }
"ADVANCING" { build_token(); return FLEX_T_ADVANCING; }
"AFTER" { build_token(); return FLEX_T_AFTER; }
"ALL" { build_token(); return FLEX_T_ALL; }
"ALPHABET" { build_token(); return FLEX_T_ALPHABET; }
"ALPHABETIC" { build_token(); return FLEX_T_ALPHABETIC; }
"ALPHABETIC-LOWER" { build_token(); return FLEX_T_ALPHABETIC_LOWER; }
"ALPHABETIC-UPPER" { build_token(); return FLEX_T_ALPHABETIC_UPPER; }
"ALPHANUMERIC" { build_token(); return FLEX_T_ALPHANUMERIC; }
"ALPHANUMERIC-EDITED" { build_token(); return FLEX_T_ALPHANUMERIC_EDITED; }
"ALSO" { build_token(); return FLEX_T_ALSO; }
"ALTER" { build_token(); return FLEX_T_ALTER; }
"ALTERNATE" { build_token(); return FLEX_T_ALTERNATE; }
"AND" { build_token(); return FLEX_T_AND; }
"ANY" { build_token(); return FLEX_T_ANY; }
"APPLY" { build_token(); return FLEX_T_APPLY; }
"ARE" { build_token(); return FLEX_T_ARE; }
"AREA" { build_token(); return FLEX_T_AREA; }
"AREAS" { build_token(); return FLEX_T_AREAS; }
"ASCENDING" { build_token(); return FLEX_T_ASCENDING; }
"ASSIGN" { build_token(); return FLEX_T_ASSIGN; }
"AT" { build_token(); return FLEX_T_AT; }
"AUTHOR" { build_token(); return FLEX_T_AUTHOR; }
"BEFORE" { build_token(); return FLEX_T_BEFORE; }
"BEGINNING" { build_token(); return FLEX_T_BEGINNING; }
"BINARY" { build_token(); return FLEX_T_BINARY; }
"BLANK" { build_token(); return FLEX_T_BLANK; }
"BLOCK" { build_token(); return FLEX_T_BLOCK; }
"BOTTOM" { build_token(); return FLEX_T_BOTTOM; }
"BY" { build_token(); return FLEX_T_BY; }
"CALL" { build_token(); return FLEX_T_CALL; }
"CANCEL" { build_token(); return FLEX_T_CANCEL; }
"CHARACTER" { build_token(); return FLEX_T_CHARACTER; }
"CHARACTERS" { build_token(); return FLEX_T_CHARACTERS; }
"CLASS" { build_token(); return FLEX_T_CLASS; }
"CLOSE" { build_token(); return FLEX_T_CLOSE; }
"CODE-SET" { build_token(); return FLEX_T_CODE_SET; }
"COLLATING" { build_token(); return FLEX_T_COLLATING; }
"COMMA" { build_token(); return FLEX_T_COMMA; }
"COMMON" { build_token(); return FLEX_T_COMMON; }
"COMP" { build_token(); return FLEX_T_COMP; }
"COMP-1" { build_token(); return FLEX_T_COMP_1; }
"COMP-2" { build_token(); return FLEX_T_COMP_2; }
"COMP-3" { build_token(); return FLEX_T_COMP_3; }
"COMP-4" { build_token(); return FLEX_T_COMP_4; }
"COMPUTATIONAL" { build_token(); return FLEX_T_COMPUTATIONAL; }
"COMPUTATIONAL-1" { build_token(); return FLEX_T_COMPUTATIONAL_1; }
"COMPUTATIONAL-2" { build_token(); return FLEX_T_COMPUTATIONAL_2; }
"COMPUTATIONAL-3" { build_token(); return FLEX_T_COMPUTATIONAL_3; }
"COMPUTATIONAL-4" { build_token(); return FLEX_T_COMPUTATIONAL_4; }
"COMPUTE" { build_token(); return FLEX_T_COMPUTE; }
"CONFIGURATION" { build_token(); return FLEX_T_CONFIGURATION; }
"CONTAINS" { build_token(); return FLEX_T_CONTAINS; }
"CONTENT" { build_token(); return FLEX_T_CONTENT; }
"CONTINUE" { build_token(); return FLEX_T_CONTINUE; }
"CONVERTING" { build_token(); return FLEX_T_CONVERTING; }
"COPY" { build_token(); return FLEX_T_COPY; }
"CORR" { build_token(); return FLEX_T_CORR; }
"CORRESPONDING" { build_token(); return FLEX_T_CORRESPONDING; }
"COUNT" { build_token(); return FLEX_T_COUNT; }
"CURRENCY" { build_token(); return FLEX_T_CURRENCY; }
"DATA" { build_token(); return FLEX_T_DATA; }
"DATE" { build_token(); return FLEX_T_DATE; }
"DATE-COMPILED" { build_token(); return FLEX_T_DATE_COMPILED; }
"DATE-WRITTEN" { build_token(); return FLEX_T_DATE_WRITTEN; }
"DAY" { build_token(); return FLEX_T_DAY; }
"DAY-OF-WEEK" { build_token(); return FLEX_T_DAY_OF_WEEK; }
"DBCS" { build_token(); return FLEX_T_DBCS; }
"DEBUG-ITEM" { build_token(); return FLEX_T_DEBUG_ITEM; }
"DEBUGGING" { build_token(); return FLEX_T_DEBUGGING; }
"DECIMAL-POINT" { build_token(); return FLEX_T_DECIMAL_POINT; }
"DECLARATIVES" { build_token(); return FLEX_T_DECLARATIVES; }
"DELETE" { build_token(); return FLEX_T_DELETE; }
"DELIMITED" { build_token(); return FLEX_T_DELIMITED; }
"DELIMITER" { build_token(); return FLEX_T_DELIMITER; }
"DEPENDING" { build_token(); return FLEX_T_DEPENDING; }
"DESCENDING" { build_token(); return FLEX_T_DESCENDING; }
"DISPLAY" { build_token(); return FLEX_T_DISPLAY; }
"DISPLAY-1" { build_token(); return FLEX_T_DISPLAY_1; }
"DIVIDE" { build_token(); return FLEX_T_DIVIDE; }
"DIVISION" { build_token(); return FLEX_T_DIVISION; }
"DOWN" { build_token(); return FLEX_T_DOWN; }
"DUPLICATES" { build_token(); return FLEX_T_DUPLICATES; }
"DYNAMIC" { build_token(); return FLEX_T_DYNAMIC; }
"EBCDIC" { build_token(); return FLEX_T_EBCDIC; }
"EGCS" { build_token(); return FLEX_T_EGCS; }
"ELSE" { build_token(); return FLEX_T_ELSE; }
"END" { build_token(); return FLEX_T_END; }
"END-ADD" { build_token(); return FLEX_T_END_ADD; }
"END-CALL" { build_token(); return FLEX_T_END_CALL; }
"END-COMPUTE" { build_token(); return FLEX_T_END_COMPUTE; }
"END-DELETE" { build_token(); return FLEX_T_END_DELETE; }
"END-DIVIDE" { build_token(); return FLEX_T_END_DIVIDE; }
"END-EVALUATE" { build_token(); return FLEX_T_END_EVALUATE; }
"END-IF" { build_token(); return FLEX_T_END_IF; }
"END-MULTIPLY" { build_token(); return FLEX_T_END_MULTIPLY; }
"END-OF-PAGE" { build_token(); return FLEX_T_END_OF_PAGE; }
"END-PERFORM" { build_token(); return FLEX_T_END_PERFORM; }
"END-READ" { build_token(); return FLEX_T_END_READ; }
"END-RETURN" { build_token(); return FLEX_T_END_RETURN; }
"END-REWRITE" { build_token(); return FLEX_T_END_REWRITE; }
"END-SEARCH" { build_token(); return FLEX_T_END_SEARCH; }
"END-START" { build_token(); return FLEX_T_END_START; }
"END-STRING" { build_token(); return FLEX_T_END_STRING; }
"END-SUBTRACT" { build_token(); return FLEX_T_END_SUBTRACT; }
"END-UNSTRING" { build_token(); return FLEX_T_END_UNSTRING; }
"END-WRITE" { build_token(); return FLEX_T_END_WRITE; }
"ENDING" { build_token(); return FLEX_T_ENDING; }
"ENTRY" { build_token(); return FLEX_T_ENTRY; }
"ENVIRONMENT" { build_token(); return FLEX_T_ENVIRONMENT; }
"EOP" { build_token(); return FLEX_T_EOP; }
"EQUAL" { build_token(); return FLEX_T_EQUAL; }
"ERROR" { build_token(); return FLEX_T_ERROR; }
"EVALUATE" { build_token(); return FLEX_T_EVALUATE; }
"EVERY" { build_token(); return FLEX_T_EVERY; }
"EXAMINE" { build_token(); return FLEX_T_EXAMINE; }
"EXCEPTION" { build_token(); return FLEX_T_EXCEPTION; }
"EXIT" { build_token(); return FLEX_T_EXIT; }
"EXTEND" { build_token(); return FLEX_T_EXTEND; }
"EXTERNAL" { build_token(); return FLEX_T_EXTERNAL; }
"FALSE" { build_token(); return FLEX_T_FALSE; }
"FD" { build_token(); return FLEX_T_FD; }
"FILE" { build_token(); return FLEX_T_FILE; }
"FILE-CONTROL" { build_token(); return FLEX_T_FILE_CONTROL; }
"FILLER" { build_token(); return FLEX_T_FILLER; }
"FIRST" { build_token(); return FLEX_T_FIRST; }
"FOOTING" { build_token(); return FLEX_T_FOOTING; }
"FOR" { build_token(); return FLEX_T_FOR; }
"FROM" { build_token(); return FLEX_T_FROM; }
"GIVING" { build_token(); return FLEX_T_GIVING; }
"GLOBAL" { build_token(); return FLEX_T_GLOBAL; }
"GO" { build_token(); return FLEX_T_GO; }
"GOBACK" { build_token(); return FLEX_T_GOBACK; }
"GREATER" { build_token(); return FLEX_T_GREATER; }
"HIGH-VALUE" { build_token(); return FLEX_T_HIGH_VALUE; }
"HIGH-VALUES" { build_token(); return FLEX_T_HIGH_VALUES; }
"I-O" { build_token(); return FLEX_T_I_O; }
"I-O-CONTROL" { build_token(); return FLEX_T_I_O_CONTROL; }
"ID" { build_token(); return FLEX_T_ID; }
"IDENTIFICATION" { build_token(); return FLEX_T_IDENTIFICATION; }
"IF" { build_token(); return FLEX_T_IF; }
"IN" { build_token(); return FLEX_T_IN; }
"INDEX" { build_token(); return FLEX_T_INDEX; }
"INDEXED" { build_token(); return FLEX_T_INDEXED; }
"INITIAL" { build_token(); return FLEX_T_INITIAL; }
"INITIALIZE" { build_token(); return FLEX_T_INITIALIZE; }
"INPUT" { build_token(); return FLEX_T_INPUT; }
"INPUT-OUTPUT" { build_token(); return FLEX_T_INPUT_OUTPUT; }
"INSPECT" { build_token(); return FLEX_T_INSPECT; }
"INSTALLATION" { build_token(); return FLEX_T_INSTALLATION; }
"INTO" { build_token(); return FLEX_T_INTO; }
"INVALID" { build_token(); return FLEX_T_INVALID; }
"IS" { build_token(); return FLEX_T_IS; }
"JUST" { build_token(); return FLEX_T_JUST; }
"JUSTIFIED" { build_token(); return FLEX_T_JUSTIFIED; }
"KANJI" { build_token(); return FLEX_T_KANJI; }
"KEY" { build_token(); return FLEX_T_KEY; }
"LABEL" { build_token(); return FLEX_T_LABEL; }
"LEADING" { build_token(); return FLEX_T_LEADING; }
"LEFT" { build_token(); return FLEX_T_LEFT; }
"LENGTH" { build_token(); return FLEX_T_LENGTH; }
"LESS" { build_token(); return FLEX_T_LESS; }
"LINAGE" { build_token(); return FLEX_T_LINAGE; }
"LINAGE-COUNTER" { build_token(); return FLEX_T_LINAGE_COUNTER; }
"LINE" { build_token(); return FLEX_T_LINE; }
"LINES" { build_token(); return FLEX_T_LINES; }
"LINKAGE" { build_token(); return FLEX_T_LINKAGE; }
"LOCK" { build_token(); return FLEX_T_LOCK; }
"LOW-VALUE" { build_token(); return FLEX_T_LOW_VALUE; }
"LOW-VALUES" { build_token(); return FLEX_T_LOW_VALUES; }
"MEMORY" { build_token(); return FLEX_T_MEMORY; }
"MERGE" { build_token(); return FLEX_T_MERGE; }
"MODE" { build_token(); return FLEX_T_MODE; }
"MODULES" { build_token(); return FLEX_T_MODULES; }
"MORE-LABELS" { build_token(); return FLEX_T_MORE_LABELS; }
"MOVE" { build_token(); return FLEX_T_MOVE; }
"MULTIPLE" { build_token(); return FLEX_T_MULTIPLE; }
"MULTIPLY" { build_token(); return FLEX_T_MULTIPLY; }
"NATIVE" { build_token(); return FLEX_T_NATIVE; }
"NEGATIVE" { build_token(); return FLEX_T_NEGATIVE; }
"NEXT" { build_token(); return FLEX_T_NEXT; }
"NO" { build_token(); return FLEX_T_NO; }
"NOT" { build_token(); return FLEX_T_NOT; }
"NULL" { build_token(); return FLEX_T_NULL; }
"NULLS" { build_token(); return FLEX_T_NULLS; }
"NUMERIC" { build_token(); return FLEX_T_NUMERIC; }
"NUMERIC-EDITED" { build_token(); return FLEX_T_NUMERIC_EDITED; }
"OBJECT-COMPUTER" { build_token(); return FLEX_T_OBJECT_COMPUTER; }
"OCCURS" { build_token(); return FLEX_T_OCCURS; }
"OF" { build_token(); return FLEX_T_OF; }
"OFF" { build_token(); return FLEX_T_OFF; }
"OMITTED" { build_token(); return FLEX_T_OMITTED; }
"ON" { build_token(); return FLEX_T_ON; }
"OPEN" { build_token(); return FLEX_T_OPEN; }
"OPTIONAL" { build_token(); return FLEX_T_OPTIONAL; }
"OR" { build_token(); return FLEX_T_OR; }
"ORDER" { build_token(); return FLEX_T_ORDER; }
"ORGANIZATION" { build_token(); return FLEX_T_ORGANIZATION; }
"OTHER" { build_token(); return FLEX_T_OTHER; }
"OUTPUT" { build_token(); return FLEX_T_OUTPUT; }
"OVERFLOW" { build_token(); return FLEX_T_OVERFLOW; }
"PACKED-DECIMAL" { build_token(); return FLEX_T_PACKED_DECIMAL; }
"PADDING" { build_token(); return FLEX_T_PADDING; }
"PAGE" { build_token(); return FLEX_T_PAGE; }
"PASSWORD" { build_token(); return FLEX_T_PASSWORD; }
"PERFORM" { build_token(); return FLEX_T_PERFORM; }
"POINTER" { build_token(); return FLEX_T_POINTER; }
"POSITION" { build_token(); return FLEX_T_POSITION; }
"POSITIVE" { build_token(); return FLEX_T_POSITIVE; }
"PROCEDURE" { build_token(); return FLEX_T_PROCEDURE; }
"PROCEDURES" { build_token(); return FLEX_T_PROCEDURES; }
"PROCEED" { build_token(); return FLEX_T_PROCEED; }
"PROGRAM" { build_token(); return FLEX_T_PROGRAM; }
"PROGRAM-ID" { build_token(); return FLEX_T_PROGRAM_ID; }
"QUOTE" { build_token(); return FLEX_T_QUOTE; }
"QUOTES" { build_token(); return FLEX_T_QUOTES; }
"RANDOM" { build_token(); return FLEX_T_RANDOM; }
"READ" { build_token(); return FLEX_T_READ; }
"RECORD" { build_token(); return FLEX_T_RECORD; }
"RECORDING" { build_token(); return FLEX_T_RECORDING; }
"RECORDS" { build_token(); return FLEX_T_RECORDS; }
"REDEFINES" { build_token(); return FLEX_T_REDEFINES; }
"REEL" { build_token(); return FLEX_T_REEL; }
"REFERENCE" { build_token(); return FLEX_T_REFERENCE; }
"RELATIVE" { build_token(); return FLEX_T_RELATIVE; }
"RELEASE" { build_token(); return FLEX_T_RELEASE; }
"REMAINDER" { build_token(); return FLEX_T_REMAINDER; }
"REMARKS" { build_token(); return FLEX_T_REMARKS; }
"REMOVAL" { build_token(); return FLEX_T_REMOVAL; }
"RENAMES" { build_token(); return FLEX_T_RENAMES; }
"REPLACING" { build_token(); return FLEX_T_REPLACING; }
"RERUN" { build_token(); return FLEX_T_RERUN; }
"RESERVE" { build_token(); return FLEX_T_RESERVE; }
"RETURN" { build_token(); return FLEX_T_RETURN; }
"RETURN-CODE" { build_token(); return FLEX_T_RETURN_CODE; }
"REVERSED" { build_token(); return FLEX_T_REVERSED; }
"REWIND" { build_token(); return FLEX_T_REWIND; }
"REWRITE" { build_token(); return FLEX_T_REWRITE; }
"RIGHT" { build_token(); return FLEX_T_RIGHT; }
"ROUNDED" { build_token(); return FLEX_T_ROUNDED; }
"RUN" { build_token(); return FLEX_T_RUN; }
"SAME" { build_token(); return FLEX_T_SAME; }
"SD" { build_token(); return FLEX_T_SD; }
"SEARCH" { build_token(); return FLEX_T_SEARCH; }
"SECTION" { build_token(); return FLEX_T_SECTION; }
"SECURITY" { build_token(); return FLEX_T_SECURITY; }
"SEGMENT-LIMIT" { build_token(); return FLEX_T_SEGMENT_LIMIT; }
"SELECT" { build_token(); return FLEX_T_SELECT; }
"SENTENCE" { build_token(); return FLEX_T_SENTENCE; }
"SEPARATE" { build_token(); return FLEX_T_SEPARATE; }
"SEQUENCE" { build_token(); return FLEX_T_SEQUENCE; }
"SEQUENTIAL" { build_token(); return FLEX_T_SEQUENTIAL; }
"SET" { build_token(); return FLEX_T_SET; }
"SHIFT-IN" { build_token(); return FLEX_T_SHIFT_IN; }
"SHIFT-OUT" { build_token(); return FLEX_T_SHIFT_OUT; }
"SIGN" { build_token(); return FLEX_T_SIGN; }
"SIZE" { build_token(); return FLEX_T_SIZE; }
"SORT" { build_token(); return FLEX_T_SORT; }
"SORT-CONTROL" { build_token(); return FLEX_T_SORT_CONTROL; }
"SORT-CORE-SIZE" { build_token(); return FLEX_T_SORT_CORE_SIZE; }
"SORT-FILE-SIZE" { build_token(); return FLEX_T_SORT_FILE_SIZE; }
"SORT-MERGE" { build_token(); return FLEX_T_SORT_MERGE; }
"SORT-MESSAGE" { build_token(); return FLEX_T_SORT_MESSAGE; }
"SORT-MODE-SIZE" { build_token(); return FLEX_T_SORT_MODE_SIZE; }
"SORT-RETURN" { build_token(); return FLEX_T_SORT_RETURN; }
"SOURCE-COMPUTER" { build_token(); return FLEX_T_SOURCE_COMPUTER; }
"SPACE" { build_token(); return FLEX_T_SPACE; }
"SPACES" { build_token(); return FLEX_T_SPACES; }
"SPECIAL-NAMES" { build_token(); return FLEX_T_SPECIAL_NAMES; }
"STANDARD" { build_token(); return FLEX_T_STANDARD; }
"STANDARD-1" { build_token(); return FLEX_T_STANDARD_1; }
"STANDARD-2" { build_token(); return FLEX_T_STANDARD_2; }
"START" { build_token(); return FLEX_T_START; }
"STATUS" { build_token(); return FLEX_T_STATUS; }
"STOP" { build_token(); return FLEX_T_STOP; }
"STRING" { build_token(); return FLEX_T_STRING; }
"SUBTRACT" { build_token(); return FLEX_T_SUBTRACT; }
"SUPPRESS" { build_token(); return FLEX_T_SUPPRESS; }
"SYMBOLIC" { build_token(); return FLEX_T_SYMBOLIC; }
"SYNC" { build_token(); return FLEX_T_SYNC; }
"SYNCHRONIZED" { build_token(); return FLEX_T_SYNCHRONIZED; }
"TALLY" { build_token(); return FLEX_T_TALLY; }
"TALLYING" { build_token(); return FLEX_T_TALLYING; }
"TAPE" { build_token(); return FLEX_T_TAPE; }
"TEST" { build_token(); return FLEX_T_TEST; }
"THAN" { build_token(); return FLEX_T_THAN; }
"THEN" { build_token(); return FLEX_T_THEN; }
"THROUGH" { build_token(); return FLEX_T_THROUGH; }
"THRU" { build_token(); return FLEX_T_THRU; }
"TIME" { build_token(); return FLEX_T_TIME; }
"TIMES" { build_token(); return FLEX_T_TIMES; }
"TO" { build_token(); return FLEX_T_TO; }
"TOP" { build_token(); return FLEX_T_TOP; }
"TRAILING" { build_token(); return FLEX_T_TRAILING; }
"TRUE" { build_token(); return FLEX_T_TRUE; }
"UNIT" { build_token(); return FLEX_T_UNIT; }
"UNSTRING" { build_token(); return FLEX_T_UNSTRING; }
"UNTIL" { build_token(); return FLEX_T_UNTIL; }
"UP" { build_token(); return FLEX_T_UP; }
"UPON" { build_token(); return FLEX_T_UPON; }
"USAGE" { build_token(); return FLEX_T_USAGE; }
"USE" { build_token(); return FLEX_T_USE; }
"USING" { build_token(); return FLEX_T_USING; }
"VALUE" { build_token(); return FLEX_T_VALUE; }
"VALUES" { build_token(); return FLEX_T_VALUES; }
"VARYING" { build_token(); return FLEX_T_VARYING; }
"WHEN" { build_token(); return FLEX_T_WHEN; }
"WHEN-COMPILED" { build_token(); return FLEX_T_WHEN_COMPILED; }
"WITH" { build_token(); return FLEX_T_WITH; }
"WORDS" { build_token(); return FLEX_T_WORDS; }
"WORKING-STORAGE" { build_token(); return FLEX_T_WORKING_STORAGE; }
"WRITE" { build_token(); return FLEX_T_WRITE; }
"WRITE-ONLY" { build_token(); return FLEX_T_WRITE_ONLY; }
"ZERO" { build_token(); return FLEX_T_ZERO; }
"ZEROES" { build_token(); return FLEX_T_ZEROES; }
"ZEROS" { build_token(); return FLEX_T_ZEROS; }

"PICTURE" {
	BEGIN(PIC);
	build_token();
	return FLEX_T_PICTURE;
}

"PIC" {
	BEGIN(PIC);
	build_token();
	return FLEX_T_PIC;
}

[\+\-]?(([0-9]*[\.\,][0-9]+)|[0-9]+) {
	build_token();
	yylval = TTbuildVscii_numeric(yylval);
	return FLEX_VSCII_NUMERIC;
}

<PIC>([^ \t\n,;.]|([,;.][^ \t\n]))+ {
	BEGIN(INITIAL);
	build_token();
	yylval = TTbuildVscii_character_string(yylval);
	return FLEX_VSCII_CHARACTER_STRING;
}

<PIC>({WS}|{COM})+ {
	build_token();
	return FLEX_LAYOUT;
}

[A-Za-z0-9\-]+ {
	build_token();
	yylval = TTbuildVscii_cobol_word(yylval);
	return FLEX_VSCII_COBOL_WORD;
}

"=="[^=]*"==" {
	build_token();
	yylval = TTbuildVscii_quoted_pseudo_text(yylval);
	return FLEX_VSCII_QUOTED_PSEUDO_TEXT;
}

["][^"\n]*["] {
	build_token();
	yylval = TTbuildVscii_nonnumeric(yylval);
	return FLEX_VSCII_NONNUMERIC;
}

['][^'\n]*['] {
	build_token();
	yylval = TTbuildVscii_nonnumeric(yylval);
	return FLEX_VSCII_NONNUMERIC;
}

. {
	snprintf(st_buf, BUFSZ, "Illegal character in input: `%c'\n", yytext[0]);
	TTerror(st_buf);
}

%%

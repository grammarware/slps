% Parse keywords
@ Cs -->
  maxlex(Cs,_),
  ( { cobol_word(Cs,[]) }, !, eocw#
  ; []
  ),
  !,
  whitespaces?
  .


% Test for end-of-cobol-word by follow restriction
eocw -->
  maxlex(( ~( 0'- ; 0'A .. 0'Z ; 0'a .. 0'z ; 0'0 .. 0'9 ) 
         ), _)
  .


% Parse a user-defined word
user_defined_word -->
  cobol_word3(Cs),
  { \+ reserved_word(Cs) }
  .


% Parse an alphabetic user-defined word
alphabetic_user_defined_word -->
  cobol_word3(Cs),
  { \+ reserved_word(Cs) },
  { member(C,Cs) },
  { C >= 0'A, C =< 0'Z, ! 
  ; C >= 0'a, C =< 0'z 
  }
  .


% Parse a Cobol word
cobol_word --> cobol_word3(_).
cobol_word3(Cs) -->
  maxlex(( ( 0'A .. 0'Z ; 0'a .. 0'z ; 0'0 .. 0'9 )+,
           ( 0'-+, ( 0'A .. 0'Z ; 0'a .. 0'z ; 0'0 .. 0'9 )+ )*
         ), Cs),
  whitespaces?
  .


% Parse a level number; 66 and 88 handled by specific productions.
level_number -->
  maxlex(( ( 0'0, 0'1..0'9 )
         ; ( 0'1..0'4, 0'0..0'9 )
         ; "77"
	 ; 0'1..0'9
         ), _),
  eocw#,
  whitespaces?
  .


% Parse a priority number
priority_number -->
  maxlex(( 0'0..0'9, (0'0..0'9)?
         ), _),
  eocw#,
  whitespaces?
  .


% Parse a character string = picture mask
character_string -->
  maxlex( ( ~( nl ; space ; 0', ; 0'; ; 0'. )
          ; ( 0', ; 0'; ; 0'. ), ~( nl ; space )
          )+, Cs),
  { \+ reserved_word(Cs) },
  whitespaces?
  .


% Parse a nonnumeric literal
nonnumeric --> 
  { member(Q,[0'",0'']) },
  maxlex(( Q, (~Q; Q, Q)*, Q
         ; (0'X;0'x), Q, (0'0..0'9;0'A..0'F;0'a..0'f)+, Q
         ), _),
  whitespaces?
  .


% Parse a numeric literal
numeric -->
  maxlex(( (0'+;0'-)?, ( (0'0..0'9)*, ( 0'. ; 0', ), (0'0..0'9)+ 
	               ; (0'0..0'9)+
                       )
         ), _),
  whitespaces?
  .


% Parse an integer literal
integer -->
  maxlex(( (0'0..0'9)+
         ), _),
  eocw#,
  whitespaces?
  .


% Parsing DBCS literals unsupported
dbcs -->
  { fail }
  .


% Parsing comment entries unsupported; resolved by preprocessing
comment_entry -->
  { fail }
  .


% Parsing system names as Cobol words but see 1.1.1.3
system_name -->
  cobol_word3(Cs),
  { \+ reserved_word(Cs) }
  .


% Parse a nonnumeric literal
quoted_pseudo_text --> 
  { Q = 0'= },
  maxlex(( Q, Q, (~Q)*, Q, Q
         ), _),
  whitespaces?
  .


% White spaces
whitespaces  -->
  maxlex( ( space
          ; nl, space*, (("EJECT";"SKIP1";"SKIP2";"SKIP3"), space*, (nl;eof)#)?
          ; 0'%,(~(nl;eof))*
          )+, _)
  .


% Not all terminals are reserved words
reserved_word("F") :- !, fail.
reserved_word("V") :- !, fail.
reserved_word("U") :- !, fail.
reserved_word("S") :- !, fail.
reserved_word(Cs)  :- terminal(Cs).

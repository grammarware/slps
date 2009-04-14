% Made "." optional.
Replace
 "."
By 
 "."?
In
 data-description-entry-format-i
;

% Made "." optional.
Replace
 "."
By 
 "."?
In
 data-description-entry-format-ii
;

% Made "." optional.
Replace
 "."
By 
 "."?
In
 data-description-entry-format-iii
;

% Enabled non-grammatical code as accepted by compiler.
Replace
 ( "RECORD" "IS"?
 | "RECORDS" "ARE"?
 )
By
 ( "RECORD" | "RECORDS" )
 ( "IS" | "ARE" )?
In
 label-records-clause
;

% Enabled non-grammatical code as accepted by compiler.
Replace
 ( "RECORD" "IS"?
 | "RECORDS" "ARE"?
 )
By
 ( "RECORD" | "RECORDS" )
 ( "IS" | "ARE" )?
In
 data-records-clause
;

% Enabled non-grammatical code as accepted by compiler.
Replace
 "IS"? "NOT"?
By
 ( "IS"
 | "NOT"
 | "IS" "NOT"
 | "NOT" "IS"
 )?
In
 class-condition
;

% Enabled non-grammatical code as accepted by compiler.
Replace
 "IS"? "NOT"?
By
 ( "IS"
 | "NOT"
 | "IS" "NOT"
 | "NOT" "IS"
 )?
In
 sign-condition
;

% Enabled non-grammatical code as accepted by compiler.
Replace
 "NOT"?
By
 /* epsilon */
In
 relational-operator
;

% Enabled non-grammatical code as accepted by compiler.
Replace
 "IS"?
By
 ( "IS"
 | "NOT"
 | "IS" "NOT"
 | "NOT" "IS"
 )?
In
 relational-operator
;

% Enabled non-grammatical code as accepted by compiler.
Extend
 subscript
By
 ( "+" | "-" ) integer
;

% Supported obsolete verb EXAMINE
Add 
 examine-statement =
   "EXAMINE" identifier "TALLYING" ( "ALL" | "LEADING" ) literal
After
 inspect-statement
;

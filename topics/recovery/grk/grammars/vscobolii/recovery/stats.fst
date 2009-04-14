/*

  This script comprehends all transformations to clean up the statement
  syntax of Cobol programs. The script is subdivided in steps as follows:

  - Fix flaws and implement notes given in the text.
  - Identify some complex clauses.
  - Add diagrams to take the union of different statement formats.
  - Fold conditional phrases to improve reuse and reduce grammar size.

*/


% Removed "." which is not part of the statement.
Replace 
 "EXIT" "PROGRAM" "."
By
 "EXIT" "PROGRAM"
In 
 exit-program-statement
;

% Removed all illustrative parts which are not part of the statement.
Replace
 paragraph-name "." "EXIT" "."
By
 "EXIT"
In
 exit-statement
;

% Treat altered GOs as statements and not paragraphs.
Extract
 altered-go-to = "GO" "TO"?
From
 go-to-statement-format-iii
;

% Enabled multiple targets as exercised by VS Cobol II code.
Replace 
 identifier-2
By
 identifier-2+
In
 move-statement-format-ii
;

% Repaired typo: "NEXT-SENTENCE" -> "NEXT" "SENTENCE"
Replace
 "NEXT-SENTENCE"
By
 "NEXT" "SENTENCE"
In
 search-statement-format-i
;

% Implemented note (1) as given below the diagram
Replace
    series-of-imperative-statements-2 @1
  | "NEXT" "SENTENCE" @1
By
  ( series-of-imperative-statements-2 @1
  | "NEXT" "SENTENCE" @1 
  )?
In 
 search-statement-format-ii
;

% Implemented note (1) as given below the diagram.
Replace
 series-of-imperative-statements-1  @1
By
 series-of-imperative-statements-1? @1
In
 perform-statement-format-i
;

% Implemented note (1) as given below the diagram.
Replace
 series-of-imperative-statements-1  @1
By
 series-of-imperative-statements-1? @1
In
 perform-statement-format-ii
;

% Implemented note (1) as given below the diagram.
Replace
 series-of-imperative-statements-1  @1
By
 series-of-imperative-statements-1? @1
In
 perform-statement-format-iii
;

% Implemented note (1) as given below the diagram.
Replace
 series-of-imperative-statements-1  @1
By
 series-of-imperative-statements-1? @1
In
 perform-statement-format-iv
;

% Identified USING phrase of subprogram call.
Add
 call-using-phrase =
   "USING" ( ("BY"? "REFERENCE")? ( identifier-2
                                  | { "ADDRESS" "OF" identifier-3 }
                                  | { file-name-1 }
                                  )+ 
           | "BY"? "CONTENT" ( ( { "LENGTH" "OF" } )? identifier-2
                             | { "ADDRESS" "OF" identifier-3 } 
                             | { literal-2 }
                             )+
           )+
To
 3.4
;

% Factored out clause for conciseness.
Fold
 call-using-phrase
In
 call-statement-format-i
;

% Factored out clause for conciseness.
Fold
 call-using-phrase
In
 call-statement-format-ii
;

% Different formats of ACCEPT statements.
Add
 accept-statement = 
    accept-statement-format-i
  | accept-statement-format-ii
To
 3.1
;

% Different formats of ADD statements.
Add
 add-statement = 
    add-statement-format-i
  | add-statement-format-ii
  | add-statement-format-iii
To
 3.2
;

% Different formats of CALL statements.
Add
 call-statement = 
    call-statement-format-i
  | call-statement-format-ii
To
 3.4
;

% Different formats of CLOSE statements.
Add
 close-statement = 
    close-statement-format-i
  | close-statement-format-ii
To
 3.6
;

% Different formats of DIVIDE statements.
Add
 divide-statement = 
    divide-statement-format-i
  | divide-statement-format-ii
  | divide-statement-format-iii
  | divide-statement-format-iv
  | divide-statement-format-v
To
 3.11
;

% Different formats of GO statements.
Add
 go-to-statement =
     go-to-statement-format-i
   | go-to-statement-format-ii
   | altered-go-to
   | go-to-statement-format-iv
To
 3.17
;

% Different formats of INSPECT statements.
Add
 inspect-statement =
     inspect-statement-format-i
   | inspect-statement-format-ii
   | inspect-statement-format-iii
   | inspect-statement-format-iv
To
 3.20
;

% Different formats of MOVE statements.
Add
 move-statement = 
    move-statement-format-i
  | move-statement-format-ii
To
 3.22
;

% Different formats of MULTIPLY statements.
Add
 multiply-statement = 
    multiply-statement-format-i
  | multiply-statement-format-ii
To
 3.23
;


% Different formats of OPEN statements.
Add
 open-statement = 
    open-statement-format-i
  | open-statement-format-ii
To
 3.24
;

% Different formats of PERFORM statements.
Add
 perform-statement = 
    perform-statement-format-i
  | perform-statement-format-ii
  | perform-statement-format-iii
  | perform-statement-format-iv
To
 3.25
;

% Different formats of READ statements.
Add
 read-statement = 
    read-statement-format-i
  | read-statement-format-ii
To
 3.26
;

% Different formats of SEARCH statements.
Add
 search-statement = 
    search-statement-format-i
  | search-statement-format-ii
To
 3.30
;

% Different formats of SET statements.
Add
 set-statement = 
    set-statement-format-i
  | set-statement-format-ii
  | set-statement-format-iii
  | set-statement-format-iv
  | set-statement-format-v
To
 3.31
;

% Different formats of SUBTRACT statements.
Add
 subtract-statement = 
    subtract-statement-format-i
  | subtract-statement-format-ii
  | subtract-statement-format-iii
To
 3.36
;

% Different formats of WRITE statements.
Add
 write-statement = 
    write-statement-format-i
  | write-statement-format-ii
  | write-statement-format-iii
  | write-statement-format-iv
To
 3.38
;

% Union of replace directive
Add
 replace-directive =
    replace-directive-format-i
  | replace-directive-format-ii
To
 4.10
;

% Union of use directives
Add
 use-directive = 
    use-directive-format-i
  | use-directive-format-ii
  | use-directive-format-iii
To
 4.15
;

% Top-rule for compiler-directing statement; for completeness' sake.
Add
 compiler-directing-statement =
    basis-directive
  | cbl-process-directive
  | control-cbl-directive
  | copy-directive
  | delete-directive
  | eject-directive
  | enter-directive
  | insert-directive
  | ready-reset-trace-directive
  | replace-directive
  | service-label-directive
  | service-reload-directive
  | skip123-directive
  | title-directive
  | use-directive
To
 4.0
;

% Capture conditional phrase for reuse.
Add
 on-size-error =
   "ON"? "SIZE" "ERROR" series-of-imperative-statements-1
To
 2.8.7.4
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 add-statement-format-i
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 add-statement-format-ii
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 add-statement-format-iii
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 compute-statement
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 divide-statement-format-i
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 divide-statement-format-ii
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 divide-statement-format-iii
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 divide-statement-format-iv
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 divide-statement-format-v
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 multiply-statement-format-i
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 multiply-statement-format-ii
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 subtract-statement-format-i
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 subtract-statement-format-ii
;

% Factored out clause for conciseness.
Fold
 on-size-error
In
 subtract-statement-format-iii
;

% Capture conditional phrase for reuse.
Add
 not-on-size-error =
   "NOT" "ON"? "SIZE" "ERROR" series-of-imperative-statements-2
To
 2.8.7.4
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 add-statement-format-i
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 add-statement-format-ii
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 add-statement-format-iii
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 compute-statement
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 divide-statement-format-i
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 divide-statement-format-ii
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 divide-statement-format-iii
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 divide-statement-format-iv
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 divide-statement-format-v
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 multiply-statement-format-i
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 multiply-statement-format-ii
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 subtract-statement-format-i
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 subtract-statement-format-ii
;

% Factored out clause for conciseness.
Fold
 not-on-size-error
In
 subtract-statement-format-iii
;

% Capture conditional phrase for reuse.
Add
 on-overflow =
   "ON"? "OVERFLOW" series-of-imperative-statements-1
To
 3.4.6
;

% Factored out clause for conciseness.
Fold
 on-overflow
In
 call-statement-format-i
;

% Factored out clause for conciseness.
Fold
 on-overflow
In
 string-statement
;

% Factored out clause for conciseness.
Fold
 on-overflow
In
 unstring-statement
;

% Capture conditional phrase for reuse.
Add
 not-on-overflow =
   "NOT" "ON"? "OVERFLOW" series-of-imperative-statements-2
To
 3.4.6
;

% Factored out clause for conciseness.
Fold
 not-on-overflow
In
 string-statement
;

% Factored out clause for conciseness.
Fold
 not-on-overflow
In
 unstring-statement
;

% Capture conditional phrase for reuse.
Add
 on-exception =
   "ON"? "EXCEPTION" series-of-imperative-statements-1
To
 3.4.4
;

% Factored out clause for conciseness.
Fold
 on-exception
In
 call-statement-format-ii
;

% Capture conditional phrase for reuse.
Add
 not-on-exception =
   "NOT" "ON"? "EXCEPTION" series-of-imperative-statements-2
To
 3.4.5
;

% Factored out clause for conciseness.
Fold
 not-on-exception
In
 call-statement-format-ii
;

% Capture conditional phrase for reuse.
Add
 not-invalid-key =
   "NOT" "INVALID" "KEY"? series-of-imperative-statements
To
 2.8.9.1.2
;

% Factored out clause for conciseness.
Fold
 not-invalid-key
In
 delete-statement
;

% Factored out clause for conciseness.
Fold
 not-invalid-key
In
 read-statement-format-ii
;

% Factored out clause for conciseness.
Fold
 not-invalid-key
In
 rewrite-statement
;

% Factored out clause for conciseness.
Fold
 not-invalid-key
In
 start-statement
;

% Factored out clause for conciseness.
Fold
 not-invalid-key
In
 write-statement-format-i
;

% Factored out clause for conciseness.
Fold
 not-invalid-key
In
 write-statement-format-iii
;

% Factored out clause for conciseness.
Fold
 not-invalid-key
In
 write-statement-format-iv
;

% Capture conditional phrase for reuse.
Add
 invalid-key =
   "INVALID" "KEY"? series-of-imperative-statements
To
 2.8.9.1.2
;

% Factored out clause for conciseness.
Fold
 invalid-key
In
 delete-statement
;

% Factored out clause for conciseness.
Fold
 invalid-key
In
 read-statement-format-ii
;

% Factored out clause for conciseness.
Fold
 invalid-key
In
 rewrite-statement
;

% Factored out clause for conciseness.
Fold
 invalid-key
In
 start-statement
;

% Factored out clause for conciseness.
Fold
 invalid-key
In
 write-statement-format-i
;

% Factored out clause for conciseness.
Fold
 invalid-key
In
 write-statement-format-iii
;

% Factored out clause for conciseness.
Fold
 invalid-key
In
 write-statement-format-iv
;

% Capture subscript-free syntax fragment used in evaluate-statement.
Add
 e-phrase =
  "ANY" | condition | "TRUE" | "FALSE" | "NOT"? (identifier | literal | arithmetic-expression) (("THROUGH" | "THRU") (identifier | literal | arithmetic-expression))?
After
 evaluate-statement;
 
% Factored out clause for conciseness.
Fold
 e-phrase
In
 e-phrase-a
;

% Factored out clause for conciseness.
Fold
 e-phrase
In
 e-phrase-b
;

% Inline clause for conciseness.
Unfold
 e-phrase-a
In
 evaluate-statement
;

% Inline clause for conciseness.
Unfold
 e-phrase-b
In
 evaluate-statement
;

% Not needed anymore as a result of refactoring.
Reject
 e-phrase-a
;

% Not needed anymore as a result of refactoring.
Reject
 e-phrase-b
;

% Capture conditional phrase for conciseness
Extract
 when-phrase =
   ("WHEN" e-phrase ("ALSO" e-phrase)*)+ series-of-imperative-statements-1
From
 evaluate-statement
;

% Capture conditional phrase for conciseness
Extract
 when-other-phrase =
   "WHEN" "OTHER" series-of-imperative-statements-2
From
 evaluate-statement
;

% Share syntax fragment among the four formats.
Add 
 before-after-phrase =
   ("BEFORE" | "AFTER") "INITIAL"? (identifier-4 | literal-2)
To
 3.20.4
;

% Factored out clause for conciseness.
Fold
 before-after-phrase
In
 i1-phrase
;

% Factored out clause for conciseness.
Fold
 before-after-phrase
In
 i2-phrase
;

% Factored out clause for conciseness.
Fold
 before-after-phrase
In
 i3-phrase
;

% Factored out clause for conciseness.
Fold
 before-after-phrase
In
 i4-phrase
;

% Inline clause for conciseness.
Unfold
 i1-phrase
In
 inspect-statement-format-i
;

% Inline clause for conciseness.
Unfold
 i2-phrase
In
 inspect-statement-format-ii
;

% Inline clause for conciseness.
Unfold
 i3-phrase
In
 inspect-statement-format-iii
;

% Inline clause for conciseness.
Unfold
 i4-phrase
In
 inspect-statement-format-iv
;

% Not needed anymore as a result of refactoring.
Reject
 i1-phrase
;

% Not needed anymore as a result of refactoring.
Reject
 i2-phrase
;

% Not needed anymore as a result of refactoring.
Reject
 i3-phrase
;

% Not needed anymore as a result of refactoring.
Reject
 i4-phrase
;

% Capture phrase for reuse.
Extract
 inspect-tallying-phrase =
   "TALLYING" \\
   ( identifier-2 "FOR" ( "CHARACTERS" before-after-phrase*
                        | ( "ALL" | "LEADING" )
                          ( ( identifier-3 | literal-1 ) before-after-phrase* )+
                        )+
   )+
From
 inspect-statement-format-i
;

% Factored out clause for conciseness.
Fold
 inspect-tallying-phrase
In
 inspect-statement-format-iii
;

% Capture phrase for reuse.
Extract
 inspect-replacing-phrase =
   "REPLACING" \\
   ( "CHARACTERS" "BY"
     ( identifier-5 | literal-3 ) before-after-phrase* 
     | ( "ALL" | "LEADING" | "FIRST" )
       ( ( identifier-3 | literal-1 )
         "BY"
         ( identifier-5 | literal-3 ) before-after-phrase*
       )+
   )+
From
 inspect-statement-format-ii
;

% Factored out clause for conciseness.
Fold
 inspect-replacing-phrase
In
 inspect-statement-format-iii
;

% Capture conditional phrase for reuse.
Add
 at-end =
   "AT"? "END" series-of-imperative-statements-1
To
 3.26.2
;

% Factored out clause for conciseness.
Fold
 at-end
In
 read-statement-format-i
;

% Factored out clause for conciseness.
Fold
 at-end
In
 return-statement
;

% Factored out clause for conciseness.
Fold
 at-end
In
 search-statement-format-i
;

% Factored out clause for conciseness.
Fold
 at-end
In
 search-statement-format-ii
;

% Capture conditional phrase for reuse.
Add
 not-at-end =
   "NOT" "AT"? "END" series-of-imperative-statements-2
To
 3.26.2
;

% Factored out clause for conciseness.
Fold
 not-at-end
In
 read-statement-format-i
;

% Factored out clause for conciseness.
Fold
 not-at-end
In
 return-statement
;

% Factored out clause for conciseness.
Extract
 write-before-after =
   ( ("BEFORE" | "AFTER") "ADVANCING"?
     ( ( identifier-2 | integer-1 ) ( "LINE" | "LINES" )? 
     | mnemonic-name-1
     | "PAGE"
     ) 
   )?
From
 write-statement-format-i
;

% Capture conditional phrase for clarity.
Extract
 at-eop =
   "AT"? ("END-OF-PAGE" | "EOP") series-of-imperative-statements-2
From
 w-phrase
;

% Capture conditional phrase for clarity.
Extract
 not-at-eop =
   "NOT" "AT"? ("END-OF-PAGE" | "EOP") series-of-imperative-statements-4
From
 w-phrase
;

% DEGUGGING should better be DEBUGGING
Replace
 "DEGUGGING"
By 
 "DEBUGGING"
In
 use-directive-format-iii
;


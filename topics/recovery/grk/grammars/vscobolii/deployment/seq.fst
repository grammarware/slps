/*

  This script comprehends all transformations to establish the proper
  structure of statement sequences, in particular in the sense of
  conditional statements with nested statement sequences. To obtain
  both a simpler and a more tolerant grammar, we do NOT enforce the
  VS Cobol II restriction that conditional statements can only be
  nested inside IF statements.

  This process involves the following steps:

  - Groups of conditional phrases are added as designated diagrams.
  - These groups are folded on use-sites accordingly for safety.
  - Conditional phrases and END delimiters are removed from formats.
  - The epsilon option is removed from these groups.
  - Statement sequences are defined including optional conditional phrases.

*/

% Identified conditional phrases.
Add
 size-error-phrases = on-size-error? not-on-size-error? 
To
 2.8.7.4
;

% Identified conditional phrases.
Add
 exception-phrases = on-exception? not-on-exception? 
To
 3.4.4
;

% Identified conditional phrases.
Add
 at-end-phrases = at-end? not-at-end? 
To
 3.28.1
;

% Identified conditional phrases.
Add
 invalid-key-phrases = invalid-key? not-invalid-key? 
To
 3.26.3
;

% Identified conditional phrases.
Add
 overflow-phrases = on-overflow? not-on-overflow? 
To
 3.4.6
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 add-statement-format-i
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 add-statement-format-ii
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 add-statement-format-iii
;

% Identified conditional phrases.
Fold
 exception-phrases
In
 call-statement-format-ii
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 compute-statement
;

% Identified conditional phrases.
Fold
 invalid-key-phrases
In
 delete-statement
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 divide-statement-format-i
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 divide-statement-format-ii
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 divide-statement-format-iii
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 divide-statement-format-iv
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 divide-statement-format-v
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 multiply-statement-format-i
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 multiply-statement-format-ii
;

% Identified conditional phrases.
Fold
 at-end-phrases
In
 read-statement-format-i
;

% Identified conditional phrases.
Fold
 invalid-key-phrases
In
 read-statement-format-ii
;

% Identified conditional phrases.
Fold
 invalid-key-phrases
In
 rewrite-statement
;

% Identified conditional phrases.
Fold
 invalid-key-phrases
In
 start-statement
;

% Identified conditional phrases.
Fold
 overflow-phrases
In
 string-statement
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 subtract-statement-format-i
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 subtract-statement-format-ii
;

% Identified conditional phrases.
Fold
 size-error-phrases
In
 subtract-statement-format-iii
;

% Identified conditional phrases.
Fold
 overflow-phrases
In
 unstring-statement
;

% Normalised away IBM braces.
Replace
 ( { invalid-key } )? ( { not-invalid-key } )? 
By
 invalid-key? not-invalid-key? 
In
 write-statement-format-i
;

% Identified conditional phrases.
Fold
 invalid-key-phrases
In
 write-statement-format-i
;

% Identified conditional phrases.
Fold
 invalid-key-phrases
In
 write-statement-format-iii
;

% Identified conditional phrases.
Fold
 invalid-key-phrases
In
 write-statement-format-iv
;

% Removed conditional phrases and explicit scope delimiter
Replace
 size-error-phrases
 "END-ADD"?
By
 /* epsilon */
;

% Removed conditional phrases and explicit scope delimiter
Replace
 on-overflow? "END-CALL"?
By
 /* epsilon */
In
 call-statement-format-i
;

% Removed conditional phrases and explicit scope delimiter
Replace
 exception-phrases "END-CALL"?
By
 /* epsilon */
In
 call-statement-format-ii
;

% Removed conditional phrases and explicit scope delimiter
Replace
 size-error-phrases
 "END-COMPUTE"?
By
 /* epsilon */
In
 compute-statement
;

% Removed conditional phrases and explicit scope delimiter
Replace
 invalid-key-phrases "END-DELETE"?
By
 /* epsilon */
In
 delete-statement
;

% Removed conditional phrases and explicit scope delimiter
Replace
 size-error-phrases
 "END-DIVIDE"?
By
 /* epsilon */
;

% Removed explicit scope delimiter
Replace
 "END-EVALUATE"?
By
 /* epsilon */
In
 evaluate-statement
;

% Removed explicit scope delimiter
Replace
 ("END-IF" @1)?
By
 /* epsilon */
In
 if-statement
;

% Removed conditional phrases and explicit scope delimiter
Replace
 size-error-phrases
 "END-MULTIPLY"?
By
 /* epsilon */
;

% Removed conditional phrases and explicit scope delimiter
Replace
 at-end-phrases "END-READ"?
By
 /* epsilon */
In
 read-statement-format-i
;

% Removed conditional phrases and explicit scope delimiter
Replace
 invalid-key-phrases "END-READ"?
By
 /* epsilon */
In
 read-statement-format-ii
;

% Removed explicit scope delimiter
Replace
 "END-RETURN"?
By
 /* epsilon */
In
 return-statement
;

% Removed conditional phrases and explicit scope delimiter
Replace
 invalid-key-phrases
 "END-REWRITE"?
By
 /* epsilon */
In
 rewrite-statement
;

% Removed explicit scope delimiter
Replace
 ( "END-SEARCH" @1 )?
By
 /* epsilon */
In
 search-statement-format-i
;

% Removed contained statements and explicit scope delimiter
Replace
 ( "END-SEARCH" @2 )?
By
 /* epsilon */
In
 search-statement-format-ii
;

% Removed conditional phrases and explicit scope delimiter
Replace
 invalid-key-phrases "END-START"?
By
 /* epsilon */
In
 start-statement
;

% Removed conditional phrases and explicit scope delimiter
Replace
 overflow-phrases "END-STRING"?
By
 /* epsilon */
In
 string-statement
;

% Removed conditional phrases and explicit scope delimiter
Replace
 size-error-phrases
 "END-SUBTRACT"?
By
 /* epsilon */
;

% Removed conditional phrases and explicit scope delimiter
Replace
 overflow-phrases "END-UNSTRING"?
By
 /* epsilon */
In
 unstring-statement
;

% Removed conditional phrases and explicit scope delimiter
Replace
 ( write-before-after @1 w-phrase | invalid-key-phrases )
 "END-WRITE"?
By
 /* epsilon */
In
 write-statement-format-i
;

% Eliminated option for omission of conditional phrases.
Nonempty
 size-error-phrases
;

% Eliminated option for omission of conditional phrases.
Nonempty
 exception-phrases
;

% Eliminated option for omission of conditional phrases.
Nonempty
 at-end-phrases
;

% Eliminated option for omission of conditional phrases.
Nonempty
 invalid-key-phrases
;

% Eliminated option for omission of conditional phrases.
Nonempty
 w-phrase
;

% Given up on earlier approximation.
Reject
 statements
;

% Statement sequences as in sentences and conditional phrases.
Add
 statements =

     accept-statement statements?

   | add-statement ( statements?
                   | "END-ADD" statements?
                   | size-error-phrases ( "END-ADD" statements? )?
                   )

   | alter-statement statements?

   | call-statement-format-i ( statements?
                             | "END-CALL" statements?
                             | on-overflow ( "END-CALL" statements? )?
                             )

   | call-statement-format-ii ( statements?
                              | "END-CALL" statements?
                              | exception-phrases ( "END-CALL" statements? )?
                              )

   | cancel-statement statements?

   | close-statement statements?

   | compute-statement ( statements?
                       | "END-COMPUTE" statements?
                       | size-error-phrases ( "END-COMPUTE" statements? )?
                       )

   | continue-statement statements?

   | delete-statement ( statements?
                      | "END-DELETE" statements?
                      | invalid-key-phrases ( "END-DELETE" statements? )?
                      )

   | display-statement statements?

   | divide-statement ( statements?
                      | "END-DIVIDE" statements?
                      | size-error-phrases ( "END-DIVIDE" statements? )?
                      )

   | entry-statement statements?

   | evaluate-statement ( "END-EVALUATE" statements? )?

   | exit-statement statements?

   | exit-program-statement statements?

   | goback-statement statements?

   | go-to-statement statements?

   | if-statement ( "END-IF" statements? )?

   | initialize-statement statements?

   | inspect-statement statements?

   | merge-statement statements?

   | move-statement statements?

   | multiply-statement ( statements?
                        | "END-MULTIPLY" statements?
                        | size-error-phrases ( "END-MULTIPLY" statements? )?
                        )

   | open-statement statements?

   | perform-statement statements?

   | read-statement-format-i ( statements?
                             | "END-READ" statements?
                             | at-end-phrases ( "END-READ" statements? )?
                             )

   | read-statement-format-ii ( statements?
                              | "END-READ" statements?
                              | invalid-key-phrases ( "END-READ" statements? )?
                              )

   | release-statement statements?

   | return-statement ( "END-RETURN" statements? )?

   | rewrite-statement ( statements? 
                       | "END-REWRITE" statements?
                       | invalid-key-phrases ( "END-REWRITE" statements? )?
                       )

   | search-statement ( "END-SEARCH" statements? )?

   | set-statement statements?

   | sort-statement statements?

   | start-statement ( statements? 
                     | "END-START" statements?
                     | invalid-key-phrases ( "END-START" statements? )?
                     )

   | stop-statement statements?

   | string-statement ( statements? 
                     | "END-STRING" statements?
                     | overflow-phrases ( "END-STRING" statements? )?
                     )

   | subtract-statement ( statements?
                        | "END-SUBTRACT" statements?
                        | size-error-phrases ( "END-SUBTRACT" statements? )?
                        )

   | unstring-statement ( statements? 
                        | "END-UNSTRING" statements?
                        | overflow-phrases ( "END-UNSTRING" statements? )?
                        )

   | write-statement-format-i 
       ( statements? 
       | "END-WRITE" statements?
       | write-before-after ( statements? 
                            | "END-WRITE" statements?
                            | w-phrase ( "END-WRITE" statements? )?
                            )
       | invalid-key-phrases ( "END-WRITE" statements? )?
       )

   | examine-statement statements?

To 
 1.2.3
;

% Relaxed nesting of conditional statements.
Replace 
 imperative-statement+
By
 statements
;

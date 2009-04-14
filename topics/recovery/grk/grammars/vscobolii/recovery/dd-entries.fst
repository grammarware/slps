/*

  This script comprehends all transformations to clean up the
  data description entries as used everywhere in the DATA DIVISION.
  The applied transformations identify some useful clauses/phrases,
  and they repair or relax a few phrases according to notes.

*/



% Different formats for data description entries.
Add
 data-description-entry = 
     data-description-entry-format-i
   | data-description-entry-format-ii
   | data-description-entry-format-iii
Before
 data-description-entry-format-i
;

% Identified ingredients of a permutation phrase.
Extract
 data-clauses =
   blank-when-zero-clause? \\
   external-clause?        \\
   global-clause?          \\
   justified-clause?       \\
   occurs-clause?          \\
   picture-clause?         \\
   sign-clause?            \\
   synchronized-clause?    \\
   usage-clause?           \\
   value-clause?
From
 data-description-entry-format-i
;

% Inserted separator period as required at the beginning of 2.7.
Replace
 data-clauses
By 
 data-clauses "."
In
 data-description-entry-format-i
;

% Identified the kind of value clause needed here.
Replace 
 value-clause
By
 value-clause-format-i
In
 data-clauses
;

% Identified the kind of value clause needed here.
Replace 
 value-clause
By
 value-clause-format-ii
In
 data-description-entry-format-iii
;

% Relaxed order as described in the below note.
Permute
 data-clauses
;

% Implemented text below the diagram.
Replace
 level-number (data-name-1 | "FILLER")? 
By
 /* epsilon */
In
 redefines-clause
;

% Different formats of ADD statements.
Add
 occurs-clause =
    occurs-clause-format-i
  | occurs-clause-format-ii
To 
 2.7.6
;

% Implemented note (1) below the diagram.
Replace
   integer-1 @1 "TO"
By 
 ( integer-1 @1 "TO" )?
In
 occurs-clause-format-ii
;

% Implemented note that 88 and condition-name-1 do not belong to clause.
Replace
 "88" condition-name-1
By
 /* epsilon */
In
 value-clause-format-ii
;

% Neither does the "." belong to the clause.
Replace
 "."
By
 /* epsilon */
In
 value-clause-format-ii
;

% Implemented note that 66 and data-name-1 do not belong to clause.
Replace
 "66" data-name-1
By
 /* epsilon */
In
 renames-clause
;

% Neither does the "." belong to the clause.
Replace
 "."
By
 /* epsilon */
In
 renames-clause
;

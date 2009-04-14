/*

  This script comprehends all transformations to clean up the
  file description entries as used in the FILE SECTION in 
  the DATA DIVISION. The applied transformations identify some
  useful clauses/phrases, and they relax syntax according to
  notes (e.g., by means of permutation phrases). A more
  involved activity is the derivation of a unified format for
  file description entries from the very specific, overlapping
  formats for sequential, indexed, and relative files.

*/



% Unified format for file (and sort) description entries.
Add
 file-description-entry =
   ( "FD" | "SD" ) file-name-1 file-clauses "."
To
 2.6
;

% Capture the above subtopics as permutation phrase.
Add 
 file-clauses =
      external-clause?
   || global-clause?
   || block-contains-clause?
   || record-clause?
   || label-records-clause?
   || value-of-clause?
   || data-records-clause?
   || linage-clause?
   || recording-mode-clause?
   || code-set-clause?
After
 file-description-entry
;

% Used elsewhere for folding.
Add
 external-clause = "IS"? "EXTERNAL" 
After
 file-clauses
;

% Factored out clause for conciseness.
Fold
 external-clause
In
 fd-entry-sequential-files
;

% Factored out clause for conciseness.
Fold
 external-clause
In
 fd-entry-relative-indexed-files
;

% Used elsewhere for folding.
Add
 global-clause = "IS"? "GLOBAL" 
After
 file-clauses
;

% Factored out clause for conciseness.
Fold
 global-clause
In
 fd-entry-sequential-files
;

% Factored out clause for conciseness.
Fold
 global-clause
In
 fd-entry-relative-indexed-files
;

% Used elsewhere for folding.
Add
 block-contains-clause =
   "BLOCK" "CONTAINS"? (integer-1 "TO")? integer-2 ("CHARACTERS" | "RECORDS")
After
 file-clauses
;

% Factored out clause for conciseness.
Fold
 block-contains-clause
In
 fd-entry-sequential-files
;

% Factored out clause for conciseness.
Fold
 block-contains-clause
In
 fd-entry-relative-indexed-files
; 

% Factored out clause for conciseness.
Fold
 block-contains-clause
In
 sd-entry-sort-merge-files
;

% Made ( "CHARACTERS" | "RECORDS" ) option to allow for default as of 2.6.4
Replace 
 ( "CHARACTERS" | "RECORDS" )
By
 ( "CHARACTERS" | "RECORDS" )?
In
 block-contains-clause
;

% Added RECORD as option for RECORDS as found in VS Cobol II code.
Replace
 "RECORDS"
By 
 "RECORDS" | "RECORD"
In
 block-contains-clause
;

% Used elsewhere for folding.
Add
 record-clause =
   "RECORD" ( "CONTAINS"? integer-3 "CHARACTERS"? 
            | "CONTAINS"? integer-4 "TO" integer-5 "CHARACTERS"?
            | seq-clause-a ("DEPENDING" "ON"? qualified-data-name-1)?
            )
After
 file-clauses
;

% Factored out clause for conciseness.
Fold
 record-clause
In
 fd-entry-sequential-files
;

% Used elsewhere for folding.
Add
 record-varying-clause =
   "IS"? "VARYING" "IN"? "SIZE"? \\
   ("FROM"? integer-6)? ("TO" integer-7)? "CHARACTERS"?
After
 file-clauses
;

% Factored out clause for conciseness.
Fold
 record-varying-clause
In
 seq-clause-a
;

% Factored out clause for conciseness.
Fold
 record-varying-clause
In
 ri-clause
;

% Factored out clause for conciseness.
Fold
 record-varying-clause
In
 sd-clause-a
;

% Inline clause for conciseness.
Unfold
 seq-clause-a
In
 record-clause
;

% Inline clause for conciseness.
Unfold
 ri-clause
In
 fd-entry-relative-indexed-files
;

% Inline clause for conciseness.
Unfold
 sd-clause-a
In
 sd-entry-sort-merge-files
;

% Not needed anymore as a result of refactoring.
Reject
 seq-clause-a
;

% Not needed anymore as a result of refactoring.
Reject
 ri-clause
;

% Not needed anymore as a result of refactoring.
Reject
 sd-clause-a
;

% Factored out clause for conciseness.
Fold
 record-clause
In
 fd-entry-relative-indexed-files
;

% Factored out clause for conciseness.
Fold
 record-clause
In
 sd-entry-sort-merge-files
;

% Captured most general format that is used in the above diagrams.
Add
 label-records-clause =
   "LABEL" ( "RECORD" "IS"?
           | "RECORDS" "ARE"?
           )
           ( "STANDARD"
           | "OMITTED"
           | { qualified-data-name-2+ } )
After
 file-clauses
;

% Factored out clause for conciseness.
Fold
 label-records-clause
In
 fd-entry-sequential-files
;

% Used elsewhere for folding.
Add
 value-of-clause =
   "VALUE" "OF" (system-name-1 "IS"? (qualified-data-name-3 | literal-1))+
After
  file-clauses
;

% Factored out clause for conciseness.
Fold 
 value-of-clause
In
 fd-entry-sequential-files
;

% Factored out clause for conciseness.
Fold 
 value-of-clause
In
 fd-entry-relative-indexed-files
;

% Factored out clause for conciseness.
Fold 
 value-of-clause
In
 sd-entry-sort-merge-files
;

% Used elsewhere for folding.
Add
 data-records-clause =
   "DATA" ("RECORD" "IS"? | "RECORDS" "ARE"?) qualified-data-name-4+
After
 file-clauses
;

% Factored out clause for conciseness.
Fold 
 data-records-clause
In
 fd-entry-sequential-files
;

% Factored out clause for conciseness.
Fold 
 data-records-clause
In
 fd-entry-relative-indexed-files
;

% Factored out clause for conciseness.
Fold 
 data-records-clause
In
 sd-entry-sort-merge-files
;

% Used elsewhere for folding.
Add
 linage-clause =
   "LINAGE" "IS"? (qualified-data-name-5 | integer-8)
   "LINES"? seq-clause-b
After
 file-clauses
;

% Factored out clause for conciseness.
Fold
 linage-clause
In 
 fd-entry-sequential-files
;

% Used elsewhere for folding.
Add
 linage-area-clause =
   ("WITH"? "FOOTING" "AT"? (data-name-6 | integer-9))? \\
   ("LINES"? "AT"? "TOP" (data-name-7 | integer-10))?   \\
   ("LINES"? "AT"? "BOTTOM" (data-name-8 | integer-11))?
After
 file-clauses
;

% Factored out clause for conciseness.
Fold
 linage-area-clause
In
 seq-clause-b
;

% Factored out clause for conciseness.
Fold
 linage-area-clause
In
 sd-clause-b
;

% Inline clause for conciseness.
Unfold
 seq-clause-b
In
 linage-clause
;

% Inline clause for conciseness.
Unfold
 sd-clause-b
In
 sd-entry-sort-merge-files
;

% Not needed anymore as a result of refactoring.
Reject
 seq-clause-b
;

% Not needed anymore as a result of refactoring.
Reject
 sd-clause-b
;

% Factored out clause for conciseness.
Fold
 linage-clause
In
 sd-entry-sort-merge-files
;

% Used elsewhere for folding.
Add
 recording-mode-clause =
  { "RECORDING" "MODE"? "IS"? mode }
After
 file-clauses
;

% Factored out clause for conciseness.
Fold
 recording-mode-clause
In 
 fd-entry-sequential-files
;

% Used elsewhere for folding.
Add
 code-set-clause =
  "CODE-SET" "IS"? alphabet-name
After
 file-clauses
;

% Factored out clause for conciseness.
Fold
 code-set-clause
In
 fd-entry-sequential-files
;

% Factored out clause for conciseness.
Fold
 code-set-clause
In
 sd-entry-sort-merge-files
;

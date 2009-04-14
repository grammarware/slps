/*

  This script comprehends all transformations to clean up the
  ENVIRONMENT DIVISION in the Cobol grammar. This concerns:

   - the SPECIAL-NAMES paragraph in the CONFIGURATION SECTION.
   - the FILE-CONTROL paragraph in the INPUT-OUTPUT SECTION.

  The applied transformations identify some useful clauses/phrases,
  and they relax syntax according to notes (e.g., by means of
  permutation phrases). A more involved activity is the derivation
  of a unified format for FILE-CONTROL entries from the
  very specific, overlapping formats for sequential, indexed,
  and relative files.

*/

% Identified body of permutation phrase.
Extract
 computer-paragraphs =
   source-computer-paragraph? \\ 
   object-computer-paragraph?
From
 configuration-section
;

% No note on immaterial order, but code exercises different orders. 
Permute
 computer-paragraphs
;

% Factored out clause for clarity.
Extract 
 environment-clause =
     environment-name-1 "IS"? mnemonic-name-1 
   | environment-name-2 ("IS"? mnemonic-name-2 snp-entry? | snp-entry)
From
 special-names-paragraph
;

% Factored out clause for clarity.
Extract 
 alphabet-clause =
   "ALPHABET" alphabet-name-1 "IS"? 
   ( "STANDARD-1"
   | "STANDARD-2"
   | "NATIVE"
   | "EBCDIC"
   | (literal-1 (("THROUGH" | "THRU") literal-2 | ("ALSO" literal-3)+)?)+
   )
From
 special-names-paragraph
;

% Factored out clause for clarity.
Extract 
 symbolic-clause =
   "SYMBOLIC" "CHARACTERS"?
   (symbolic-character-1+ ("ARE" | "IS") integer-1+)+
   ("IN" alphabet-name-2)?
From
 special-names-paragraph
;

% Relaxed ARE | IS to be optional as exercised by actual code.
Replace
   "ARE" | "IS"
By
 ( "ARE" | "IS" )?
In 
 symbolic-clause
;

% Factored out clause for clarity.
Extract 
 class-clause =
   "CLASS" class-name-1 "IS"?
   (literal-4 (("THROUGH" | "THRU") literal-5)?)+
From
 special-names-paragraph
;

% Factored out clause for clarity.
Extract 
 currency-clause =
   ( "CURRENCY" "SIGN"? "IS"? literal-6 )?
   ( "DECIMAL-POINT" "IS"? "COMMA" )?
From
 special-names-paragraph
;

% Identified body of permutation phrase.
Extract 
 special-names-clauses =
   environment-clause* \\
   alphabet-clause* \\
   symbolic-clause* \\
   class-clause* \\
   currency-clause
From
 special-names-paragraph
;

% No note on immaterial order, but code exercises different orders. 
Permute
 special-names-clauses
;

% Implemented Note (2) from 2.4.1.
Replace 
 file-control-paragraph
By
 file-control-paragraph?
In
 input-output-section
;

% Redirected to a single format for entries.
Replace 
  ( qsam-sam-vsam-sequential-file-control-entries
  | vsam-indexed-file-control-entries
  | vsam-relative-file-control-entries )
By
  file-control-entry
In
 file-control-paragraph
;

% Implemented Note (1) from 2.4.1.
Replace 
  file-control-entry
By
  file-control-entry?
In
 file-control-paragraph
;

% Start with SELECT/ASSIGN; leave other clauses for permutation phrase.
Add
 file-control-entry =
   select-clause assign-clause file-control-clauses "."
After
 file-control-paragraph
;

% Used elsewhere for folding.
Add
 select-clause = "SELECT" "OPTIONAL"? file-name-1
After
 file-control-paragraph
;

% Factored out clause for clarity.
Fold 
 select-clause
In
 qsam-sam-vsam-sequential-file-control-entries
;

% Factored out clause for clarity.
Fold
 select-clause
In 
 vsam-indexed-file-control-entries
;

% Factored out clause for clarity.
Fold
 select-clause
In
 vsam-relative-file-control-entries
;

% Used elsewhere for folding.
Add
 assign-clause = "ASSIGN" "TO"? (assignment-name-1 | literal-1)+
After
 file-control-paragraph
;

% Factored out clause for clarity.
Fold 
 assign-clause
In 
 qsam-sam-vsam-sequential-file-control-entries
;

% Factored out clause for clarity.
Fold
 assign-clause
In 
 vsam-indexed-file-control-entries
;

% Factored out clause for clarity.
Fold
 assign-clause
In
 vsam-relative-file-control-entries
;

% Permutation phrase for all possible file-control-clauses.
Add
 file-control-clauses =
      reserve-clause?
   || organisation-clause?
   || padding-character-clause?
   || record-delimiter-clause?
   || access-mode-clause?
   || key-clause?
   || password-clause?
   || status-clause?
After
 file-control-paragraph
;

% Used elsewhere for folding.
Add
 reserve-clause = 
   "RESERVE" integer ("AREA" | "AREAS")?
After
 file-control-paragraph
;

% Factored out clause for clarity.
Fold 
 reserve-clause
In
 qsam-sam-vsam-sequential-file-control-entries
;

% Factored out clause for clarity.
Fold
 reserve-clause
In 
 vsam-indexed-file-control-entries
;

% Factored out clause for clarity.
Fold
 reserve-clause
In
 vsam-relative-file-control-entries
;

% Centralised different organisations.
Add
 organisation-clause = 
   ("ORGANIZATION" "IS"?)?  ("SEQUENTIAL" | "INDEXED" | "RELATIVE")
After
 file-control-paragraph
;

% Used elsewhere for folding.
Add
 padding-character-clause =
   "PADDING" "CHARACTER"? "IS"? (qualified-data-name-5 | literal-1)
After
 file-control-paragraph
;

% Factored out clause for clarity.
Fold
 padding-character-clause
In
 qsam-sam-vsam-sequential-file-control-entries
;

% Used elsewhere for folding.
Add
 record-delimiter-clause =
   "RECORD" "DELIMITER" "IS"? ("STANDARD-1" | assignment-name-2)
After
 file-control-paragraph
;

% Factored out clause for clarity.
Fold
 record-delimiter-clause
In
 qsam-sam-vsam-sequential-file-control-entries
;

% Centralised and simplified different access modes
Add
 access-mode-clause = 
  ( "ACCESS" "MODE"? "IS"? )?
  ( "SEQUENTIAL"
  | "RANDOM"
  | "DYNAMIC"
  )
After 
 file-control-paragraph
;

% Centralised and simplified different key clauses; 
Add
 key-clause = 
   record-key | relative-key
After
 file-control-paragraph
; 

% Used elsewhere for folding.
Add
 password-clause =
   { "PASSWORD" "IS"? qualified-data-name-6 }
After 
 file-control-paragraph
;

% Factored out clause for clarity.
Fold 
 password-clause
In
 qsam-sam-vsam-sequential-file-control-entries
;

% Factored out clause for clarity.
Fold 
 password-clause
In
 vsam-indexed-file-control-entries
;

% Factored out clause for clarity.
Fold 
 password-clause
In
 vsam-relative-file-control-entries
;

% Factored out key phrase for clarity.
Extract
 record-key =
   "RECORD" "KEY"? \\
   "IS"? qualified-data-name-2 password-clause? \\
   idx-entry*
From
 vsam-indexed-file-control-entries
;

% Factored out key phrase for clarity.
Extract
 relative-key =
   "RELATIVE" "KEY"? "IS"? qualified-data-name-4
From
 vsam-relative-file-control-entries
;

% Used elsewhere for folding.
Add
 status-clause =
   "FILE"? "STATUS" "IS"? qualified-data-name-1
                      ( { qualified-data-name-8 } )? 
After 
 file-control-paragraph
;

% Factored out clause for clarity.
Fold 
 status-clause
In
 qsam-sam-vsam-sequential-file-control-entries
;

% Factored out clause for clarity.
Fold 
 status-clause
In
 vsam-indexed-file-control-entries
;

% Factored out clause for clarity.
Fold 
 status-clause
In
 vsam-relative-file-control-entries
;

% Inserted a line break.
Replace 
 record-key
By 
 \\ record-key
In 
 vsam-indexed-file-control-entries
;

% Implemented Note (1) given below the diagram.
Replace
 "RECORD" @1
By
 "RECORD"? @1
In
 idx-entry
;

% Implemented Note (1) given below the diagram.
Replace
 "ON" @1
By
 "ON"? @1
In
 qsam-or-sam-i-o-control-entries
;

% Implemented Note (2) given below the diagram.
Replace
 (file-name-4 @2)+
By
 (file-name-4 @2)*
In
 qsam-or-sam-i-o-control-entries
;

% Implemented Note (1) given below the diagram.
Replace
 "ON" @1
By
 "ON"? @1
In
 vsam-i-o-control-entries
;

% Implemented Note (2) given below the diagram.
Replace
 (file-name-4 @2)+
By
 (file-name-4 @2)*
In
 vsam-i-o-control-entries
;

% Implemented Note (1) given below the diagram.
Replace
 (file-name-4 @1)+
By
 (file-name-4 @1)*
In
 sort-merge-i-o-control-entries
;

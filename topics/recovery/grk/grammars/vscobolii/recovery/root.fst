/*

  This script comprehends all transformations to clean up the
  root-level structure in the Cobol grammar. This mainly concerns
  the overall division structure.

*/


% Implemented footnote on optional periods.
Replace
 "." @1
By
 "."? @1
In
 cobol-source-program
;

% Implemented footnote on optional periods.
Replace
 "." @1
By
 "."? @1
In
 nested-source-program
;

% Made "." optional for DATE-COMPILED as well.
Replace
 "DATE-COMPILED" "." 
By 
 "DATE-COMPILED" "." @1
In 
 identification-division
;

% Implemented footnote on optional periods.
Replace
 "." @1
By
 "."? @1
In
 identification-division
;

% Implemented footnote on optional periods.
Replace
 "." @1
By
 "."? @1
In
 program-id-paragraph
;

% Identified content of the division as referred to elsewhere.
Extract
 identification-division-content =
  ("AUTHOR" "."? @1 comment-entry*)? \\
  ("INSTALLATION" "."? @1 comment-entry*)? \\
  ("DATE-WRITTEN" "."? @1 comment-entry*)? \\
  ("DATE-COMPILED" "."? @1 comment-entry*)? \\
  ("SECURITY" "."? @1 comment-entry*)?
From
 identification-division
;

% Added obsolete remarks paragraph.
Replace
 ("SECURITY" "."? @1 comment-entry*)?
By
 ("SECURITY" "."? @1 comment-entry*)? \\
 ("REMARKS" "." comment-entry*)?
In
 identification-division-content
;

% Implemented note on optional paragraphs.
Permute
 identification-division-content
;

% Identified content of the division as referred to elsewhere.
Extract
 environment-division-content =
   configuration-section?
   input-output-section?
From
 environment-division
;

% Identified content of the division as referred to elsewhere.
Extract 
 data-division-content =
   ("FILE" "SECTION" "." (file-description-entry record-description-entry+)*)? \\
   ("WORKING-STORAGE" "SECTION" "." (record-description-entry | data-item-description-entry)*)? \\
   ("LINKAGE" "SECTION" "." (record-description-entry | data-item-description-entry)*)?
From
 data-division
;

% record-description-entry as a form of data-description-entry (see beginning of 2.7).
Add 
 record-description-entry = data-description-entry
After
 data-division-content
;

% data-item-description-entry as a form of data-description-entry (see beginning of 2.7).
Add 
 data-item-description-entry = data-description-entry
After
 data-division-content
;

% record-... and data-item-description-entry treated equally.
Replace 
 record-description-entry | data-item-description-entry
By
 data-description-entry
In
 data-division-content
;

% Enable Cobol subprograms
Replace 
 "PROCEDURE" "DIVISION" "."
By
 "PROCEDURE" "DIVISION" using-phrase? "."
In
 cobol-source-program
;

% Enable Cobol subprograms
Replace 
 "PROCEDURE" "DIVISION" "."
By
 "PROCEDURE" "DIVISION" using-phrase? "."
In
 nested-source-program
;

% Identified USING phrases as referred to elsewhere.
Extract
 using-phrase = "USING" data-name-1+
From
 procedure-division-format-i
;

% Fold USING phrase as referred to elsewhere.
Fold
 using-phrase
In
 procedure-division-format-ii
;

% Identified section structure for clarity.
Extract
 section = 
   section-name @1 "SECTION" priority-number? "." para
From
 procedure-division-format-i
;

% Identified relevant part for procedure-division-content
Extract 
 procedure-division-content = 
   ("DECLARATIVES" "." (sect "." use-statement "." para)+
    "END" "DECLARATIVES" ".")? \\
   section+
From
 procedure-division-format-i
;

% Enforced separation of directives vs. statements
Replace
 use-statement
By
 use-directive
In 
 procedure-division-content
;

% Stretched below note (1): entire section header is optional.
Replace
 section+
By 
 para? section*
In 
 procedure-division-content
;

% Applied Note (1) for 2nd format to 1st format as well.
Replace 
 (paragraph-name "." sentence*)+
By 
 sentence* (paragraph-name "." sentence*)* 
In 
 para
;

% 1 imperative statement = many imperative statements (see 2.8.6.1)
Replace
 imperative-statement
By
 series-of-imperative-statements
;

% Implemented series of imperative statements.
Add
 series-of-imperative-statements = imperative-statement+
To
 2.8.6.1
;

% Implemented statement according to given description; 2.8.6.4 omitted.
Add
 statement = 
     imperative-statement
   | conditional-statement
To 
 1.2.4
;

% Implemented statement according to given description; 2.8.6.4 omitted.
Add
 imperative-statement =
     unconditional-statement
   | delimited-scope-statement
To 
 1.2.4
;

% Implemented sentence.
Add
 sentence = statements "."
To 
 1.2.3
;

% Restricted statement sequences in IF statements
Replace
 statement+
By
 statements
In
 if-statement
;

% Needed for sentences and IF-statements.
Add
 statements =
     delimited-scope-statement statements?
   | conditional-statement
   | unconditional-statement statements?
To 
 1.2.3
;

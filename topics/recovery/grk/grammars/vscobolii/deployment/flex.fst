/*

This script makes the grammar fit to work with a very simple
FLEX-based scanner as done in the BTYACC subdirectory. Most notably,
all kinds of Cobol words are not properly distinguished. Also, level
numbers are just numeric literals. More advanced stuff is done later
in the script; see the comments there. (As an aside, this script is
rather similar to finalize.trafo as it comes with the VS COBOL II
distribution for GDK.)

*/

% Do not enforce fine differences for different kinds of Cobol words 
Add 
 alphabetic-user-defined-word = cobol-word
To
 1.1.1.2
;

% Do not enforce fine differences for different kinds of Cobol words 
Add 
 system-name = cobol-word
To
 1.1.1.2
;

% Do not enforce fine differences for different kinds of Cobol words 
Add 
 user-defined-word = cobol-word
To
 1.1.1.2
;

% Do not enforce precise lexical syntax of integers
Add 
 integer = numeric
To
 1.1.1.2
;

% Do not enforce precise lexical syntax of level numbers
Add 
 level-number = numeric
To
 1.1.1.2
;

% Do not enforce precise lexical syntax of priority numbers
Add 
 priority-number = numeric
To
 1.1.1.2
;

% Remove definition of mode which would treat F, V, ... as keywords
Reject mode;

% Be very liberal about modes
Add
 mode = cobol-word
To 
 2.6.10
;

% Reflect that user-defined-word will not cover numeric Cobol words 
Extend 
 paragraph-name
By
 integer
;

% Reflect that user-defined-word will not cover numeric Cobol words 
Extend 
 section-name
By
 integer
;

% Establish a single format for data decription entries
Replace
 value-clause-format-i
By 
   value-clause-format-ii
 | renames-clause
In 
 data-clauses
;

% Remove simple VALUE clauses
Reject value-clause-format-i;

% Remove format "88" 
Reject data-description-entry-format-ii;

% Remove format "66"
Reject data-description-entry-format-iii;

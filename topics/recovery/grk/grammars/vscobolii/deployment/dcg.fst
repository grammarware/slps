% Resolved that length/2 is reserved in Prolog.
Rename
 length
To
 mod-length
;

% Added cut to limit backtracking.
Replace
 "."
By 
 "." !
In
 cobol-source-program
;

% Added cut to limit backtracking.
Replace
 "."
By 
 "." !
In
 file-description-entry
;

% Added cut to limit backtracking.
Replace
 "."?
By 
 ("." !)?
In
 data-description-entry-format-i
;

% Reordered OR in the view of cut.
Replace
 occurs-clause-format-i | occurs-clause-format-ii
By
 occurs-clause-format-ii | occurs-clause-format-i
In
 occurs-clause
;

% Added cut to limit backtracking.
Replace
 "."?
By 
 ("." !)?
In
 data-description-entry-format-ii
;

% Added cut to limit backtracking.
Replace
 "."?
By 
 ("." !)?
In
 data-description-entry-format-iii
;

% Added cut to limit backtracking.
Replace
 "."
By 
 "." !
In
 sentence
;

% Added helper phrase for guarded cuts.
Add 
 statement-verb =
     "ACCEPT"
   | "ADD"
   | "ALTER"
   | "CALL"
   | "CANCEL"
   | "CLOSE"
   | "COMPUTE"
   | "CONTINUE"
   | "DELETE"
   | "DISPLAY"
   | "DIVIDE"
   | "ENTRY"
   | "EVALUATE"
   | "EXIT"
   | "GOBACK"
   | "GO"
   | "IF"
   | "INITIALIZE"
   | "INSPECT"
   | "MERGE"
   | "MOVE"
   | "MULTIPLY"
   | "OPEN"
   | "PERFORM"
   | "READ"
   | "RELEASE"
   | "RETURN"
   | "REWRITE"
   | "SEARCH"
   | "SET"
   | "SORT"
   | "START"
   | "STOP"
   | "STRING"
   | "SUBTRACT"
   | "UNSTRING"
   | "WRITE"
   | "EXAMINE"
   | "COPY"
After
 statements
;

% Added cut to limit backtracking in conditions.
Replace
 "THEN"?
By
 "THEN"? ( statement-verb | "NEXT" "SENTENCE" )# !
In
 if-statement
;

% Added cut to limit backtracking in statement sequences.
Replace
 statements
By
 statement-verb# ! statements
In
 statements
;

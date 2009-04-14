% Added context-free position of COPY statement.
Extend 
 data-description-entry
By 
 copy-directive
;

% Added context-free position of COPY statement.
Replace 
 using-phrase? "."
By
   using-phrase? "." 
 | copy-directive
 | "USING" data-name* copy-directive
;

% Added context-free position of COPY statement.
Extend 
 statements
By
 copy-directive statements?
;

% Added context-free position of COPY statement.
Replace 
 call-using-phrase
By
 call-using-phrase copy-directive-without-period?
In
 call-statement-format-i
;

% Added context-free position of COPY statement.
Replace 
 call-using-phrase
By
 call-using-phrase copy-directive-without-period?
In
 call-statement-format-ii
;

% Enabled interpretation of "." in COPY as end-of sentence.
Extract
 copy-directive-without-period =
   "COPY" ( text-name
          | { literal-1 }
          )
          ( ("OF" | "IN") ( library-name 
                          | { literal-2 }
                          )
            )? \\
            ( { "SUPPRESS" } )? \\
            ( "REPLACING" ( copy-operand-1 "BY" copy-operand-2 )+ )?
From
 copy-directive
;

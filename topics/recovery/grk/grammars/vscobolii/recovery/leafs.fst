/*

  This script comprehends all transformations to clean up the
  leaf-level structure in the Cobol grammar. This concerns:

   - procedure names
   - identifiers (as means of data reference)
   - the use of data-name vs. qualified-data-name
   - the use of condition-name vs. condition-name-reference
   - the resolution of aliases
   - the resolution of lexicals
   - the syntax of arithmetic and conditional expressions
   - the syntax of COPY statements

*/


% Names as used in GO TOs and PERFORMS.
Add 
 procedure-name =
    reference-to-procedure-division-name-format-i
  | reference-to-procedure-division-name-format-ii
To
 1.5.1.1.3
;

% Combined below formats and addendum in 1.1.1.9.
Add
 identifier =
     identifier-format-i
   | identifier-format-ii
   | special-register
Before
 identifier-format-i
;

% Made explicit qualified data names as part of identifiers.
Extract
 qualified-data-name =
   data-name-1 (("IN" | "OF") data-name-2)* (("IN" | "OF") file-name-1)?
From
 identifier-format-i
;

% Moved iteration inside parentheses as in subscripting diagram.
Replace
 ("(" subscript ")")+
By
 "(" subscript+ ")"
In
 identifier-format-i
;

% Identified phrase as referred to elsewhere.
Extract
 subscript =
     integer-1 
   | data-name-2  (("+" | "-") integer-2)?
   | index-name-1 (("+" | "-") integer-3)?
From
 subscripting
;

% Enabled identifiers instead of plain data names.
Replace
 data-name
By
 identifier
In
 subscript
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In
 qsam-sam-vsam-sequential-file-control-entries
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In
 vsam-indexed-file-control-entries
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In 
 idx-entry
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In
 vsam-relative-file-control-entries 
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In
 fd-entry-sequential-files
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In
 fd-entry-relative-indexed-files
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In
 sd-entry-sort-merge-files
;

% Enabled qualified instead of plain data names triggered by actual code.
Replace
 data-name-1
By
 qualified-data-name-1
In
 occurs-clause-format-ii
;

% Enabled qualified instead of plain data names.
Replace
 data-name-2
By
 qualified-data-name-2
In
 renames-clause
;

% Enabled qualified instead of plain data names.
Replace
 data-name-3
By
 qualified-data-name-3
In
 renames-clause
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In
 merge-statement
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In
 read-statement-format-ii
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In
 search-statement-format-ii
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In
 sort-statement
;

% Enabled qualified instead of plain data names.
Replace
 data-name
By
 qualified-data-name
In
 start-statement
;

% Generalised qualified-data-name to identifier triggered by actual code.
Replace
 qualified-data-name
By
 identifier
In
 search-statement-format-ii
;

% Joined different formats for condition-name-references.
Add
 condition-name-reference =
     condition-name-in-data-division
   | condition-name-in-special-names-paragraph
To
 1.5.1.3
;

% Allowed for proper condition-name reference not just names.
Replace
 condition-name
By
 condition-name-reference
In
 condition-name-condition
;

% Allowed for proper condition-name reference not just names.
Replace
 condition-name
By
 condition-name-reference
In
 switch-status-condition
;

% Allowed for proper condition-name reference not just names.
Replace
 condition-name
By
 condition-name-reference
In
 search-statement-format-ii
;

% Allowed for proper condition-name reference not just names.
Replace
 condition-name
By
 condition-name-reference
In
 set-statement-format-iv
;

% Implemented as required in the text.
Add
 leftmost-character-position = arithmetic-expression
To
 1.5.1.5
;

% Implemented as required in the text.
Add
 length = arithmetic-expression 
To
 1.5.1.5
;

% Implemented in accordance with the above table.
Add 
 alphabet-name = alphabetic-user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 class-name = alphabetic-user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 condition-name = alphabetic-user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 data-name = alphabetic-user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 file-name = alphabetic-user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 index-name = alphabetic-user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 mnemonic-name = alphabetic-user-defined-word
To
 1.1.1.2
;

% Error tables; record names can be qualified.
Add 
 record-name = qualified-data-name
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 routine-name = alphabetic-user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 symbolic-character = alphabetic-user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 library-name = user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 program-name = user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 text-name = user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 paragraph-name = user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above table.
Add 
 section-name = user-defined-word
To
 1.1.1.2
;

% Implemented in accordance with the above description.
Add
 computer-name = system-name
To
 1.1.1.3
;

% Implemented in accordance with the above description.
Add
 language-name = system-name
To
 1.1.1.3
;

% Implemented in accordance with the above description.
Add
 implementor-name = environment-name | assignment-name
To
 1.1.1.3
;

% Implemented in accordance with the above description.
Add
 environment-name = system-name
To
 1.1.1.3
;

% Implemented in accordance with the above description.
Add
 assignment-name = system-name
To
 1.1.1.3
;

% Implemented as required in the below text.
Add
 basis-name = program-name
After
 basis-directive
;

% Implemented in accordance with the above enumeration.
Add 
 figurative-constant =
     "ZERO"
   | "ZEROS"
   | "ZEROES"
   | "SPACE"
   | "SPACES"
   | "HIGH-VALUE"
   | "HIGH-VALUES"
   | "LOW-VALUE"
   | "LOW-VALUES"
   | "QUOTE"
   | "QUOTES"
   | "ALL" literal
   | "NULL"
   | "NULLS"
To 
 1.1.1.8
;

% Implemented in accordance with the above text.
Add 
 literal =
     nonnumeric
   | numeric
   | dbcs
   | figurative-constant
To
 1.1.1.10
;

% Implemented in accordance with the above enumeration.
Add
 special-register =
     "ADDRESS" "OF" data-name
   | "DEBUG-ITEM"
   | "LENGTH" "OF" identifier
   | "RETURN-CODE"
   | "SHIFT-OUT"
   | "SHIFT-IN"
   | "SORT-CONTROL"
   | "SORT-CORE-SIZE"
   | "SORT-FILE-SIZE"
   | "SORT-MESSAGE"
   | "SORT-MODE-SIZE"
   | "SORT-RETURN"
   | "TALLY"
   | "WHEN-COMPILED"
To
 1.1.1.9
;

% Defined modes as described in text; note: modes are not reserved words.
Add
 mode = "F"|"V"|"U"|"S"
To 
 2.6.10
;

% Top layer of arithmetic expressions.
Add
 arithmetic-expression =
   times-div  ( ("+" | "-") times-div )*
To
 2.8.4
;

% 2nd layer of arithmetic expressions.
Add
 times-div =
   power ( ("*" | "/") power )*
To
 2.8.4
;

% 3rd layer of arithmetic expressions.
Add
 power =  ( "+" | "-" )? basis ( "**" basis )*
To
 2.8.4
;

% Bottom layer of arithmetic expressions.
Add
 basis =
     identifier
   | literal
   | "(" arithmetic-expression ")"
To
 2.8.4
;

% Implemented definition as given in the text.
Add
 condition =
   combinable-condition | combined-condition
To
 2.8.5
;

% Implemented definition as given in the text.
Add
 combinable-condition =
     simple-condition
   | negated-simple-condition
   | abbreviated-combined-relation-condition
To 
 2.8.5
;

% Implemented definition as given in the text.
Add
 simple-condition = 
     class-condition
   | condition-name-condition
   | relation-condition
   | sign-condition
   | switch-status-condition 
   | "(" condition ")"
To
 2.8.5.1
;

% Required combinable conditions rather than any kind of condition.
Replace
 condition
By
 combinable-condition
In 
 combined-condition
;

% Implemented in accordance with the below text.
Add
 operand = arithmetic-expression
After
 relation-condition
;

% Enabled reuse for elsewhere.
Extract
 relational-operator =
  "IS"? ( "NOT"? ( "GREATER" "THAN"?
                 | ">" 
                 | "LESS" "THAN"?
                 | "<" 
                 | "EQUAL" "TO"?
                 | "="
                 )
        | "GREATER" "THAN"? "OR" "EQUAL" "TO"? 
        | ">=" 
        | "LESS" "THAN"? "OR" "EQUAL" "TO"? 
        | "<="
        )
From 
 relation-condition
;

% Implemented by reading between the lines.
Add
 object = arithmetic-expression
After
 abbreviated-combined-relation-condition
;

% Enabled subsequent reuse.
Extract
 abbreviation-rest =
   (("AND" | "OR") "NOT"? relational-operator? object)+
From
 abbreviated-combined-relation-condition
;

% Enabled some uses of parentheses.
Replace
 object
By
 object | "(" object abbreviation-rest ")"
In
 abbreviation-rest
;

% Enabled some more uses parentheses.
Replace
 relation-condition abbreviation-rest
By
   relation-condition abbreviation-rest
 | arithmetic-expression relational-operator
   "(" "NOT"? arithmetic-expression abbreviation-rest ")"
 | arithmetic-expression 
   "(" "NOT"? relational-operator?
       arithmetic-expression
       abbreviation-rest 
   ")" 
In
 abbreviated-combined-relation-condition
;

% Derived definition from 3rd and 4th bullet in 3.13.2
Add
 expression = arithmetic-expression | condition
After
 evaluate-statement
;

% expression-1/2 became arithmetic-expression | condition.
Unfold
 expression
In 
 evaluate-statement
;

% Obsolete as a result of unfolding in evaluate-statement. 
Reject
 expression
;

% Resolved ambiguous use of `operand'.
Replace
 operand
By 
 copy-operand
In
 copy-directive
; 

% Implemented in accordance with the below text.
Add
 copy-operand =
     quoted-pseudo-text
   | identifier
   | literal
   | cobol-word
After
 copy-directive
;

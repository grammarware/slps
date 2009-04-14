% Not favoured as start symbol.
Reject sequence-of-cobol-source-programs;

% Not referred to anywhere.
Reject implementor-name;

% Not referred to anywhere.
Reject language-name;

% Also part of COPY statement syntax.
Reject reference-to-copy-library;

% Also part of identifier syntax.
Reject reference-modification;

% Also part of identifier syntax.
Reject simple-data-reference;

% Also part of identifier syntax.
Reject subscripting;

% Only content needed, not complete division.
Reject identification-division;

% Covered by other diagrams.
Reject program-id-paragraph;

% Only content needed, not complete division.
Reject environment-division;

% Generalised in file-control-entry.
Reject qsam-sam-vsam-sequential-file-control-entries;

% Generalised in file-control-entry.
Reject vsam-indexed-file-control-entries;

% Generalised in file-control-entry.
Reject vsam-relative-file-control-entries;

% Only content needed, not complete division.
Reject data-division;

% Covered by other diagrams.
Reject data-at-level;

% Not needed because of generalised form.
Reject record-clause-format-i;

% Not needed because of generalised form.
Reject record-clause-format-ii;

% Not needed because of generalised form.
Reject record-clause-format-iii;

% Generalised in file-description-entry.
Reject fd-entry-sequential-files;

% Generalised in file-description-entry.
Reject fd-entry-relative-indexed-files;

% Generalised in file-description-entry.
Reject sd-entry-sort-merge-files;

% data-description-entry used instead everywhere.
Reject data-item-description-entry;

% Special instance of format-i.
Reject value-clause-format-iii;

% Only content needed, not complete division.
Reject procedure-division-format-i;

% Only content needed, not complete division.
Reject procedure-division-format-ii;

% Covered by other diagrams.
Reject procedure-division-header;

% Special instance of condition syntax.
Reject comparing-pointer-data-items;

% Repetition of some statement forms.
Reject into-from-identifier-phrase;

% Definition entangled in statement sequences.
Reject statement;

% Definition entangled in statement sequences.
Reject imperative-statement;

% Relevant part covered by altered-go-to instead.
Reject go-to-statement-format-iii;

% Definition entangled in statement sequences.
Reject write-statement;

% Definition entangled in statement sequences.
Reject write-statement-format-ii;

% Definition entangled in statement sequences.
Reject write-statement-format-iii;

% Definition entangled in statement sequences.
Reject write-statement-format-iv;

% Definition entangled in statement sequences.
Reject read-statement;

% Definition entangled in statement sequences.
Reject call-statement;

% Ignored Cobol-processing statements.
Reject compiler-directing-statement;

% Ignored Cobol-processing statement.
Reject basis-directive;

% Only needed in rejected basis statement.
Reject basis-name;

% Ignored Cobol-processing statement.
Reject cbl-process-directive;

% Ignored Cobol-processing statement.
Reject control-cbl-directive;

% Ignored Cobol-processing statement.
Reject delete-directive;

% Ignored Cobol-processing statement.
Reject eject-directive;

% Ignored Cobol-processing statement.
Reject enter-directive;

% Only needed in rejected enter statement.
Reject routine-name;

% Ignored Cobol-processing statement.
Reject insert-directive;

% Ignored Cobol-processing statement.
Reject ready-reset-trace-directive;

% Ignored Cobol-processing statement.
Reject replace-directive;

% Ignored Cobol-processing statement.
Reject replace-directive-format-i;

% Ignored Cobol-processing statement.
Reject replace-directive-format-ii;

% Ignored Cobol-processing statement.
Reject service-label-directive;

% Ignored Cobol-processing statement.
Reject service-reload-directive;

% Ignored Cobol-processing statement.
Reject skip123-directive;

% Ignored Cobol-processing statement.
Reject title-directive;

%
% This is the only test program coming with the GRK distribution.
% We cannot include real-world portfolios for legal reasons.
% If you want to parse your own portfolios, you might need to 
% pre-process your sources because the parser does not deal with
% continuation lines. There is restricted support for context-free
% treatment of COPY statements, i.e., it is maybe possible to parse
% your sources without performing copy-book expansion. As we illustrate
% with this text, the parser understands line comments that must be
% started with "%". This is a non-Cobol convention which is however
% useful for a separation of preprocessing and parsing.
% 

ID DIVISION.                                                      
 PROGRAM-ID. MY-TEST-PROGRAM.
 
ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
  FILE-CONTROL.                                                     
   SELECT FILE1 ASSIGN DA-R-INOUT00.                             
                                                                   
DATA DIVISION.

 FILE SECTION.
  FD  FILE1512
      LABEL RECORD STANDARD.
  01  DATAREC PIC X(1968).

 WORKING-STORAGE SECTION.
  01 ITEM-A PIC 99. 
  COPY MY-COPY-BOOK.

 PROCEDURE DIVISION.                          

  42.
     MOVE 'I AM AT POINT 42.' TO ERROR-FIELD.
     MOVE ZERO TO RESULT IN RESULT-FIELD.
     CALL 'CRASH' USING ERROR-FIELD
                        RESULT-FIELD.
     STOP RUN.


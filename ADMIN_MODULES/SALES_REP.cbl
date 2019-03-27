*>****************************************************************
*> Author: Joseph Warren
*> Date: 3/15/2019
*> Purpose: Generate and display a report for the last period's sales
*> Tectonics: cobc
*>*****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. SALES_REP.
ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
    FILE-CONTROL.
    SELECT OPTIONAL INDEX-FILE-SALE
        ASSIGN TO '../../INDEXES/SALE.IDX'
        ORGANIZATION IS INDEXED
        ACCESS IS RANDOM
        RECORD KEY IS IDX-saleID
        ALTERNATE RECORD KEY IS IDX-carVIN WITH DUPLICATES
        ALTERNATE RECORD KEY IS IDX-empID WITH DUPLICATES
        ALTERNATE RECORD KEY IS IDX-custID WITH DUPLICATES.

DATA DIVISION.
FILE SECTION.
FD INDEX-FILE-SALE
        RECORD CONTAINS 161 CHARACTERS.
    COPY SALE_DEF REPLACING ==:TAG:== BY ==IDX==.

WORKING-STORAGE SECTION.
01 WS-KEY PIC X.
01 WS-currentDate.
   05 WS-year   PIC 9(4).
   05 WS-month  PIC 9(2).
   05 WS-day    PIC 9(2).

COPY EMP_DEF REPLACING ==:TAG:== BY ==WSE==.
COPY SALE_DEF REPLACING ==:TAG:== BY ==WSS==.

LINKAGE SECTION.
01 PARAMETRES.
   02 PA-RETURN-CODE PIC 99 VALUE 0.

SCREEN SECTION.
01 SALES-REPORT BLANK SCREEN
   FOREGROUND-COLOR 7 BACKGROUND-COLOR 0 ERASE SCREEN.
   05 TITLE-BAR FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
      10 VALUE SPACES PIC X(120).
      10 VALUE "SALES REPORT" LINE 1 COL 50.

   05 VALUE "  SALES REPORT"     LINE 3 COL 10.
   05 VALUE "----------------"   LINE 4 COL 10.

   05 VALUE "                    "        LINE 5 COL 10.

   05 VALUE "DATE: "                      LINE 6 COL 10.
   05 D-saleDate LINE 6 COL 15.
      10 D-year   PIC 9(4) FROM WSS-year.
      10 FILLER   VALUE "-".
      10 D-month  PIC 9(2) FROM WSS-month.
      10 FILLER   VALUE "-".
      10 D-day    PIC 9(2) FROM WSS-day.

   05 VALUE "PRICE: "                     LINE 6 COL 29.
   05 D-price FROM WSS-price              LINE 6 COL 38.

   05 VALUE "MILEAGE: "                   LINE 6 COL 50.
   05 D-mileage FROM WSS-mileage          LINE 6 COL 59.

   05 VALUE "SALE ID: "                   LINE 7 COL 10.
   05 D-saleID PIC X(10) FROM WSS-saleID  LINE 7 COL 19.

   05 VALUE "VIN ID: "                    LINE 7 COL 31.
   05 D-carVIN PIC X(10) FROM WSS-carVIN  LINE 7 COL 40.

   05 VALUE "CUST ID: "                   LINE 7 COL 53.
   05 D-custID PIC X(10) FROM WSS-custID  LINE 7 COL 62.

   05 VALUE "EMP ID: "                    LINE 7 COL 75.
   05 D-empID PIC X(10) FROM WSS-empID    LINE 7 COL 83.

   05 VALUE "ESC - RETURN TO SELECTION" LINE 19 COL 35.

PROCEDURE DIVISION USING PARAMETRES.
MAIN-PROCEDURE.
   *>MOVE 12345 TO WSS-saleID
   MOVE "20000101" TO WSS-saleDate
   CALL 'READ_SALE' USING WSS-Sale
   DISPLAY SALES-REPORT

   PERFORM
      ACCEPT WS-KEY
         WITH NO ECHO
         AUTO-SKIP
      MOVE FUNCTION UPPER-CASE(WS-KEY) TO WS-KEY
      EVALUATE COB-CRT-STATUS
         WHEN 2005 EXIT PERFORM
      END-EVALUATE
   END-PERFORM
   MOVE 0 TO PA-RETURN-CODE
EXIT PROGRAM.
END PROGRAM SALES_REP.

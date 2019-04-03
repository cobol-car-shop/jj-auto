*>****************************************************************
*> Author: Joseph Warren
*> Date: 3/15/2019
*> Purpose: Generate and display a report for the last period's sales
*> Tectonics: cobc
*>*****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. SALES_REP.
ENVIRONMENT DIVISION.

DATA DIVISION.
FILE SECTION.

WORKING-STORAGE SECTION.
01 WS-KEY PIC X.
01 WS-currentDate.
   05 WS-year   PIC 9(4).
   05 WS-month  PIC 9(2).
   05 WS-day    PIC 9(2).

01 WS-lastPageLookup.
   05 WS-LP-saleId   PIC 9(5).
   05 WS-LP-saleDate PIC 9(8).

01 WS-COUNTER       PIC 99.

COPY EMP_DEF REPLACING ==:TAG:== BY ==WSE==.
COPY SALE_DEF REPLACING ==:TAG:== BY ==WSS0==.
COPY SALE_DEF REPLACING ==:TAG:== BY ==WSS1==.
COPY SALE_DEF REPLACING ==:TAG:== BY ==WSS2==.
COPY SALE_DEF REPLACING ==:TAG:== BY ==WSS3==.
COPY SALE_DEF REPLACING ==:TAG:== BY ==WSS4==.

LINKAGE SECTION.
01 PARAMETRES.
   02 PA-RETURN-CODE PIC 99 VALUE 0.
   02 PA-OPTION-CODE PIC 99 VALUE 0.
   02 PA-LAST-SALEID PIC 9(5).
   02 PA-LAST-DATE.
      05 PA-YEAR   PIC 9(4).
      05 PA-MONTH  PIC 9(2).
      05 PA-DAY    PIC 9(2).

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
   05 D0-saleDate LINE 6 COL 15.
      10 D0-year   PIC 9(4) FROM WSS0-year.
      10 FILLER   VALUE "-".
      10 D0-month  PIC 9(2) FROM WSS0-month.
      10 FILLER   VALUE "-".
      10 D0-day    PIC 9(2) FROM WSS0-day.
   05 VALUE "PRICE: "                        LINE 6 COL 29.
   05 D0-price FROM WSS0-price               LINE 6 COL 38.
   05 VALUE "MILEAGE: "                      LINE 6 COL 50.
   05 D0-mileage FROM WSS0-mileage           LINE 6 COL 59.
   05 VALUE "SALE ID: "                      LINE 7 COL 10.
   05 D0-saleID PIC X(10) FROM WSS0-saleID   LINE 7 COL 19.
   05 VALUE "VIN ID: "                       LINE 7 COL 31.
   05 D0-carVIN PIC X(10) FROM WSS0-carVIN   LINE 7 COL 40.
   05 VALUE "CUST ID: "                      LINE 7 COL 53.
   05 D0-custID PIC X(10) FROM WSS0-custID   LINE 7 COL 62.
   05 VALUE "EMP ID: "                       LINE 7 COL 75.
   05 D0-empID PIC X(10) FROM WSS0-empID     LINE 7 COL 83.

   05 VALUE "                    "           LINE 8 COL 10.

   05 VALUE "DATE: "                         LINE 9 COL 10.
   05 D1-saleDate LINE 9 COL 15.
      10 D1-year   PIC 9(4) FROM WSS1-year.
      10 FILLER   VALUE "-".
      10 D1-month  PIC 9(2) FROM WSS1-month.
      10 FILLER   VALUE "-".
      10 D1-day    PIC 9(2) FROM WSS1-day.
   05 VALUE "PRICE: "                        LINE 9 COL 29.
   05 D1-price FROM WSS1-price               LINE 9 COL 38.
   05 VALUE "MILEAGE: "                      LINE 9 COL 50.
   05 D1-mileage FROM WSS1-mileage           LINE 9 COL 59.
   05 VALUE "SALE ID: "                      LINE 10 COL 10.
   05 D1-saleID PIC X(10) FROM WSS1-saleID   LINE 10 COL 19.
   05 VALUE "VIN ID: "                       LINE 10 COL 31.
   05 D1-carVIN PIC X(10) FROM WSS1-carVIN   LINE 10 COL 40.
   05 VALUE "CUST ID: "                      LINE 10 COL 53.
   05 D1-custID PIC X(10) FROM WSS1-custID   LINE 10 COL 62.
   05 VALUE "EMP ID: "                       LINE 10 COL 75.
   05 D1-empID PIC X(10) FROM WSS1-empID     LINE 10 COL 83.

   05 VALUE "DATE: "                         LINE 12 COL 10.
   05 D2-saleDate LINE 12 COL 15.
      10 D2-year   PIC 9(4) FROM WSS2-year.
      10 FILLER   VALUE "-".
      10 D2-month  PIC 9(2) FROM WSS2-month.
      10 FILLER   VALUE "-".
      10 D2-day    PIC 9(2) FROM WSS2-day.
   05 VALUE "PRICE: "                        LINE 12 COL 29.
   05 D2-price FROM WSS2-price               LINE 12 COL 38.
   05 VALUE "MILEAGE: "                      LINE 12 COL 50.
   05 D2-mileage FROM WSS2-mileage           LINE 12 COL 59.
   05 VALUE "SALE ID: "                      LINE 13 COL 10.
   05 D2-saleID PIC X(10) FROM WSS2-saleID   LINE 13 COL 19.
   05 VALUE "VIN ID: "                       LINE 13 COL 31.
   05 D2-carVIN PIC X(10) FROM WSS2-carVIN   LINE 13 COL 40.
   05 VALUE "CUST ID: "                      LINE 13 COL 53.
   05 D2-custID PIC X(10) FROM WSS2-custID   LINE 13 COL 62.
   05 VALUE "EMP ID: "                       LINE 13 COL 75.
   05 D2-empID PIC X(10) FROM WSS2-empID     LINE 13 COL 83.

   05 VALUE "DATE: "                         LINE 15 COL 10.
   05 D3-saleDate LINE 15 COL 15.
      10 D3-year   PIC 9(4) FROM WSS3-year.
      10 FILLER   VALUE "-".
      10 D3-month  PIC 9(2) FROM WSS3-month.
      10 FILLER   VALUE "-".
      10 D3-day    PIC 9(2) FROM WSS3-day.
   05 VALUE "PRICE: "                        LINE 15 COL 29.
   05 D3-price FROM WSS3-price               LINE 15 COL 38.
   05 VALUE "MILEAGE: "                      LINE 15 COL 50.
   05 D3-mileage FROM WSS3-mileage           LINE 15 COL 59.
   05 VALUE "SALE ID: "                      LINE 16 COL 10.
   05 D3-saleID PIC X(10) FROM WSS3-saleID   LINE 16 COL 19.
   05 VALUE "VIN ID: "                       LINE 16 COL 31.
   05 D3-carVIN PIC X(10) FROM WSS3-carVIN   LINE 16 COL 40.
   05 VALUE "CUST ID: "                      LINE 16 COL 53.
   05 D3-custID PIC X(10) FROM WSS3-custID   LINE 16 COL 62.
   05 VALUE "EMP ID: "                       LINE 16 COL 75.
   05 D3-empID PIC X(10) FROM WSS3-empID     LINE 16 COL 83.

   05 VALUE "DATE: "                         LINE 18 COL 10.
   05 D4-saleDate LINE 18 COL 15.
      10 D4-year   PIC 9(4) FROM WSS4-year.
      10 FILLER   VALUE "-".
      10 D4-month  PIC 9(2) FROM WSS4-month.
      10 FILLER   VALUE "-".
      10 D4-day    PIC 9(2) FROM WSS4-day.
   05 VALUE "PRICE: "                        LINE 18 COL 29.
   05 D4-price FROM WSS4-price               LINE 18 COL 38.
   05 VALUE "MILEAGE: "                      LINE 18 COL 50.
   05 D4-mileage FROM WSS4-mileage           LINE 18 COL 59.
   05 VALUE "SALE ID: "                      LINE 19 COL 10.
   05 D4-saleID PIC X(10) FROM WSS4-saleID   LINE 19 COL 19.
   05 VALUE "VIN ID: "                       LINE 19 COL 31.
   05 D4-carVIN PIC X(10) FROM WSS4-carVIN   LINE 19 COL 40.
   05 VALUE "CUST ID: "                      LINE 19 COL 53.
   05 D4-custID PIC X(10) FROM WSS4-custID   LINE 19 COL 62.
   05 VALUE "EMP ID: "                       LINE 19 COL 75.
   05 D4-empID PIC X(10) FROM WSS4-empID     LINE 19 COL 83.

   05 VALUE "ESC - RETURN TO SELECTION"      LINE 21 COL 35.
   05 VALUE "RIGHT ARROW - NEXT EMPLOYEE" LINE 22 COL 35.
   05 VALUE "LEFT ARROW - PREVIOUS EMPLOYEE" LINE 23 COL 35.
   *>05 RETURNCODE PIC 99 FROM PA-RETURN-CODE LINE 24 COL 35.

PROCEDURE DIVISION USING PARAMETRES.
MAIN-PROCEDURE.
   MOVE 00000 TO PA-LAST-SALEID
   MOVE "20000101" TO PA-LAST-DATE
   MOVE 01 TO PA-OPTION-CODE

   PERFORM 200-LOAD-VALUES

   DISPLAY SPACES BLANK SCREEN
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

200-LOAD-VALUES.
   *> Sets the start of the page for tabbing backward
   MOVE WSS0-saleID TO WS-LP-saleID
   MOVE WSS0-saleDate TO WS-LP-saleDate

   CALL 'READ_SALE' USING WSS0-Sale, PARAMETRES

   MOVE WSS0-saleID TO PA-LAST-SALEID
   MOVE WSS0-saleDate TO PA-LAST-DATE

   *> End of File = 10; Invalid key = 23
   IF PA-RETURN-CODE = 10 OR PA-RETURN-CODE = 23
      MOVE 0 TO PA-RETURN-CODE
      MOVE ZEROES TO WSS0-Sale
      EXIT PARAGRAPH
   END-IF

   CALL 'READ_SALE' USING WSS1-Sale, PARAMETRES

   MOVE WSS1-saleID TO PA-LAST-SALEID
   MOVE WSS1-saleDate TO PA-LAST-DATE

   IF PA-RETURN-CODE = 10 OR PA-RETURN-CODE = 23
      MOVE 0 TO PA-RETURN-CODE
      MOVE ZEROES TO WSS1-Sale
      EXIT PARAGRAPH
   END-IF

   CALL 'READ_SALE' USING WSS2-Sale, PARAMETRES

   MOVE WSS2-saleID TO PA-LAST-SALEID
   MOVE WSS2-saleDate TO PA-LAST-DATE

   IF PA-RETURN-CODE = 10 OR PA-RETURN-CODE = 23
      MOVE 0 TO PA-RETURN-CODE
      MOVE ZEROES TO WSS2-Sale
      EXIT PARAGRAPH
   END-IF

   CALL 'READ_SALE' USING WSS3-Sale, PARAMETRES

   MOVE WSS3-saleID TO PA-LAST-SALEID
   MOVE WSS3-saleDate TO PA-LAST-DATE

   IF PA-RETURN-CODE = 10 OR PA-RETURN-CODE = 23
      MOVE 0 TO PA-RETURN-CODE
      MOVE ZEROES TO WSS3-Sale
      EXIT PARAGRAPH
   END-IF

   CALL 'READ_SALE' USING WSS4-Sale, PARAMETRES

   MOVE WSS4-saleID TO PA-LAST-SALEID
   MOVE WSS4-saleDate TO PA-LAST-DATE

   IF PA-RETURN-CODE = 10 OR PA-RETURN-CODE = 23
      MOVE 0 TO PA-RETURN-CODE
      MOVE ZEROES TO WSS4-Sale
      EXIT PARAGRAPH
   END-IF

EXIT PARAGRAPH.

END PROGRAM SALES_REP.

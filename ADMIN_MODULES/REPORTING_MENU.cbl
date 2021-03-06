*>****************************************************************
*> Author: Joseph Warren
*> Date: 3/13/2019
*> Purpose: Serve as a hub to select which report needs to be ran
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. REPORTING_MENU.
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
01 WS-CONTROL PIC 9 VALUE 1.
01 PARAMETRES.
   02 PA-RETURN-CODE PIC 99 VALUE 0.
   02 PA-OPTION-CODE PIC 99 VALUE 0.

01 REPORT-OPTION  PIC X(15).
   88 SALES       VALUES "SALES", "1", "1. SALES".
   88 PURCHASES   VALUES "PURCHASES", "2", "2. PURCHASES".
   88 EMPLOYEES   VALUES "EMPLOYEES", "3", "3. EMPLOYEES".
   88 CUSTOMERS   VALUES "CUSTOMERS", "4", "4. CUSTOMERS".
   88 QUIT        VALUES "QUIT", "Q".

SCREEN SECTION.
01 REPORT-MENU BLANK SCREEN
   FOREGROUND-COLOR 7 BACKGROUND-COLOR 0 ERASE SCREEN.
   05 TITLE-BAR FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
      10 VALUE SPACES PIC X(120).
      10 VALUE "REPORT MENU" LINE 1 COL 50.

   05 VALUE "  REPORT TYPES"      LINE 3 COL 10.
   05 VALUE "----------------"    LINE 4 COL 10.
   05 VALUE "1. SALES"            LINE 5 COL 10.
   05 VALUE "2. PURCHASES"        LINE 6 COL 10.
   05 VALUE "3. EMPLOYEES"        LINE 7 COL 10.
   05 VALUE "4. CUSTOMERS"        LINE 8 COL 10.

   05 VALUE "ENTER REPORT TYPE:"  LINE 12 COL 10.
   05 IN-REPORT-OPTIONS PIC X(16) TO REPORT-OPTION
      LINE 12 COL 29.


   05 VALUE "ENTER - SUBMIT OPTION" LINE 18 COL 35.
   05 VALUE "ESC - CANCEL" LINE 19 COL 35.

PROCEDURE DIVISION.
MAIN-PROCEDURE.
   SET ENVIRONMENT "COB_SCREEN_EXCEPTIONS" TO "Y".
   SET ENVIRONMENT "COB_SCREEN_ESC" TO "Y".
   SET ENVIRONMENT "COB_BELL" TO "FLASH".

   PERFORM UNTIL REPORT-OPTION = "QUIT"

   DISPLAY SPACES BLANK SCREEN
   DISPLAY REPORT-MENU

      ACCEPT REPORT-MENU
         ON ESCAPE
            MOVE "QUIT" TO REPORT-OPTION
         NOT ON ESCAPE
            MOVE FUNCTION UPPER-CASE(REPORT-OPTION) TO REPORT-OPTION

      EVALUATE TRUE *> REPORT-OPTION
         WHEN SALES *> REPORT-OPTION = "SALES"
            CALL "SALES_REP" USING PARAMETRES
            MOVE SPACES TO IN-REPORT-OPTIONS
         WHEN PURCHASES
            MOVE 0 TO WS-CONTROL
         WHEN EMPLOYEES
            MOVE 0 TO WS-CONTROL
         WHEN CUSTOMERS
            MOVE 0 TO WS-CONTROL
      END-EVALUATE

   END-PERFORM
   STOP RUN.
END PROGRAM REPORTING_MENU.

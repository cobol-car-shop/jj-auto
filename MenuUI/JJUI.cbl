*>****************************************************************
*> Author:Aiden Stahl
*> Date:3/12/2019
*> Purpose: To allow access to other programs in the j&jauto system based on permissions
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. JJUI.
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
01 RESPONSE PIC 9(2).
01 NUMERIC-LABEL PIC 9(2).
01 CALLABLE-MODULES.
    05 SALES-MAIN PIC 9(2).
    05 PARTS-MAIN PIC 9(2).
    05 ACCOUNT-CREATE PIC 9(2).
    05 EMPLOYEE-ADD PIC 9(2).
    05 EPLOYEE-BROWSE PIC 9(2).
    05 EMPLOYEE-EDIT PIC 9(2).
    05 CUSTOMER-MAIN PIC 9(2).
    05 ADMIN-FEILDS PIC 9(2).
LINKAGE SECTION.
01 USERNAME-IN PIC X(30).
01 PERMISSION-IN PIC A(5).
SCREEN SECTION.
 01 MENU-SCREEN.
                05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "MENU".
           10 LINE 2 COLUMN 50 VALUE ("USERNAME: ", USERNAME-IN).
           10 LINE 3 COLUMN 50 VALUE ("ROLE: ", PERMISSION-IN).
 01 INPUT-SCREEN.
                   05 FOREGROUND-COLOR 07
                   BACKGROUND-COLOR 00
                   ERASE SCREEN.
                   10 LINE 24 COLUMN 20 VALUE "Please enter the number next to the desired Program. Enter a 0 before a single digit number: "
                   10 PIC 9(2) TO RESPONSE.
 01 SALES-SCREEN.
           05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "SALES".
 01 MAINTENANCE-SCREEN.
                05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "MAINTENANCE".
 01 PARTS-SCREEN.
                05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "PARTS".
 01 CUSTOMER-MANAGMENT-SCREEN.
                05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "CUSTOMER MANAGEMENT".
 01 EMPLOYEE-MANAGEMENT-SCREEN.
                05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "CUSTOMER MANAGMENT".
 01 ADMINISTRATION-SCREEN.
            05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "ADMINISTRATION".
 01 REPORTING-SCREEN.
                05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "REPORTING".
PROCEDURE DIVISION.
100-MAIN-PROCEDURE.
    MOVE 0 TO NUMERIC-LABEL
    PERFORM 200-CHECK-PERMISSIONS
    PERFORM 300-COLLECT-INPUT
        STOP RUN.
200-CHECK-PERMISSIONS.
    IF PERMISSION-IN = "ADMIN"
        THEN
        MOVE NUMERIC-LABEL + 1 TO NUMERIC-LABEL
        MOVE NUMERIC-LABEL TO ACCOUNT-CREATE
        MOVE NUMERIC-LABEL + 1 TO NUMERIC-LABEL
        MOVE NUMERIC-LABEL TO ADMIN-FEILDS
        DISPLAY ADMINISTRATION-SCREEN
        END-IF.
    IF PERMISSION-IN = "SALES" OR PERMISSION-IN = "ADMIN"
        THEN
        MOVE NUMERIC-LABEL + 1 TO NUMERIC-LABEL
        MOVE NUMERIC-LABEL TO SALES-MAIN
        DISPLAY SALES-SCREEN
        END-IF.
    IF PERMISSION-IN = "PARTS" OR PERMISSION-IN = "ADMIN"
        THEN
          MOVE NUMERIC-LABEL + 1 TO NUMERIC-LABEL
          MOVE NUMERIC-LABEL TO EMPLOYEE-ADD
          MOVE NUMERIC-LABEL + 1 TO NUMERIC-LABEL
          MOVE NUMERIC-LABEL TO EPLOYEE-BROWSE
          MOVE NUMERIC-LABEL + 1 TO NUMERIC-LABEL
          MOVE NUMERIC-LABEL TO EMPLOYEE-EDIT
          DISPLAY PARTS-SCREEN
          END-IF.
    IF PERMISSION-IN = "CUSTM" OR PERMISSION-IN = "ADMIN"
        THEN
        MOVE NUMERIC-LABEL + 1 TO NUMERIC-LABEL
        MOVE NUMERIC-LABEL TO CUSTOMER-MAIN
        DISPLAY CUSTOMER-MANAGMENT-SCREEN
        END-IF.
300-COLLECT-INPUT.
    DISPLAY INPUT-SCREEN
    ACCEPT INPUT-SCREEN.
END PROGRAM JJUI.
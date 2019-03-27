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
01 RESPONSE PIC X(4).
01 NUMERIC-LABEL PIC 9(2).
01 NEW-LINE-POSITION PIC 9(2).
01 CALLABLE-MODULES.
    05 SALES-MAIN PIC 9(2).
    05 PARTS-MAIN PIC 9(2).
    05 ACCOUNT-CREATE PIC 9(2).
    05 EMPLOYEE-ADD PIC 9(2).
    05 EPLOYEE-BROWSE PIC 9(2).
    05 EMPLOYEE-EDIT PIC 9(2).
    05 CUSTOMER-MAIN PIC 9(2).
    05 ADMIN-FEILDS PIC 9(2).
01 CURENT-NUMBER PIC 9(2).
01 TOTAL-NUMBER PIC 9(2).
01 PROGRAM-NAME PIC X(20).
01 IF-IN PIC A(1).
01 OPTIONS-TABLE.
    05 AVAILABLE-PROGRAMS OCCURS 10 TIMES.
      10 PROGRAM-NUMBER PIC 9(2).
      10 PROGRAM-CALLS PIC X(20).
LINKAGE SECTION.
01 USERNAME-IN PIC X(30).
01 PERMISSION-IN PIC A(5).
SCREEN SECTION.
 01 MENU-SCREEN.
                05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "MENU".
           10 LINE 2 COLUMN 50  USING USERNAME-IN.
           10 LINE 3 COLUMN 50  USING PERMISSION-IN.
 01 INPUT-SCREEN.
                   05 FOREGROUND-COLOR 07
                   BACKGROUND-COLOR 00
                   ERASE SCREEN.
                   10 LINE 23 COLUMN 20 VALUE "Please enter the number next to the desired Program or enter exit to close the program: ".
                   10 PIC X(4) TO RESPONSE.
 01 BAD-VALUE-SCREEN.
                   05 FOREGROUND-COLOR 07
                   BACKGROUND-COLOR 00
                   ERASE SCREEN.
                   10 LINE 24 COLUMN 20 VALUE "The value you entered was invalid please enter the number of a program you have access to or enter exit to close the program".
 01 TEST-SUCCESS-SCREEN.
           05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 USING RESPONSE.
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
 01 SELECTION-SCREEN.
                05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE NUMERIC-LABEL COLUMN 60 USING PROGRAM-NAME.
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
    DISPLAY MENU-SCREEN
    MOVE 0 TO NUMERIC-LABEL
    PERFORM 200-CHECK-PERMISSIONS
    PERFORM 300-COLLECT-INPUT
        STOP RUN.
200-CHECK-PERMISSIONS.
    IF PERMISSION-IN = "ADMIN"
        THEN
        ADD 1 TO NUMERIC-LABEL
        MOVE NUMERIC-LABEL TO ACCOUNT-CREATE
        ADD 5 TO NUMERIC-LABEL GIVING NEW-LINE-POSITION
        MOVE "ACCOUNT CREATION" TO PROGRAM-NAME
        DISPLAY SELECTION-SCREEN
        ADD 1 TO NUMERIC-LABEL
        *>MOVE NUMERIC-LABEL TO ADMIN-FEILDS
        ADD 5 TO NUMERIC-LABEL GIVING NEW-LINE-POSITION
        MOVE "ADMIN TOOLS" TO PROGRAM-NAME
        DISPLAY SELECTION-SCREEN
        *>Admin
        END-IF.
    IF PERMISSION-IN = "SALES" OR PERMISSION-IN = "ADMIN"
        THEN
        ADD 1 TO NUMERIC-LABEL
        MOVE "Sales Main" TO PROGRAM-NAME
        ADD 5 TO NUMERIC-LABEL GIVING NEW-LINE-POSITION
        DISPLAY SELECTION-SCREEN
        END-IF.
    IF PERMISSION-IN = "PARTS" OR PERMISSION-IN = "ADMIN"
        THEN
          ADD 1 TO NUMERIC-LABEL
          MOVE NUMERIC-LABEL TO EMPLOYEE-ADD
          ADD 1 TO NUMERIC-LABEL
          MOVE NUMERIC-LABEL TO EPLOYEE-BROWSE
          ADD 1 TO NUMERIC-LABEL
          MOVE NUMERIC-LABEL TO EMPLOYEE-EDIT
         *> DISPLAY PARTS-SCREEN
          END-IF.
    IF PERMISSION-IN = "CUSTM" OR PERMISSION-IN = "ADMIN"
        THEN
        ADD 1 TO NUMERIC-LABEL
        MOVE NUMERIC-LABEL TO CUSTOMER-MAIN
        *>DISPLAY CUSTOMER-MANAGMENT-SCREEN
        END-IF.
300-COLLECT-INPUT.
    DISPLAY INPUT-SCREEN
    ACCEPT INPUT-SCREEN.
    PERFORM 400-SEARCH-PROGRAMS.
    IF IF-IN = 'T'
        THEN
    ELSE
        DISPLAY BAD-VALUE-SCREEN
        END-IF.


400-SEARCH-PROGRAMS.
    MOVE 'F' TO IF-IN
    MOVE 0 TO CURENT-NUMBER
    MOVE NUMERIC-LABEL TO TOTAL-NUMBER
 PERFORM VARYING CURENT-NUMBER FROM 1 BY 1
     UNTIL CURENT-NUMBER = TOTAL-NUMBER
     IF RESPONSE = PROGRAM-NUMBER(CURENT-NUMBER)
         THEN
          MOVE PROGRAM-CALLS(CURENT-NUMBER) TO PROGRAM-NAME
          MOVE 'T' TO IF-IN
              MOVE TOTAL-NUMBER TO CURENT-NUMBER
             END-IF
             END-PERFORM.
END PROGRAM JJUI.

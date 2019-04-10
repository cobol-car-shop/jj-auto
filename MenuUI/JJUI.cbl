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
01 PROGRAM-NAME PIC X(20).
01 TO-BE-CALLED PIC X(20).
01 RESPONSE-NUMERIC PIC 9(4).
01 IF-IN PIC A(1).
01 EXIT-VAL PIC A(4).
01 OPTIONS-TABLE.
    05 AVAILABLE-PROGRAMS OCCURS 10 TIMES.
      10 PROGRAM-CALLS PIC X(20).
LINKAGE SECTION.
01 USERNAME-IN PIC X(30).
01 PERMISSION-IN PIC A(5).
SCREEN SECTION.
 01 BLANK-SCREEN.
      05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
              10 VALUE " " BLANK SCREEN LINE 1 COLUMN 1.
 01 MENU-SCREEN.
                05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "MENU".
           10 USERNAME-DISP LINE 2 COLUMN 50 FROM USERNAME-IN.
           10 PERMISSION-DISP LINE 3 COLUMN 50 FROM PERMISSION-IN.
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

                    01 TEST-SCREEN.
             05 FOREGROUND-COLOR 07
              BACKGROUND-COLOR 00.
              10 LINE 20 COLUMN 1 FROM RESPONSE-NUMERIC.
                                  01 TEST-SCREEN2.
             05 FOREGROUND-COLOR 07
              BACKGROUND-COLOR 00.
              10 LINE 22 COLUMN 1 FROM RESPONSE-NUMERIC.
 01 SELECTION-SCREEN.
                05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE NEW-LINE-POSITION COLUMN 56 FROM NUMERIC-LABEL.
           10 LINE NEW-LINE-POSITION COLUMN 60 FROM PROGRAM-NAME.
PROCEDURE DIVISION USING USERNAME-IN,PERMISSION-IN.
100-MAIN-PROCEDURE.
    DISPLAY BLANK-SCREEN
    DISPLAY MENU-SCREEN
    MOVE 0 TO NUMERIC-LABEL
    PERFORM 200-CHECK-PERMISSIONS
    PERFORM 300-COLLECT-INPUT
        STOP RUN.
200-CHECK-PERMISSIONS.
    IF PERMISSION-IN = "ADMIN"
        THEN
        ADD 1 TO NUMERIC-LABEL
        ADD 5 TO NUMERIC-LABEL GIVING NEW-LINE-POSITION
        MOVE 'ACCOUNT' TO PROGRAM-CALLS (NUMERIC-LABEL)
        MOVE "ACCOUNT CREATION" TO PROGRAM-NAME
        DISPLAY SELECTION-SCREEN
        ADD 1 TO NUMERIC-LABEL
        ADD 5 TO NUMERIC-LABEL GIVING NEW-LINE-POSITION
        MOVE "REPORTING_MENU" TO PROGRAM-CALLS(NUMERIC-LABEL)
        MOVE "Reporting " TO PROGRAM-NAME
        DISPLAY SELECTION-SCREEN
        *>Admin
        END-IF.
    IF PERMISSION-IN = "SALES" OR PERMISSION-IN = "ADMIN"
        THEN
        ADD 1 TO NUMERIC-LABEL
        MOVE "Sales Main" TO PROGRAM-NAME
        ADD 5 TO NUMERIC-LABEL GIVING NEW-LINE-POSITION
        MOVE "SALES_MAIN" TO PROGRAM-CALLS(NUMERIC-LABEL)
        DISPLAY SELECTION-SCREEN
        END-IF.
    IF PERMISSION-IN = "MANAG" OR PERMISSION-IN = "ADMIN"
        THEN
          ADD 1 TO NUMERIC-LABEL
          MOVE "Employee Browse" TO PROGRAM-NAME
          ADD 5 TO NUMERIC-LABEL GIVING NEW-LINE-POSITION
          MOVE "EMPLOYEE_BROWSE" TO PROGRAM-CALLS(NUMERIC-LABEL)
          DISPLAY SELECTION-SCREEN
          END-IF.
    IF PERMISSION-IN = "CUSTM" OR PERMISSION-IN = "ADMIN"
        THEN
        ADD 1 TO NUMERIC-LABEL
        MOVE "Customer Add" TO PROGRAM-NAME
         ADD 5 TO NUMERIC-LABEL GIVING NEW-LINE-POSITION
        MOVE "CUSADD" TO PROGRAM-CALLS(NUMERIC-LABEL)
        DISPLAY SELECTION-SCREEN
                ADD 1 TO NUMERIC-LABEL
        MOVE "Customer Delete" TO PROGRAM-NAME
         ADD 5 TO NUMERIC-LABEL GIVING NEW-LINE-POSITION
        MOVE "CUSDEL" TO PROGRAM-CALLS(NUMERIC-LABEL)
        DISPLAY SELECTION-SCREEN
                ADD 1 TO NUMERIC-LABEL
        MOVE "Customer Update" TO PROGRAM-NAME
         ADD 5 TO NUMERIC-LABEL GIVING NEW-LINE-POSITION
        MOVE "CUSUPD" TO PROGRAM-CALLS(NUMERIC-LABEL)
        DISPLAY SELECTION-SCREEN
                ADD 1 TO NUMERIC-LABEL
        MOVE "Customer Veiw" TO PROGRAM-NAME
         ADD 5 TO NUMERIC-LABEL GIVING NEW-LINE-POSITION
        MOVE "CUSTVW" TO PROGRAM-CALLS(NUMERIC-LABEL)
        DISPLAY SELECTION-SCREEN
        END-IF.
300-COLLECT-INPUT.
    PERFORM UNTIL EXIT-VAL = "EXIT"
    DISPLAY INPUT-SCREEN
    ACCEPT INPUT-SCREEN
    PERFORM 400-SEARCH-PROGRAMS

    IF IF-IN = 'T'
        THEN
        DISPLAY BLANK-SCREEN
        IF TO-BE-CALLED = "EMPLOYEE_BROWSE"
            THEN
            CALL "SYSTEM" USING TO-BE-CALLED
        ELSE
        CALL TO-BE-CALLED
        END-IF
    ELSE
        DISPLAY BAD-VALUE-SCREEN
        END-IF
            END-PERFORM.


400-SEARCH-PROGRAMS.
   *> IF RESPONSE IS NUMERIC
        *>THEN
        MOVE 'F' TO IF-IN
          MOVE RESPONSE TO RESPONSE-NUMERIC
        DISPLAY TEST-SCREEN
        EVALUATE RESPONSE-NUMERIC WHEN 1 THRU NUMERIC-LABEL
        DISPLAY TEST-SCREEN2
             MOVE 'T' TO IF-IN
             MOVE PROGRAM-CALLS(RESPONSE-NUMERIC) TO TO-BE-CALLED

        *> END-IF
        END-EVALUATE
         IF FUNCTION UPPER-CASE (RESPONSE) NOT EQUAL "EXIT" AND IF-IN ='F'
             THEN
                 MOVE 'F' TO IF-IN
                 DISPLAY BAD-VALUE-SCREEN
                 ELSE
                     MOVE "EXIT" TO EXIT-VAL
             END-IF.

END PROGRAM JJUI.

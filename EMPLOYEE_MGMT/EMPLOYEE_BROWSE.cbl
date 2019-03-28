      ******************************************************************
      * Author: Matthew East
      * Date: 03/05/2019
      * Purpose: Browses the employees in the employee file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE_BROWSE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS CRT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL EMP-FILE
               ASSIGN TO '../INDEXES/EMPLOYEE.IDX'
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS IDX-empID
               ALTERNATE RECORD KEY IS IDX-LNAME WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD EMP-FILE
           RECORD CONTAINS 161 CHARACTERS.
           COPY EMP_DEF REPLACING ==:TAG:== BY ==IDX==.
       WORKING-STORAGE SECTION.
           01 WS-KEY PIC X.
           01 WS-DONE PIC X VALUE "N".
           01 WS-READY PIC X VALUE "N".
           01 WS-EDIT-CMD.
              05 PIC X(14) VALUE "EMPLOYEE_EDIT ".
              05 EDIT-ID PIC 9(5).
       SCREEN SECTION.
       01 EMPLOYEE-VIEW-SCREEN BLANK SCREEN
           FOREGROUND-COLOR 7 BACKGROUND-COLOR 0.
           05 TITLE-BAR FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
             10 VALUE SPACES PIC X(120).
             10 VALUE "EMPLOYEE MANAGEMENT - BROWSE" LINE 1 COL 50.

           05 VALUE "EMPLOYEE ID #" LINE 3 COL 10.
           05 D-EMP-ID FROM IDX-EMPID LINE 3 COL 25.

           05 VALUE "FIRST NAME" LINE 4 COL 10.
           05 D-EMP-FNAME FROM IDX-FNAME LINE 4 COL 25.

           05 VALUE "LAST NAME" LINE 5 COL 10.
           05 D-EMP-LNAME FROM IDX-LNAME LINE 5 COL 25.

           05 VALUE "SSN" LINE 6 COL 10.
           05 D-EMP-SSN FROM IDX-SOCIAL LINE 6 COL 25.

           05 VALUE "PHONE #" LINE 7 COL 10.
      *>     05 D-EMP-PHONE FROM IDX-PHONE LINE 7 COL 25.
           05 D-EMP-PHONE-AREA FROM IDX-PHONE(1:3) LINE 7 COL 25.
           05 VALUE "-" LINE 7 COL 28.
           05 D-EMP-PHONE-EXCH FROM IDX-PHONE(4:3) LINE 7 COL 29.
           05 VALUE "-" LINE 7 COL 32.
           05 D-EMP-PHONE-LAST FROM IDX-PHONE(7:4) LINE 7 COL 33.

           05 VALUE "EMAIL" LINE 8 COL 10.
           05 D-EMP-EML FROM IDX-EMAIL LINE 8 COL 25.

           05 VALUE "ADDRESS" LINE 9 COL 10.
           05 D-EMP-ADDRESS FROM IDX-ADDRESS LINE 9 COL 25.

           05 VALUE "CITY" LINE 10 COL 10.
           05 D-EMP-CITY FROM IDX-CITY LINE 10 COL 25.

           05 VALUE "STATE" LINE 11 COL 10.
           05 D-EMP-STATE FROM IDX-STATE LINE 11 COL 25.

           05 VALUE "POSTAL CODE" LINE 12 COL 10.
           05 D-EMP-ZIP FROM IDX-ZIP LINE 12 COL 25.

           05 VALUE "WAGE" LINE 13 COL 10.
           05 D-EMP-WAGE PIC $ZZZZ9.99 FROM IDX-WAGE LINE 13 COL 25.

           05 VALUE "HOURLY?" LINE 14 COL 10.
           05 D-EMP-HOURLY FROM IDX-HOURLY LINE 14 COL 25.

           05 VALUE "POSITION" LINE 15 COL 10.
           05 D-EMP-POSITION FROM IDX-POSITION LINE 15 COL 25.

           05 VALUE "E - EDIT EMPLOYEE" LINE 18 COL 35.
           05 VALUE "D - DELETE EMPLOYEE" LINE 19 COL 35.
           05 VALUE "C - CREATE EMPLOYEE" LINE 20 COL 35.
           05 VALUE "ESC - RETURN TO MENU" LINE 21 COL 35.
           05 VALUE "RIGHT ARROW - NEXT EMPLOYEE" LINE 22 COL 35.
           05 VALUE "LEFT ARROW - PREVIOUS EMPLOYEE" LINE 23 COL 35.

       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT EMP-FILE.
           PERFORM UNTIL WS-READY = "Y"
           READ EMP-FILE
               NOT AT END
                   MOVE "Y" TO WS-READY
               AT END
                   CLOSE EMP-FILE
                   CALL "SYSTEM" USING "EMPLOYEE_ADD"
                   OPEN INPUT EMP-FILE
           END-PERFORM.
           DISPLAY EMPLOYEE-VIEW-SCREEN.
      *> The first two environment vars here let me handle arrow keys and the escape key
      *> The third makes the screen flash when I call DISPLAY WITH BELL
           SET ENVIRONMENT "COB_SCREEN_EXCEPTIONS" TO "Y".
           SET ENVIRONMENT "COB_SCREEN_ESC" TO "Y".
           SET ENVIRONMENT "COB_BELL" TO "FLASH".

           PERFORM UNTIL WS-DONE = "Y"
               ACCEPT WS-KEY
                   WITH NO ECHO
                   BACKGROUND-COLOR 1
                   AUTO-SKIP
               EVALUATE FUNCTION UPPER-CASE(WS-KEY)
                   WHEN SPACE PERFORM 200-HANDLE-SPECIAL-KEY
                   WHEN "E"
                       CLOSE EMP-FILE
                       MOVE IDX-EMPID TO EDIT-ID
                       CALL "SYSTEM" USING WS-EDIT-CMD
                       DISPLAY SPACES BLANK SCREEN
                       OPEN INPUT EMP-FILE
                       START EMP-FILE KEY IS EQUAL TO IDX-EMPID
                       READ EMP-FILE
                   WHEN "C"
                       CLOSE EMP-FILE
                       CALL "SYSTEM" USING "EMPLOYEE_ADD"
                       DISPLAY SPACES BLANK SCREEN
                       OPEN INPUT EMP-FILE
                       START EMP-FILE KEY IS EQUAL TO IDX-EMPID
                       READ EMP-FILE
               END-EVALUATE
              DISPLAY EMPLOYEE-VIEW-SCREEN
           END-PERFORM.
           CLOSE EMP-FILE.
           STOP RUN.
       200-HANDLE-SPECIAL-KEY.
      *> Left Arrow - 2009
      *> Right Arrow - 2010
      *> Up Arrow - 2003
      *> Down Arrow - 2004
      *> ESC - 2005
      *> See also: https://edoras.sdsu.edu/doc/GNU_Cobol_Programmers_Guide_2.1.pdf pg 345
           EVALUATE COB-CRT-STATUS
               WHEN 2005
                   CLOSE EMP-FILE
                   STOP RUN
               WHEN 2009
                   READ EMP-FILE PREVIOUS RECORD
                       AT END
                           DISPLAY SPACE WITH BELL
                           READ EMP-FILE NEXT RECORD
               WHEN 2010
                   READ EMP-FILE NEXT RECORD
                       AT END
                           DISPLAY SPACE WITH BELL
                           READ EMP-FILE PREVIOUS RECORD
           END-EVALUATE.
       END PROGRAM EMPLOYEE_BROWSE.

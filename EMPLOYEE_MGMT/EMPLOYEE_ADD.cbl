      ******************************************************************
      * Author: Matthew East
      * Date: 03/13/2019
      * Purpose: Adds a new employee to the employee file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE_ADD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL EMP-FILE
               ASSIGN TO '../INDEXES/EMPLOYEE.IDX'
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS IDX-empID
               ALTERNATE RECORD KEY IS IDX-LNAME.
       DATA DIVISION.
       FILE SECTION.
       FD EMP-FILE
           RECORD CONTAINS 161 CHARACTERS.
           COPY EMP_DEF REPLACING ==:TAG:== BY ==IDX==.
       WORKING-STORAGE SECTION.
           01 WS-FNAME PIC X(15).
           01 WS-LNAME PIC X(15).
           01 WS-SSN PIC 9(9).
           01 WS-PHONE PIC X(13).
           01 WS-EMAIL PIC X(25).
           01 WS-ADDRESS PIC X(25).
           01 WS-CITY PIC X(25).
           01 WS-STATE PIC XX.
           01 WS-POSTAL-CODE PIC 9(5).
           01 WS-WAGE PIC 9(5).9(2).
           01 WS-HOURLY PIC XXX.
               88 HOURLY-VALID VALUE "YES", "NO".
           01 WS-POSITION PIC A(15).
               88 POSITION-VALID
                   VALUE "SALES", "MECHANIC", "ACCOUNTANT" "ADMIN".
           01 IS-VALID PIC X VALUE 'N'.
           01 MORE-RECS PIC X VALUE 'Y'.
       SCREEN SECTION.
       01 EMPLOYEE-ADD-SCREEN BLANK SCREEN
           FOREGROUND-COLOR 7 BACKGROUND-COLOR 0.
           05 TITLE-BAR FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
             10 VALUE SPACES PIC X(120).
             10 VALUE "EMPLOYEE MANAGEMENT" LINE 1 COL 50.

           05 VALUE "EMPLOYEE ID #" LINE 3 COL 10.
           05 VALUE "-----" LINE 3 COL 25.

           05 VALUE "FIRST NAME" LINE 4 COL 10.
           05 IN-FNAME PIC X(15) FROM WS-FNAME TO WS-FNAME REQUIRED
               LINE 4 COL 25.

           05 VALUE "LAST NAME" LINE 5 COL 10.
           05 IN-LNAME FROM WS-LNAME TO WS-LNAME REQUIRED
               LINE 5 COL 25.

           05 VALUE "SSN" LINE 6 COL 10.
           05 IN-SSN FROM WS-SSN TO WS-SSN LINE 6 COL 25.

           05 VALUE "PHONE #" LINE 7 COL 10.
           05 IN-PHONE FROM WS-PHONE TO WS-PHONE LINE 7 COL 25.

           05 VALUE "EMAIL" LINE 8 COL 10.
           05 IN-EMAIL FROM WS-EMAIL TO WS-EMAIL LINE 8 COL 25.

           05 VALUE "ADDRESS" LINE 9 COL 10.
           05 IN-ADDRESS FROM WS-ADDRESS TO WS-ADDRESS LINE 9 COL 25.

           05 VALUE "CITY" LINE 10 COL 10.
           05 IN-CITY FROM WS-CITY TO WS-CITY LINE 10 COL 25.

           05 VALUE "STATE" LINE 11 COL 10.
           05 IN-STATE FROM WS-STATE TO WS-STATE LINE 11 COL 25.

           05 VALUE "POSTAL CODE" LINE 12 COL 10.
           05 IN-POSTAL-CODE FROM WS-POSTAL-CODE TO WS-POSTAL-CODE
               LINE 12 COL 25.

           05 VALUE "WAGE" LINE 13 COL 10.
           05 VALUE "$" LINE 13 COL 25.
           05 IN-WAGE FROM WS-WAGE TO WS-WAGE REQUIRED
               LINE 13 COL 26.

           05 VALUE "HOURLY?" LINE 14 COL 10.
           05 IN-HOURLY FROM WS-HOURLY TO WS-HOURLY REQUIRED
               LINE 14 COL 25.

           05 VALUE "POSITION" LINE 15 COL 10.
           05 IN-POSITION FROM WS-POSITION TO WS-POSITION
               LINE 15 COL 25.

           05 VALUE "ENTER - CREATE EMPLOYEE" LINE 18 COL 35.
           05 VALUE "ESC - CANCEL" LINE 19 COL 35.

       PROCEDURE DIVISION.
       100-MAIN.
           SET ENVIRONMENT "COB_SCREEN_EXCEPTIONS" TO "Y".
           SET ENVIRONMENT "COB_SCREEN_ESC" TO "Y".
           SET ENVIRONMENT "COB_BELL" TO "FLASH".
           PERFORM UNTIL IS-VALID = 'Y'
               DISPLAY EMPLOYEE-ADD-SCREEN
               ACCEPT EMPLOYEE-ADD-SCREEN
                   ON EXCEPTION
                       IF COB-CRT-STATUS = 2005 THEN
                           STOP RUN
                       END-IF
               END-ACCEPT

               MOVE FUNCTION UPPER-CASE(WS-HOURLY) TO WS-HOURLY
               IF HOURLY-VALID AND POSITION-VALID THEN
                   MOVE 'Y' TO IS-VALID
               ELSE
                   DISPLAY SPACE WITH BELL
               END-IF
           END-PERFORM.

           OPEN INPUT EMP-FILE.
      *> Seek to the end of the file
           PERFORM UNTIL MORE-RECS = 'N'
             READ EMP-FILE
               AT END MOVE 'N' TO MORE-RECS
           END-PERFORM.
           CLOSE EMP-FILE.

           OPEN EXTEND EMP-FILE.
           IF IDX-EMPID IS NUMERIC THEN
               ADD 1 TO IDX-EMPID
           ELSE
      *> This seems to happen when the file is first being created
               MOVE 1 TO IDX-EMPID
           END-IF.
           MOVE WS-FNAME TO IDX-FNAME.
           MOVE WS-LNAME TO IDX-LNAME.
           MOVE WS-SSN TO IDX-SOCIAL.
           MOVE WS-PHONE TO IDX-PHONE.
           MOVE WS-EMAIL TO IDX-EMAIL.
           MOVE WS-ADDRESS TO IDX-ADDRESS.
           MOVE WS-CITY TO IDX-CITY.
           MOVE WS-WAGE TO IDX-WAGE.
           MOVE WS-STATE TO IDX-STATE.
           MOVE WS-HOURLY TO IDX-HOURLY.
           MOVE WS-POSITION TO IDX-POSITION.
           WRITE IDX-EMPLOYEE.
           CLOSE EMP-FILE.
           STOP RUN.
       END PROGRAM EMPLOYEE_ADD.

      ******************************************************************
      * Author: Matthew East
      * Date: 03/05/2019
      * Purpose: Browses the employees in the employee file (UI mock only)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE_BROWSE.
       DATA DIVISION.
       FILE SECTION.
       SCREEN SECTION.
       01 EMPLOYEE-VIEW-SCREEN BLANK SCREEN
           FOREGROUND-COLOR 7 BACKGROUND-COLOR 0.
           05 TITLE-BAR FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
             10 VALUE SPACES PIC X(120).
             10 VALUE "EMPLOYEE MANAGEMENT" LINE 1 COL 50.

           05 VALUE "EMPLOYEE ID #" LINE 3 COL 10.
           05 VALUE "00001" LINE 3 COL 25.

           05 VALUE "FIRST NAME" LINE 4 COL 10.
           05 VALUE "JOHN" LINE 4 COL 25.

           05 VALUE "LAST NAME" LINE 5 COL 10.
           05 VALUE "DOE" LINE 5 COL 25.

           05 VALUE "SSN" LINE 6 COL 10.
           05 VALUE "699-45-1234" LINE 6 COL 25.

           05 VALUE "PHONE #" LINE 7 COL 10.
           05 VALUE "417-555-1234" LINE 7 COL 25.

           05 VALUE "EMAIL" LINE 8 COL 10.
           05 VALUE "EXAMPLE@GMAIL.COM" LINE 8 COL 25.

           05 VALUE "ADDRESS" LINE 9 COL 10.
           05 VALUE "123 Example St" LINE 9 COL 25.

           05 VALUE "CITY" LINE 10 COL 10.
           05 VALUE "CARTHAGE" LINE 10 COL 25.

           05 VALUE "STATE" LINE 11 COL 10.
           05 VALUE "MO" LINE 11 COL 25.

           05 VALUE "POSTAL CODE" LINE 12 COL 10.
           05 VALUE "64836" LINE 12 COL 25.

           05 VALUE "WAGE" LINE 13 COL 10.
           05 VALUE "$   10.00" LINE 13 COL 25.

           05 VALUE "HOURLY?" LINE 14 COL 10.
           05 VALUE "YES" LINE 14 COL 25.

           05 VALUE "POSITION" LINE 15 COL 10.
           05 VALUE "ADMINISTRATOR" LINE 15 COL 25.

           05 VALUE "E - EDIT EMPLOYEE" LINE 18 COL 35.
           05 VALUE "D - DELETE EMPLOYEE" LINE 19 COL 35.
           05 VALUE "C - CREATE EMPLOYEE" LINE 20 COL 35.
           05 VALUE "ESC - RETURN TO MENU" LINE 21 COL 35.
           05 VALUE "RIGHT ARROW - NEXT EMPLOYEE" LINE 22 COL 35.
           05 VALUE "LEFT ARROW - PREVIOUS EMPLOYEE" LINE 23 COL 35.

       PROCEDURE DIVISION.
       100-MAIN.
           DISPLAY EMPLOYEE-VIEW-SCREEN.
           STOP RUN.
       END PROGRAM EMPLOYEE_BROWSE.

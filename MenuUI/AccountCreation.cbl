      *>*****************************************************************
      *> Author: Aiden Stahl
      *> Date:3/12/2019
      *> Purpose: To Collect account information
      *>*****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
             SELECT OPTIONAL ACCOUNT-FILE
            ASSIGN TO "ACCOUNT.MST"
            ORGANIZATION IS INDEXED
            ACCESS IS SEQUENTIAL
            RECORD KEY IS USERNAME.
       DATA DIVISION.
       FILE SECTION.
           FD ACCOUNT-FILE.
           01 ACCOUNT-INFO.
               05 USERNAME PIC X(30).
               05 PERMISSION PIC X(5).
               05 HASH PIC X(128).

       WORKING-STORAGE SECTION.
       01 SHA3-OUTPUT    PIC X(128).
       01 TEMP-USERNAME PIC X(30).
       01 TEMP-PASSWORD PIC X(30).
       01 TEMP-PERMISSION PIC X(5).
       01 PASS-LENGTH PIC 9(2).
       01 TALLY-VAL PIC 9(10).
           SCREEN SECTION.
         01 ENTRYSCREEN.
            05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
                10 LINE 1 COLUMN 50 VALUE "CREATE ACCOUNT".
                10 LINE 3 COLUMN 15 VALUE "USERNAME: ".
                10 PIC X(30) TO TEMP-USERNAME.
                10 LINE 4 COLUMN 15 VALUE "PASSWORD: ".
                10 PIC X(30) TO TEMP-PASSWORD.
                10 LINE 5 COLUMN 15 VALUE "PERMISSION: ".
                10 PIC X(5) TO TEMP-PERMISSION.
         01 ERRORSCREEN.
             05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
               10 LINE 7 COLUMN 1 VALUE "BRO There is an error".
       PROCEDURE DIVISION.
       100-HASH.
           OPEN EXTEND ACCOUNT-FILE
           INITIALIZE SHA3-OUTPUT
           PERFORM 200-GATHER-DATA
           INSPECT FUNCTION REVERSE (TEMP-PASSWORD)
            TALLYING TALLY-VAL FOR LEADING SPACES
            COMPUTE PASS-LENGTH = LENGTH OF TEMP-PASSWORD - TALLY-VAL

            CALL "SHA3-512" USING TEMP-PASSWORD
                          PASS-LENGTH
                          SHA3-OUTPUT
        END-CALL
            MOVE FUNCTION UPPER-CASE(TEMP-USERNAME) TO USERNAME
            MOVE FUNCTION UPPER-CASE (TEMP-PERMISSION) TO PERMISSION
            MOVE SHA3-OUTPUT TO HASH
            WRITE ACCOUNT-INFO
            INVALID KEY DISPLAY ERRORSCREEN
            END-WRITE
            CLOSE ACCOUNT-FILE
            STOP RUN.

        200-GATHER-DATA.
            DISPLAY ENTRYSCREEN
            ACCEPT ENTRYSCREEN.
       END PROGRAM ACCOUNT.

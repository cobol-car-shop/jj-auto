      *>*****************************************************************
      *> Author:Aiden Stahl
      *> Date:
      *> Purpose: To allow sign in and base menus for the J&J System
      *>*****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JJSIGNIN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
             SELECT ACCOUNT-FILE
            ASSIGN TO "ACCOUNT.MST"
            ORGANIZATION IS INDEXED
            ACCESS IS RANDOM
            RECORD KEY IS USERNAME.
       DATA DIVISION.
       FILE SECTION.
        FD ACCOUNT-FILE.
           01 ACCOUNT-INFO.
               05 USERNAME PIC X(30).
               05 PERMISSION PIC X(5).
               05 HASH PIC X(128).
       WORKING-STORAGE SECTION.
         01 LOGIN-LOOP PIC A(1).
         01 USERNAME-IN PIC X(30).
         01 PASSWORD-IN PIC X(30).
         01 PERMISSION-IN PIC A(5).
         01 PRESS-ANY-KEY PIC X(3).
         01 ACCOUNT-CHECK PIC A(3) VALUE 'INC'.
         01 TEMP-PASSWORD PIC X(30).
         01 PASS-LENGTH PIC 9(2).
         01 SHA3-OUTPUT PIC X(128).
         01 TALLY-VAL PIC 9(10).
         SCREEN SECTION.
         01 AUTHSCREEN.
           05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "AUTHENTICATE".
           10 LINE 3 COLUMN 15 VALUE "USER ID:".
           10 PIC X(30) TO USERNAME-IN.
           10 LINE 4 COLUMN 15 VALUE "PASSWORD:".
           10 PIC X(30) TO PASSWORD-IN.
         01 ERROR-SCREEN.
           05 FOREGROUND-COLOR 07
              BACKGROUND-COLOR 00.
           10 LINE 5 COLUMN 1 VALUE "Your UserID or Password Was incorrect Type END to leave the program or press space to try again".
           10 PIC X(3) TO PRESS-ANY-KEY.
       PROCEDURE DIVISION.
       100-MAIN-PROCEDURE.
            PERFORM UNTIL PRESS-ANY-KEY ='END'
            PERFORM 200-READ-DATA
            END-PERFORM
            CLOSE ACCOUNT-FILE
            STOP RUN.
       200-READ-DATA.
           DISPLAY AUTHSCREEN
           ACCEPT AUTHSCREEN
           MOVE FUNCTION UPPER-CASE (USERNAME-IN) TO USERNAME-IN
           PERFORM 300-CHECK-ACCOUNT
      *>    ACCOUNT-CHECK will get an INC for incorrect or a COR for Correct
           If ACCOUNT-CHECK = 'INC'
               THEN
                PERFORM 500-CALL-ERROR
               END-IF.
       300-CHECK-ACCOUNT.
           MOVE 'COR' TO ACCOUNT-CHECK
           OPEN INPUT ACCOUNT-FILE
           MOVE USERNAME-IN TO USERNAME
           READ ACCOUNT-FILE
           INVALID KEY MOVE 'INC' TO ACCOUNT-CHECK
           END-READ
           IF ACCOUNT-CHECK = 'COR'
               THEN
               MOVE PASSWORD-IN TO TEMP-PASSWORD
               *>fix this so hashing works
               PERFORM 400-HASH-PASS
                 IF SHA3-OUTPUT = HASH
                    *>IF HASH = HASH
                   THEN
                     MOVE 'COR' TO ACCOUNT-CHECK
                     MOVE 'END' TO PRESS-ANY-KEY
                     MOVE FUNCTION  UPPER-CASE (PERMISSION) TO PERMISSION-IN
                     CALL "JJUI" USING USERNAME-IN
                                       PERMISSION-IN
                     END-CALL
                ELSE
                    DISPLAY ERROR-SCREEN
                     END-IF
                     ELSE
                    DISPLAY ERROR-SCREEN
                END-IF.
        400-HASH-PASS.
            INITIALIZE SHA3-OUTPUT
            INSPECT FUNCTION REVERSE (TEMP-PASSWORD)
            TALLYING TALLY-VAL FOR LEADING SPACES
            COMPUTE PASS-LENGTH = LENGTH OF TEMP-PASSWORD - TALLY-VAL
            CALL "SHA3-512" USING TEMP-PASSWORD
                          PASS-LENGTH
                          SHA3-OUTPUT
            END-CALL.

        500-CALL-ERROR.
            DISPLAY ERROR-SCREEN
               ACCEPT ERROR-SCREEN
               MOVE FUNCTION UPPER-CASE(PRESS-ANY-KEY) TO PRESS-ANY-KEY.
       END PROGRAM JJSIGNIN.

      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JJUI.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
         01 LOGIN-LOOP PIC A(1).
         01 USERNAME PIC X(30).
         01 PASSWORD PIC X(30).
         01 PERMISSION PIC A(5).
         01 PRESS-ANY-KEY PIC X(3).
         01 ACCOUNT-CHECK PIC A(3) VALUE 'INC'.
         SCREEN SECTION.
         01 AUTHSCREEN.
           05 FOREGROUND-COLOR 07
               BACKGROUND-COLOR 00
               ERASE SCREEN.
           10 LINE 1 COLUMN 60 VALUE "AUTHENTICATE".
           10 LINE 3 COLUMN 15 VALUE "USER ID:".
           10 PIC X(30) TO USERNAME.
           10 LINE 4 COLUMN 15 VALUE "PASSWORD:".
           10 PIC X(30) TO PASSWORD.
         01 ERROR-SCREEN.
           05 FOREGROUND-COLOR 07
              BACKGROUND-COLOR 00.
           10 LINE 5 COLUMN 1 VALUE "Your UserID or Password Was incorr
      - "ect Type END to leave the program or press space to try again".
           10 PIC X(3) TO PRESS-ANY-KEY.
       PROCEDURE DIVISION.
       100-MAIN-PROCEDURE.
            PERFORM UNTIL PRESS-ANY-KEY ='END'
            PERFORM 200-READ-DATA
            END-PERFORM
            STOP RUN.
       200-READ-DATA.
           DISPLAY AUTHSCREEN
           ACCEPT AUTHSCREEN
      *    ACCOUNT-CHECK will get an INC for incorrect or a COR for Correct
           If ACCOUNT-CHECK = 'INC'
               THEN
               DISPLAY ERROR-SCREEN
               ACCEPT ERROR-SCREEN
               END-IF.
       END PROGRAM JJUI.

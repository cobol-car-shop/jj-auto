      ******************************************************************
      * Author:  Brock Sharp
      * Date: 03/01/2019
      * Purpose: Main landing screen for parts maintenance (likely temp)
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PARTS_MAIN.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       01 WS-PROGRAM-TO-CALL       PIC X(30).
       01 WS-USER-RESPONSE           PIC 99.
       01 END-LOOP                PIC X VALUE 'F'.
       01 WS-ERROR-MESSAGE        PIC X(20).
       88 VALID-MENU-OPTION VALUES ARE 01 THRU 01.
       SCREEN SECTION.
      *-----------------------
       01 PARTS-HOME-SCREEN.
           05 TITLE-SECTION.
               10 VALUE "PARTS INVENTORY MAINTENANCE" BLANK SCREEN
                   LINE 1 COL 29.
               10 VALUE "-----------------------------------------------
      -             "--------------------------------"
                  LINE 2 COL 1.
           05 MENU-OPTIONS.
               10 OPTION-ONE.
                   20 VALUE "1."                          LINE 4 COL 11.
                   20 VALUE "LOOKUP PART NUMBER"          LINE 4 COL 15.
               10 OPTION-TWO.
                   20 VALUE "2."                          LINE 5 COL 11.
                   20 VALUE "LOOKUP MAINTENANCE ORDER"    LINE 5 COL 15.
               10 OPTION-THREE.
                   20 VALUE "3."                          LINE 6 COL 11.
                   20 VALUE "VIEW ALL PARTS"              LINE 6 COL 15.
               10 OPTION-FOUR.
                   20 VALUE "4."                          LINE 7 COL 11.
                   20 VALUE "ADD NEW PART"                LINE 7 COL 15.
               10 OPTION-TEN.
                   20 VALUE "10."                        LINE 13 COL 11.
                   20 VALUE "EXIT PROGRAM"               LINE 13 COL 15.
           05 DATA-SECTION.
               10 USER-RESPONSE                           LINE 23 COL 20
                   PIC 99 TO WS-USER-RESPONSE.
               10 VALUE "ENTER THE NUMBER OF THE PROGRAM TO RUN"
                                                         LINE 23 COL 23.
           05 ERROR-LINE.
               10 ERROR-MESSAGE                          LINE 21 COL 30
                   PIC X(20) FROM WS-ERROR-MESSAGE.
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **


           PERFORM UNTIL END-LOOP = 'T'
               DISPLAY PARTS-HOME-SCREEN
               ACCEPT PARTS-HOME-SCREEN
               PERFORM 100-VALIDATE-INPUT
           END-PERFORM.


           STOP RUN.
      ** add other procedures here

       100-VALIDATE-INPUT.

           MOVE SPACES TO WS-ERROR-MESSAGE.
           MOVE SPACES TO WS-PROGRAM-TO-CALL.

           EVALUATE TRUE
               WHEN WS-USER-RESPONSE = 01
                   MOVE "CHOSE OPTION 1" TO WS-ERROR-MESSAGE
               WHEN WS-USER-RESPONSE = 02
                   MOVE "CHOSE OPTION 2" TO WS-ERROR-MESSAGE
               WHEN WS-USER-RESPONSE = 03
                   MOVE "CHOSE OPTION 3" TO WS-ERROR-MESSAGE
               WHEN WS-USER-RESPONSE = 04
                   MOVE 'PARTS_ADD' TO WS-PROGRAM-TO-CALL
               WHEN WS-USER-RESPONSE = 10
                   MOVE "T" TO END-LOOP
               WHEN OTHER
                   MOVE "INVALID MENU OPTION" TO WS-ERROR-MESSAGE
           END-EVALUATE.

           IF WS-PROGRAM-TO-CALL NOT EQUAL TO SPACES THEN
               CALL WS-PROGRAM-TO-CALL
           END-IF.

           MOVE "  " TO WS-USER-RESPONSE.
           MOVE "  " TO USER-RESPONSE.

       END PROGRAM PARTS_MAIN.

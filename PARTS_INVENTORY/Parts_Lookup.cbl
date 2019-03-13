      ******************************************************************
      * Author: Brock Sharp
      * Date: 03/09/2019
      * Purpose: Lookup a part's information by entering the part-ID
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PARTS_LOOKUP.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT FIN ASSIGN TO "../PARTLIST.DAT"
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS WS-PART-ID.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD FIN RECORD CONTAINS 67 CHARACTERS.
           01 REC-IO.
               05 PARTID       PIC 9(5).
               05 PARTNAME     PIC X(15).
               05 PARTDESC     PIC X(35).
               05 PARTPRICE    PIC $ZZ9.99.
               05 PARTSUPP     PIC 9(5).
      *-----------------------
       WORKING-STORAGE SECTION.
       COPY PART_DEF REPLACING ==:TAG:== BY ==WS==.
       01 WS-DONE              PIC X VALUE "N".
       01 WS-VALID             PIC X VALUE "N".
       SCREEN SECTION.
       01 PART-ID-INPUT-SCREEN.
           05 TITLE-SECTION.
               10 VALUE "PARTS INVENTORY MAINTENANCE" BLANK SCREEN
                   LINE 1 COL 29.
               10 VALUE "-----------------------------------------------
      -             "--------------------------------"
                  LINE 2 COL 1.
           05 DATA-ENTRY-SECTION.
               10 PART-ID-FIELD.
                   20 VALUE "Part ID: "                   LINE 5 COL 25.
                   20 PART-ID PIC 9(5)
                       FROM WS-PART-ID
                       TO WS-PART-ID                      LINE 5 COL 34.
       01 OUTPUT-SCREEN.
           05 TITLE-SECTION.
               10 VALUE "PARTS INVENTORY MAINTENANCE" BLANK SCREEN
                   LINE 1 COL 29.
               10 VALUE "-----------------------------------------------
      -             "--------------------------------"
                  LINE 2 COL 1.
           05 DATA-SECTION.
               10 PART-ID-FIELD.
                   20 VALUE "Part ID: "                   LINE 5 COL 25.
                   20 PART-ID PIC 9(5) FROM WS-PART-ID    LINE 5 COL 34.
               10 PART-SUPP-FIELD.
                   20 VALUE "Supplier ID: "               LINE 7 COL 21.
                   20 PART-SUPP PIC 9(5)
                       FROM WS-PART-SUPP                  LINE 7 COL 34.
               10 PART-NAME-FIELD.
                   20 VALUE "Part Name: "                 LINE 9 COL 23.
                   20 PART-NAME PIC X(15)
                       FROM WS-PART-NAME                  LINE 9 COL 34.
               10 PART-DESC-FIELD.
                   20 VALUE "Description: "              LINE 11 COL 21.
                   20 PART-DESC PIC X(35)
                       FROM WS-PART-DESC                 LINE 11 COL 34.
               10 PART-PRICE-FIELD.
                   20 VALUE "Part Price: "               LINE 13 COL 22.
                   20 PART-PRICE PIC 999V99
                       FROM WS-PART-PRICE                LINE 13 COL 34.
           05 USER-RESPONSE-SECTION.
               10 RESPONSE-FIELD.
                   20 USER-RESPONSE PIC X TO WS-DONE     LINE 20 COL 30.
                   20 VALUE "ENTER 'Y' WHEN FINISHED, ENTER TO RESTART"
                                                         LINE 20 COL 32.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
           OPEN INPUT FIN.
      *      STAY ON SUBPROGRAM UNTIL USER INDICATES THEY ARE DONE
            PERFORM UNTIL WS-DONE = "Y" OR WS-DONE = "y"

      *          RESET DATA FIELDS
                MOVE "N" TO WS-VALID
                MOVE SPACES TO WS-PART


      *         STAY ON INPUT SCREEN UNTIL VALID ID IS RECEIVED
               PERFORM UNTIL WS-VALID = "Y"
      *             DISPLAY SCREEN FOR INPUT
                   DISPLAY PART-ID-INPUT-SCREEN END-DISPLAY
                   ACCEPT PART-ID-INPUT-SCREEN END-ACCEPT
      *             READ INDEX FILE INTO WS-PART, VALIDATE PART ID
                   READ FIN INTO WS-PART KEY IS WS-PART-ID
                       INVALID KEY
                           DISPLAY "BAD KEY" LINE 18 COL 30
                           MOVE "Y" TO WS-VALID
                       NOT INVALID KEY
                           MOVE "Y" TO WS-VALID
                   END-READ

               END-PERFORM

               DISPLAY OUTPUT-SCREEN END-DISPLAY
               ACCEPT OUTPUT-SCREEN END-ACCEPT
      *     DISPLAY RESULT SCREEN.
      *     GO BACK TO FIRST DISPLAY, UNLESS A BUTTON OR CODE IS ENTERED
            END-PERFORM.

           CLOSE FIN.

           GOBACK.
       EXIT PARAGRAPH.
      ** add other procedures here
       END PROGRAM PARTS_LOOKUP.

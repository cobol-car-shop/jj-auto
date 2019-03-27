      ******************************************************************
      * Author: Brock Sharp
      * Date: 03/09/2019
      * Purpose: Lookup a part's information by entering the part-ID
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PARTS_LOOKUP IS INITIAL.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT FIN ASSIGN TO "..\PARTLIST.DAT"
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS REC-PART-ID.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD FIN RECORD CONTAINS 65 CHARACTERS.
           COPY PART_DEF REPLACING ==:TAG:== BY ==REC==.
      *-----------------------
       WORKING-STORAGE SECTION.
       COPY PART_DEF REPLACING ==:TAG:== BY ==WS==.
       01 WS-DONE              PIC X VALUE "N".
       01 WS-VALID             PIC X VALUE "N".
       01 WS-ERROR-MESSAGE     PIC X(40).
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
                   20 ERROR-MESSAGE PIC X(40)
                       FROM WS-ERROR-MESSAGE             LINE 22 COL 30.
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
                   20 USER-RESPONSE PIC X TO WS-DONE     LINE 20 COL 23.
                   20 VALUE "'Y' WHEN FINISHED, 'D' TO DELETE, ENTER TO
      -            "RESTART"                             LINE 20 COL 25.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
           OPEN I-O FIN.
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

                   MOVE WS-PART-ID TO REC-PART-ID
      *             READ INDEX FILE INTO WS-PART, VALIDATE PART ID
                   READ FIN
                       INVALID KEY
                           MOVE "BAD KEY" TO WS-ERROR-MESSAGE
                           MOVE "N" TO WS-VALID
                       NOT INVALID KEY
                           MOVE "Y" TO WS-VALID
                           MOVE REC-PART TO WS-PART
                   END-READ

               END-PERFORM

               DISPLAY OUTPUT-SCREEN END-DISPLAY
               ACCEPT OUTPUT-SCREEN END-ACCEPT

               IF WS-DONE = "D" THEN
                   PERFORM DELETE-PARA
               END-IF

            END-PERFORM.

           CLOSE FIN.

           GOBACK.
       EXIT PARAGRAPH.
      ** add other procedures here

       DELETE-PARA.
           DELETE FIN RECORD
               INVALID KEY
                   MOVE "ERROR DELETING RECORD" TO WS-ERROR-MESSAGE
           END-DELETE.
       EXIT PARAGRAPH.
       END PROGRAM PARTS_LOOKUP.

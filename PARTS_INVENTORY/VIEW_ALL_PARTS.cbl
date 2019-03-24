      ******************************************************************
      * Author: Brock Sharp
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. VIEW_ALL_PARTS.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT IDXFILE ASSIGN TO "..\PARTLIST.DAT"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS REC-PART-ID.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD IDXFILE RECORD CONTAINS 67 CHARACTERS.
           COPY PART_DEF REPLACING ==:TAG:== BY ==REC==.
      *-----------------------
       WORKING-STORAGE SECTION.
       *> The variables passed to any called programs
       01  LS-RESPONSE     PIC 99.
       COPY PART_DEF REPLACING ==:TAG:== BY ==LS==.
       01  WS-EOF          PIC X VALUE 'F'.

       01  WS-RESPONSE-TEXT    PIC X(60).

       *> The tableS used to store the data displayed on screen
       01 WS-INPUT-FIELDS.
           05 WS-USER-INPUT PIC XX OCCURS 10 TIMES.
       01 WS-PART-TABLE.
               05 WS-PART-ROW OCCURS 10 TIMES INDEXED BY ROW-IDX.
                   10 WS-PART-ID       PIC 9(5).
                   10 FILLER           PIC X VALUE SPACE.
                   10 WS-PART-NAME     PIC X(15).
                   10 FILLER           PIC X VALUE SPACE.
                   10 WS-PART-DESC     PIC X(35).
                   10 FILLER           PIC X VALUE SPACE.
                   10 WS-PART-PRICE    PIC 999.99.
                   10 FILLER           PIC X VALUE SPACE.
                   10 WS-PART-SUPP     PIC 9(5).

       SCREEN SECTION.
       01 BROWSE-SCREEN.
           05 TITLE-SECTION.
               10 VALUE "PARTS INVENTORY MAINTENANCE" BLANK SCREEN
                   LINE 1 COL 29.
               10 VALUE "-----------------------------------------------
      -             "--------------------------------"
                  LINE 2 COL 1.
           05 HEADER-SECTION.
               10 VALUE "| ID "                          LINE  3 COL  6.
               10 VALUE "|   PART NAME  "                LINE  3 COL 12.
               10 VALUE "|         PART DESCRIPTION          "
                                                         LINE  3 COL 28.
               10 VALUE "|PRICE "                        LINE  3 COL 64.
               10 VALUE "| SID |"                        LINE  3 COL 71.
           05 PART-LINES-SECTION.
                *> ROW 1
               10 PIC XX USING WS-USER-INPUT(1)          LINE  5 COL  4.
               10 PIC X(70) USING WS-PART-ROW(1)                 COL  7.
                *> ROW 2
               10 PIC XX USING WS-USER-INPUT(2)          LINE  7 COL  4.
               10 PIC X(70) USING WS-PART-ROW(2)                 COL  7.
                *> ROW 3
               10 PIC XX USING WS-USER-INPUT(3)          LINE  9 COL  4.
               10 PIC X(70) USING WS-PART-ROW(3)                 COL  7.
                *> ROW 4
               10 PIC XX USING WS-USER-INPUT(4)          LINE 11 COL  4.
               10 PIC X(70) USING WS-PART-ROW(4)                 COL  7.
                *> ROW 5
               10 PIC XX USING WS-USER-INPUT(5)          LINE 13 COL  4.
               10 PIC X(70) USING WS-PART-ROW(5)                 COL  7.
                *> ROW 6
               10 PIC XX USING WS-USER-INPUT(6)          LINE 15 COL  4.
               10 PIC X(70) USING WS-PART-ROW(6)                 COL  7.
                *> ROW 7
               10 PIC XX USING WS-USER-INPUT(7)          LINE 17 COL  4.
               10 PIC X(70) USING WS-PART-ROW(7)                 COL  7.
                *> ROW 8
               10 PIC XX USING WS-USER-INPUT(8)          LINE 19 COL  4.
               10 PIC X(70) USING WS-PART-ROW(8)                 COL  7.
                *> ROW 9
               10 PIC XX USING WS-USER-INPUT(9)          LINE 21 COL  4.
               10 PIC X(70) USING WS-PART-ROW(9)                 COL  7.
                *> ROW 10
               10 PIC XX USING WS-USER-INPUT(10)         LINE 23 COL  4.
               10 PIC X(70) USING WS-PART-ROW(10)                COL  7.
           05 USER-INFO-SECTION.
              10  DISP-MORE-REC PIC XXXXXX               LINE 24 COL 70.

       01 CONFIRMATION-SCREEN.
           05 TITLE-SECTION.
               10 VALUE "PARTS INVENTORY MAINTENANCE" BLANK SCREEN
                   LINE 1 COL 29.
               10 VALUE "-----------------------------------------------
      -             "--------------------------------"
                  LINE 2 COL 1.
           05 MESSAGE-SECTION.
               10 PIC X(60) USING WS-RESPONSE-TEXT       LINE 12 COL 20.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **

           OPEN I-O IDXFILE.

           *> Perform until there are no more records
           PERFORM UNTIL DISP-MORE-REC = "BOTTOM"
               *> Perform 10 times --- Populate the screen
               PERFORM VARYING ROW-IDX FROM 1 BY 1
                       UNTIL ROW-IDX > 10
                   *> Remove any data from last run in user input field
                   MOVE SPACES TO WS-USER-INPUT(ROW-IDX)

                   *> Populate the parts table
                   IF WS-EOF = 'F' THEN
                       PERFORM READ-PART-INTO-TABLE-ROW
                   ELSE
                       PERFORM CLEAR-OUT-EMPTY-ROW
                   END-IF
               END-PERFORM

               DISPLAY BROWSE-SCREEN
               ACCEPT BROWSE-SCREEN

               *> Handle any fields that have been marked
               PERFORM PROCESS-USER-INPUT-FIELDS

           END-PERFORM.

           CLOSE IDXFILE.

           STOP RUN.
           *> End of the main method

       *> Reads the next part from the file, and moves it into the row
       READ-PART-INTO-TABLE-ROW.

           READ IDXFILE NEXT RECORD INTO REC-PART
               AT END
                   MOVE "BOTTOM" TO DISP-MORE-REC
                   MOVE 'T' TO WS-EOF
               NOT AT END
                   MOVE "MORE.." TO DISP-MORE-REC
           END-READ

           MOVE REC-PART-ID TO WS-PART-ID(ROW-IDX).
           MOVE REC-PART-NAME TO WS-PART-NAME(ROW-IDX).
           MOVE REC-PART-DESC TO WS-PART-DESC(ROW-IDX).
           MOVE REC-PART-PRICE TO WS-PART-PRICE(ROW-IDX).
           MOVE REC-PART-SUPP TO WS-PART-SUPP(ROW-IDX).

       END-PARAGAPH.

       *> Puts XX into the user input, and empties the blank part data
       CLEAR-OUT-EMPTY-ROW.

           *> Clear out the row from last round
           MOVE SPACES TO WS-PART-ROW(ROW-IDX).
           *> Put an XX in any row that doesn't have data
           MOVE 'XX' TO WS-USER-INPUT(ROW-IDX).

       END-PARAGAPH.

       PROCESS-USER-INPUT-FIELDS.

           PERFORM VARYING ROW-IDX FROM 1 BY 1 UNTIL ROW-IDX > 10

               *> Reset the response variable
               MOVE 00 TO LS-RESPONSE

               *> Transfer the formatted part to a default part definition
               MOVE WS-PART-ID(ROW-IDX) TO LS-PART-ID
               MOVE WS-PART-NAME(ROW-IDX) TO LS-PART-NAME
               MOVE WS-PART-DESC(ROW-IDX) TO LS-PART-DESC
               MOVE WS-PART-PRICE(ROW-IDX) TO LS-PART-PRICE
               MOVE WS-PART-SUPP(ROW-IDX) TO LS-PART-SUPP

               *> Parse the user input for an action to complete
               EVALUATE WS-USER-INPUT(ROW-IDX)
                   WHEN 'D'
                       CALL 'DELETE_PART' USING LS-PART, LS-RESPONSE
                   WHEN 'd'
                       CALL 'DELETE_PART' USING LS-PART, LS-RESPONSE
                   WHEN 'U'
                       CALL 'UPDATE_PART' USING LS-PART, LS-RESPONSE
                   WHEN 'u'
                       CALL 'UPDATE_PART' USING LS-PART, LS-RESPONSE
                   WHEN 'A'
                       CALL 'ADD_PART'  USING LS-PART, LS-RESPONSE
                   WHEN 'a'
                       CALL 'ADD_PART'  USING LS-PART, LS-RESPONSE
               END-EVALUATE

               *> Generate the appropriate confirmation message
               EVALUATE LS-RESPONSE
                   WHEN 01
                       MOVE "OPERATION SUCCESSFUL, PRESS ENTER TO CON
      -                "TINUE. " TO WS-RESPONSE-TEXT
                   WHEN 99
                       MOVE "OPERATION FAILED, PRESS ENTER TO CONTINUE."
                       TO WS-RESPONSE-TEXT
               END-EVALUATE

               DISPLAY LS-RESPONSE

               *> Display the confirmation message, if any
               IF LS-RESPONSE > 00 THEN
                   ACCEPT CONFIRMATION-SCREEN
               END-IF

           END-PERFORM.

       END-PARAGAPH.
       END PROGRAM VIEW_ALL_PARTS.

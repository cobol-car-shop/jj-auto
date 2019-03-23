      ******************************************************************
      * Author: Brock Sharp
      * Date: 03/22/19
      * Purpose: Adds the part passed to the module into the index file
      * RETURN CODES:
      *    00 -> SUCCESSFUL ADDING PART - PART RETURNED
      *    99 -> PART NUMBER TAKEN - PART NOT ADDED, PART NOT RETURNED
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. ADD_PART.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT IDXFILE ASSIGN TO "..\PARTLIST.DAT"
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS REC-PART-ID.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD IDXFILE RECORD CONTAINS 67 CHARACTERS.
           COPY PART_DEF REPLACING ==:TAG:== BY ==REC==.
      *-----------------------
       WORKING-STORAGE SECTION.
       01  WS-VALID-PART PIC X VALUE 'F'.
      *-----------------------
       LINKAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       COPY PART_DEF REPLACING ==:TAG:== BY ==LS==.
       01  LS-RESULT-CODE      PIC 99.
       PROCEDURE DIVISION USING LS-PART, LS-RESULT-CODE.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       0100-BEGIN.
               OPEN I-O IDXFILE.

               IF LS-PART-ID = 00000 THEN
                   PERFORM 0110-GENERATE-PART-ID
               ELSE
                   PERFORM 0120-VERIFY-PART-ID
               END-IF.

      ***** LS-PART-ID should be valid at this stage
               IF WS-VALID-PART = 'T'
                   PERFORM 0200-ADD-PART
               END-IF.

               CLOSE IDXFILE.

               GOBACK.

        STOP RUN.

      *********************************************************
      * Assigns the first available part number to LS-PART-ID *
      *********************************************************
       0110-GENERATE-PART-ID.
           PERFORM UNTIL WS-VALID-PART = 'T'
               ADD 1 TO REC-PART-ID
               READ IDXFILE
                   INVALID KEY
                       MOVE 'T' TO WS-VALID-PART
               END-READ
           END-PERFORM.
       END-PARAGRAPH.

      ************************************************************
      * Verifies that the part number in LS-PART-ID is not taken *
      ************************************************************
       0120-VERIFY-PART-ID.
           READ IDXFILE
               INVALID KEY
      *            99 -> PART NUMBER TAKEN, RETURN TO CALLING PROGRAM
                   MOVE 99 TO LS-RESULT-CODE
               NOT INVALID KEY
                   MOVE 'T' TO WS-VALID-PART
           END-READ.
       END-PARAGRAPH.

      *************************************
      * Writes the part to the index file *
      *************************************
       0200-ADD-PART.
           WRITE REC-PART FROM LS-PART
               INVALID KEY
                   MOVE 99 TO LS-RESULT-CODE
               NOT INVALID KEY
                   MOVE 00 TO LS-RESULT-CODE
           END-WRITE.
       END-PARAGRAPH.

       END PROGRAM ADD_PART.

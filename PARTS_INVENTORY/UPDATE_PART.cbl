      ******************************************************************
      * Author: Brock Sharp
      * Date: 03/22/19
      * Purpose: Updates a record in the index file, returning new rec
      * RETURN CODES:
      *     01 -> UPDATE SUCCESSFUL - PART RETURNED
      *     99 -> UPDATE FAILED, BAD KEY - NO PART RETURNED
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. UPDATE_PART.
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
       FD IDXFILE RECORD CONTAINS 70 CHARACTERS.
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

           PERFORM 0150-VERIFY-PART-ID.

           IF WS-VALID-PART = 'T' THEN
               PERFORM 0200-UPDATE-PART
           END-IF.

           CLOSE IDXFILE.

           GOBACK.

        STOP RUN.

      *********************************
      * Verifies that the part exists *
      *********************************
       0150-VERIFY-PART-ID.

           MOVE LS-PART TO REC-PART.

           READ IDXFILE
               INVALID KEY
                   MOVE 99 TO LS-RESULT-CODE
               NOT INVALID KEY
                   MOVE 01 TO LS-RESULT-CODE
                   MOVE 'T' TO WS-VALID-PART
           END-READ.

       END-PARAGRAPH.
      **************************************
      * Updates the part in the index file *
      **************************************
       0200-UPDATE-PART.

           REWRITE REC-PART FROM LS-PART
               INVALID KEY
                   MOVE 99 TO LS-RESULT-CODE
               NOT INVALID KEY
                   MOVE 01 TO LS-RESULT-CODE
                   MOVE REC-PART TO LS-PART
           END-REWRITE.

       END-PARAGRAPH.

       END PROGRAM UPDATE_PART.

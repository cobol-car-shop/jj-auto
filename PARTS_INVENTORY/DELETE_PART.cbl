      ******************************************************************
      * Author: Brock Sharp
      * Date: 03/22/19
      * Purpose: Deletes a part from the index file, given an empty part
      *            variable. Will also return the part to be deleted.
      * RETURN CODES:
      *     01 -> PART DELETED - PART RETURNED
      *     99 -> PART NOT FOUND - NO PART RETURNED
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. DELETE_PART.
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
       FD IDXFILE RECORD CONTAINS 70 CHARACTERS.
           COPY PART_DEF REPLACING ==:TAG:== BY ==REC==.
      *-----------------------
       WORKING-STORAGE SECTION.
       01  WS-VALID-PART   PIC X VALUE 'F'.
      *-----------------------
       LINKAGE SECTION.
       COPY PART_DEF REPLACING ==:TAG:== BY ==LS==.
       01  LS-RESULT-CODE  PIC 99.
       PROCEDURE DIVISION USING LS-PART, LS-RESULT-CODE.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       0100-BEGIN.

           OPEN I-O IDXFILE.

           PERFORM 0200-STASH-AND-VERIFY-PART.

           IF WS-VALID-PART = 'T' THEN
               PERFORM 0300-DELETE-PART
           END-IF.

           CLOSE IDXFILE.

           GOBACK.

        STOP RUN.

      ******************************************************************
      * Verifies that the part exists, and stores the part in LS-PART. *
      ******************************************************************
       0200-STASH-AND-VERIFY-PART.

           MOVE LS-PART TO REC-PART.

           READ IDXFILE INTO LS-PART
               INVALID KEY
                   MOVE 99 TO LS-RESULT-CODE
               NOT INVALID KEY
                   MOVE 01 TO LS-RESULT-CODE
                   MOVE 'T' TO WS-VALID-PART
           END-READ.

       END-PARAGRAPH.

      *****************************************
      * Deletes the part from the index file. *
      *****************************************
       0300-DELETE-PART.

           DELETE IDXFILE RECORD
               INVALID KEY
                   MOVE 99 TO LS-RESULT-CODE
               NOT INVALID KEY
                   MOVE 01 TO LS-RESULT-CODE
           END-DELETE.

       END-PARAGRAPH.
       END PROGRAM DELETE_PART.

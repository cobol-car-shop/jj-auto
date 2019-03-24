      ******************************************************************
      * Author: Brock Sharp
      * Date: 03/22/19
      * Purpose: Reads the part from the index file, given a part var
      *            that is empty besides a part number
      * RETURN CODES:
      *     01 -> SUCCESSFUL READING PART - PART RETURNED
      *     99 -> INVALID PART NUMBER - PART NOT RETURNED
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. READ_PART.
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
      *-----------------------
       LINKAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       COPY PART_DEF REPLACING ==:TAG:== BY ==LS==.
       01  LS-RESULT-CODE      PIC 99.

       PROCEDURE DIVISION USING LS-PART, LS-RESULT-CODE.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       0100-BEGIN.

           OPEN INPUT IDXFILE.

           MOVE LS-PART TO REC-PART.

           READ IDXFILE INTO LS-PART
               INVALID KEY
                   MOVE 99 TO LS-RESULT-CODE
               NOT INVALID KEY
                   MOVE 01 TO LS-RESULT-CODE
           END-READ.

           CLOSE IDXFILE.

           GOBACK.
        STOP RUN.

       END PROGRAM READ_PART.

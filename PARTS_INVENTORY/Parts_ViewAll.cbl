      ******************************************************************
      * Author: Brock Sharp
      * Date: 03/15/19
      * Purpose: A screen to view a listing of all part descriptions
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PARTS_VIEWALL.
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
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            STOP RUN.
      ** add other procedures here
       END PROGRAM PARTS_VIEWALL.

*>****************************************************************
*> Author: Joseph Warren
*> Date: 3/27/2019
*> Purpose: Add a sale to the index file
*> Tectonics: cobc
*>*****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. ADD_SALE.
ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
    FILE-CONTROL.
    SELECT OPTIONAL INDEX-FILE-SALE
        ASSIGN TO '../../INDEXES/SALE.IDX'
        ORGANIZATION IS INDEXED
        ACCESS IS SEQUENTIAL
        RECORD KEY IS IDX-saleID
        ALTERNATE RECORD KEY IS IDX-empID WITH DUPLICATES
        ALTERNATE RECORD KEY IS IDX-carVIN WITH DUPLICATES
        ALTERNATE RECORD KEY IS IDX-custID WITH DUPLICATES.

DATA DIVISION.
FILE SECTION.
FD INDEX-FILE-SALE
        RECORD CONTAINS 161 CHARACTERS.
    COPY SALE_DEF REPLACING ==:TAG:== BY ==IDX==.

WORKING-STORAGE SECTION.
LINKAGE SECTION.
   COPY SALE_DEF REPLACING ==:TAG:== BY ==LS==.

PROCEDURE DIVISION USING LS-Sale.
MAIN-PROCEDURE.
    OPEN EXTEND INDEX-FILE-SALE
    WRITE IDX-SALE FROM LS-Sale
      INVALID KEY DISPLAY 'INVALID KEY : ' IDX-saleID ' | IGNORING'
      NOT INVALID KEY DISPLAY 'KEY ACCEPTED: ' IDX-saleID ' ALT: ' IDX-empID
    END-WRITE
    CLOSE INDEX-FILE-SALE
   EXIT PARAGRAPH.
END PROGRAM ADD_SALE.

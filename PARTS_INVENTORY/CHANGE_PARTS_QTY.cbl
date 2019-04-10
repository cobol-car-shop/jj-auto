      ******************************************************************
      * Author: Brock Sharp
      * Date: 04/08/19
      * Purpose: Update the quantity of parts on hand.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHANGE_PARTS_QTY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY PART_DEF REPLACING ==:TAG:== BY ==WS==.
       01 WS-RETURN-CODE       PIC 99.
       LINKAGE SECTION.
       COPY PART_DEF REPLACING ==:TAG:== BY ==LS==.
       SCREEN SECTION.
       01 UPDATE-SCREEN.
           05 TITLE-SECTION.
               10 VALUE "PARTS INVENTORY MAINTENANCE" BLANK SCREEN
                   LINE 1 COL 29.
               10 VALUE "-----------------------------------------------
      -             "--------------------------------"
                  LINE 2 COL 1.
           05 PART-DISPLAY.
               10 VALUE "PART ID: "                      LINE 3 COL 20.
               10 PIC 9(5) USING WS-PART-ID                     COL 29.
               10 VALUE "PART NAME: "                    LINE 5 COL 18.
               10 PIC X(15) USING WS-PART-NAME                  COL 29.
               10 VALUE "PART DESCRIPTION: "             LINE 7 COL 11.
               10 PIC X(35) USING WS-PART-DESC                  COL 29.
               10 VALUE "PART ONHAND: "                  LINE 9 COL 15.
               10 PIC 99 USING WS-PART-ON-HAND                  COL 29.
       PROCEDURE DIVISION USING LS-PART.
       MAIN-PROCEDURE.
           *> LOAD THE PASSED PART ID INTO MEMORY
           MOVE LS-PART-ID TO WS-PART-ID.
           CALL "READ_PART" USING WS-PART, WS-RETURN-CODE.
           IF WS-RETURN-CODE > 01 THEN
               DISPLAY "ERROR READING PART ", WS-PART-ID
               AT LINE 20 COL 10 END-DISPLAY
               ACCEPT OMITTED
           ELSE
               DISPLAY UPDATE-SCREEN
               ACCEPT UPDATE-SCREEN
               CALL "UPDATE_PART" USING WS-PART, WS-RETURN-CODE
               IF WS-RETURN-CODE > 01 THEN
                   DISPLAY "ERROR UPDATING PART", WS-PART-ID
                   AT LINE 20 COL 10 END-DISPLAY
                   ACCEPT OMITTED
               END-IF
           END-IF.

               DISPLAY WS-PART AT LINE 20 COL 10.
               ACCEPT OMITTED.

           GOBACK.
       END PROGRAM CHANGE_PARTS_QTY.

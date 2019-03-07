*>****************************************************************
*> Author: Joseph Warren
*> Date: 2/27/2019
*> Purpose: Contains test code for the admin module
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. ADMIN_TEST.
ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
    FILE-CONTROL.
    SELECT INDEX-FILE-EMP
        ASSIGN TO 'EMPLOYEE.IDX'
        ORGANIZATION IS INDEXED
        ACCESS IS SEQUENTIAL
        RECORD KEY IS IDX-empID
        ALTERNATE RECORD KEY IS IDX-lName WITH DUPLICATES.

DATA DIVISION.
FILE SECTION.
FD INDEX-FILE-EMP
        RECORD CONTAINS 161 CHARACTERS.
    COPY EMP_DEF REPLACING ==:TAG:== BY ==IDX==.

WORKING-STORAGE SECTION.
   COPY EMP_DEF REPLACING ==:TAG:== BY ==WS==.
   COPY SERVICE_DEF REPLACING ==:TAG:== BY ==WS==.

PROCEDURE DIVISION.
MAIN-PROCEDURE.

    PERFORM 200-TEST-ADD-EMP

    STOP RUN.

200-TEST-ADD-EMP.
    DISPLAY "STARTING ADD_EMP TEST"
    MOVE 12345 TO WS-empID
    MOVE "FirstName" TO WS-fName
    MOVE "LastName" TO WS-lName
    MOVE 123456789 TO WS-social
    MOVE 1234567890 TO WS-phone
    MOVE "123email@server.com" TO WS-email
    MOVE 19191.34 TO WS-wage
    MOVE "Sales" TO WS-position
   DISPLAY WS-Employee
    CALL 'ADD_EMP' USING WS-Employee
    DISPLAY "WRITING COMPLETE"
    MOVE SPACES TO WS-Employee
    MOVE 12345 TO WS-empID
    CALL 'READ_EMP' USING WS-Employee
EXIT PARAGRAPH.

END PROGRAM ADMIN_TEST.

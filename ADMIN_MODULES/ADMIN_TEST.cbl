*>****************************************************************
*> Author: Joseph Warren
*> Date: 2/27/2019
*> Purpose: Contains test code for the admin module
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. ADMIN_TEST.
ENVIRONMENT DIVISION.
DATA DIVISION.
FILE SECTION.

WORKING-STORAGE SECTION.
   COPY EMP_DEF REPLACING ==:TAG:== BY ==WS==.
   COPY SERVICE_DEF REPLACING ==:TAG:== BY ==WS==.
   COPY SALE_DEF REPLACING ==:TAG:== BY ==WSS==.

01 PARAMETRES.
   02 PA-RETURN-CODE PIC 99 VALUE 0.
   02 PA-OPTION-CODE PIC 99 VALUE 0.

PROCEDURE DIVISION.
MAIN-PROCEDURE.

    PERFORM 200-TEST-ADD-EMP

    STOP RUN.

200-TEST-ADD-EMP.
    DISPLAY "STARTING ADD_EMP TEST"
    MOVE 12346 TO WS-empID
    MOVE "FirstName" TO WS-fName
    MOVE "LastName" TO WS-lName
    MOVE 123456789 TO WS-social
    MOVE 1234567890 TO WS-phone
    MOVE "123email@server.com" TO WS-email
    MOVE 19191.34 TO WS-wage
    MOVE "Sales" TO WS-position
   DISPLAY WS-Employee
    CALL 'ADD_EMP' USING WS-Employee
    MOVE 12347 TO WS-empID
    CALL 'ADD_EMP' USING WS-Employee
    MOVE 12348 TO WS-empID
    CALL 'ADD_EMP' USING WS-Employee
    MOVE 12330 TO WS-empID
    CALL 'ADD_EMP' USING WS-Employee
    MOVE SPACES TO WS-Employee
    MOVE 12346 TO WS-empID
    CALL 'READ_EMP' USING WS-Employee

    MOVE 12343 TO WSS-saleID
    MOVE 22226 TO WSS-empID
    MOVE "20000106" TO WSS-saleDate
    CALL 'ADD_SALE' USING WSS-Sale

    MOVE 12344 TO WSS-saleID
    MOVE 22225 TO WSS-empID
    MOVE "20000105" TO WSS-saleDate
    CALL 'ADD_SALE' USING WSS-Sale

    MOVE 12345 TO WSS-saleID
    MOVE 22224 TO WSS-empID
    MOVE "20000104" TO WSS-saleDate
    CALL 'ADD_SALE' USING WSS-Sale

    MOVE 12346 TO WSS-saleID
    MOVE 22223 TO WSS-empID
    MOVE "20000103" TO WSS-saleDate
    CALL 'ADD_SALE' USING WSS-Sale

    MOVE 12347 TO WSS-saleID
    MOVE 22222 TO WSS-empID
    MOVE "20000102" TO WSS-saleDate
    CALL 'ADD_SALE' USING WSS-Sale

    MOVE 12348 TO WSS-saleID
    MOVE 22221 TO WSS-empID
    MOVE "20000101" TO WSS-saleDate
    CALL 'ADD_SALE' USING WSS-Sale

    MOVE 00000 TO WSS-saleID
    MOVE 00000 TO WSS-empID
    MOVE 01 TO PA-OPTION-CODE
    DISPLAY SPACES
    CALL 'READ_SALE' USING WSS-Sale, PARAMETRES
    DISPLAY PARAMETRES
    DISPLAY WSS-Sale
EXIT PROGRAM.

END PROGRAM ADMIN_TEST.

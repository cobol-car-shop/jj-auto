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
01 PARAMETRES.
    02 PA-RETURN-CODE PIC 99 VALUE 0.

    COPY EMP_DEF REPLACING ==:TAG:== BY ==WS==.

PROCEDURE DIVISION.
MAIN-PROCEDURE.
    DISPLAY "Hello world"
    CALL 'ADMIN' USING WS-Employee
    *>CALL 'ADMIN_ADD_EMPLOYEE' USING WS-EMPLOYEE
    DISPLAY "PA TEST CODE: ", PA-RETURN-CODE
    STOP RUN.
END PROGRAM ADMIN_TEST.

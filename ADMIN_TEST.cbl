*>****************************************************************
*> Author: Joseph Warren
*> Date: 2/27/2019
*> Purpose: Contains test code for the admin module
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. ADMIN_TEST.
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
01 PARAMETRES.
    02 PA-RETURN-CODE PIC 99 VALUE 0.
PROCEDURE DIVISION.
MAIN-PROCEDURE.
    DISPLAY "Hello world"
    CALL 'ADMIN' USING PA-RETURN-CODE
    DISPLAY "PA TEST CODE: ", PA-RETURN-CODE
    STOP RUN.
END PROGRAM ADMIN_TEST.

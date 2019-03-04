*>****************************************************************
*> Author: Joseph Warren
*> Date: 2/27/2019
*> Purpose: Callable module that contains admin functions
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. ADMIN.
DATA DIVISION.
WORKING-STORAGE SECTION.
*> Employee data
*> Customer data
*> Sales data
LINKAGE SECTION.
01 PARAMETRES.
    02 PA-RETURN-CODE PIC 99 VALUE 0.
PROCEDURE DIVISION USING PARAMETRES.
MAIN-PROCEDURE.
    DISPLAY "Hello world FROM THE ADMIN MODULE"
    MOVE 0 TO PA-RETURN-CODE
    EXIT PARAGRAPH.
END PROGRAM ADMIN.

*> Add a new employee

*> Update an employee

*> Deactivate employee

*> Add new service and part types

*> Access product functions for all other roles

*> Reports: Profits, Sales breakdown, Maintenance Times, and Inventory usage

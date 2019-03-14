*>****************************************************************
*> Author: Joseph Warren
*> Date: 3/6/2019
*> Purpose: Read an employee from the index file
*> Tectonics: cobc
*>*****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. READ_EMP.
ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
    FILE-CONTROL.
    SELECT OPTIONAL INDEX-FILE-EMP
        ASSIGN TO '../../INDEXES/EMPLOYEE.IDX'
        ORGANIZATION IS INDEXED
        ACCESS IS RANDOM
        RECORD KEY IS IDX-empID
        ALTERNATE RECORD KEY IS IDX-lName WITH DUPLICATES.

DATA DIVISION.
FILE SECTION.
FD INDEX-FILE-EMP
        RECORD CONTAINS 161 CHARACTERS.
    COPY EMP_DEF REPLACING ==:TAG:== BY ==IDX==.

WORKING-STORAGE SECTION.
01 MORE-RECORDS         PIC A(3)    VALUE 'YES'.

LINKAGE SECTION.
COPY EMP_DEF REPLACING ==:TAG:== BY ==LS==.

PROCEDURE DIVISION USING LS-Employee.
MAIN-PROCEDURE.
   MOVE LS-empID TO IDX-empID
   OPEN INPUT INDEX-FILE-EMP
   READ INDEX-FILE-EMP INTO LS-Employee
      INVALID KEY DISPLAY "Not found: " IDX-empID
      NOT INVALID KEY DISPLAY "Found: " LS-Employee
   END-READ
   CLOSE INDEX-FILE-EMP

   EXIT PARAGRAPH.
END PROGRAM READ_EMP.

*>****************************************************************
*> Author:Aiden Stahl
*> Date:3/12/2019
*> Purpose: To allow access to other programs in the j&jauto system based on permissions
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. JJUI.
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
LINKAGE SECTION.
01 USERNAME-IN PIC X(30).
01 PERMISSION-IN PIC A(5).
SCREEN SECTION.

PROCEDURE DIVISION.
100-MAIN-PROCEDURE.

200-Check-PERMISSIONS.
    STOP RUN.
END PROGRAM JJUI.

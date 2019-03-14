       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           CUSVW.
      *    Programmer: Jonathan Walker
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-INFO ASSIGN TO 'C:/Users/Leslie/Documents/Pro
      -                               'gramming/CIS334/CUSINFO.RPT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *     SELECT INDEX-FILE ASSIGN TO 'C:/Users/Leslie/Documents/Progra
      *-                      'mming/CIS334/CustIndexFile.DAT'
      *                       ORGANIZATION IS INDEXED
      *                       ACCESS IS SEQUENTIAL
      *                       RECORD KEY IS CUST-ID
      *                       ALTERNATE KEY IS CUST-LNAME
      *                       WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-INFO.
       01  CUST-REC.
           05  CUST-ID-REC                      PIC 9(5).
           05  CUST-FNAME-REC                   PIC X(15).
           05  CUST-LNAME-REC                   PIC X(15).
           05  CUST-PHONE-REC                   PIC 9(10).
           05  CUST-EMAIL-REC                   PIC X(35).
           05  CUST-ADDRS-REC                   PIC X(35).
           05  CUST-CITY-REC                    PIC X(15).
           05  CUST-STATE-REC                   PIC XX.
           05  CUST-ZIP-REC                     PIC 9(5).
           05  CUST-DST-REC                     PIC X.

       WORKING-STORAGE SECTION.
       01  CUS-SRCH-INFO.
           05  CUS-ID-IN                       PIC 9(5).
           05  CUS-FNAME-IN                    PIC X(15).
           05  CUS-LNAME-IN                    PIC X(15).

       01  WORK-AREAS.
           05  MORE-RECS                       PIC X(1)
                  VALUE 'Y'.
           05  DATA-OK                         PIC X(1).
           05  WS-DATE.
               10  WS-YEAR                     PIC 9999.
               10  WS-MONTH                    PIC 99.
               10  WS-DAY                      PIC 99.

       01  COLOR-LIST.
           05  BLACK                           PIC 9(1)    VALUE 0.
           05  BLUE                            PIC 9(1)    VALUE 1.
           05  GREEN                           PIC 9(1)    VALUE 2.
           05  CYAN                            PIC 9(1)    VALUE 3.
           05  RED                             PIC 9(1)    VALUE 4.
           05  MAGENTA                         PIC 9(1)    VALUE 5.
           05  BROWN                           PIC 9(1)    VALUE 6.
           05  WHITE                           PIC 9(1)    VALUE 7.

       SCREEN SECTION.
       01  SCREEN-SELECT.
           05  BLANK SCREEN
               BACKGROUND-COLOR BLACK
               FOREGROUND-COLOR GREEN.
           05 INPUT-PROMPTS.
              10 LINE 18 COLUMN 20
                 VALUE "Please enter Customer ID or First and Last Name"
                 FOREGROUND-COLOR BLUE.
              10 LINE 20 COLUMN 20
                   VALUE "Customer ID: ".
              10 LINE 22 COLUMN 20
                   VALUE "First Name: ".
              10 LINE 24 COLUMN 20
                   VALUE "Last Name: ".
           05 INPUT-FIELDS
                  REVERSE-VIDEO
                  AUTO.
              10 LINE 20 COLUMN 33  PIC 9(5)  TO CUS-ID-IN.
              10 LINE 22 COLUMN 33  PIC X(15) TO CUS-FNAME-IN.
              10 LINE 24 COLUMN 33  PIC X(15) TO CUS-LNAME-IN.
       01  SCREEN-DISPLAY.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2
               BACKGROUND-COLOR 0.
           05  OUTPUT-PROMPTS.
               10  LINE 8 COLUMN 20       VALUE "CUSTOMER ID: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "FIRST NAME: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "LAST NAME: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "MIDDLE INITIAL: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "PHONE NUMBER: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "EMAIL ADDRESS: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "STREET ADDRESS: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "CITY: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "STATE ABBREVIATION: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "ZIPCODE: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "DELETION STATUS: ".

           05  OUTPUT-FIELDS.
               10  LINE 8      COLUMN 39  PIC 9(5)  FROM CUST-ID-REC.
               10  LINE PLUS 2 COLUMN 39  PIC X(20) FROM CUST-FNAME-REC.
               10  LINE PLUS 2 COLUMN 39  PIC X(20) FROM CUST-LNAME-REC.
               10  LINE PLUS 2 COLUMN 39  PIC 9(10) FROM CUST-PHONE-REC.
               10  LINE PLUS 2 COLUMN 39  PIC X(35) FROM CUST-EMAIL-REC.
               10  LINE PLUS 2 COLUMN 39  PIC X(35) FROM CUST-ADDRS-REC.
               10  LINE PLUS 2 COLUMN 39  PIC X(15) FROM CUST-CITY-REC.
               10  LINE PLUS 2 COLUMN 39  PIC XX    FROM CUST-STATE-REC.
               10  LINE PLUS 2 COLUMN 39  PIC 9(5)  FROM CUST-ZIP-REC.
               10  LINE PLUS 2 COLUMN 39  PIC X     FROM CUST-DST-REC.

           05  INPUT-PROMPTS.
               10 LINE 30 COLUMN 20       VALUE "Search for another? ".
           05  INPUT-FIELDS
                   REVERSE-VIDEO
                   AUTO.
               10 LINE 30 COLUMN 41       PIC X TO MORE-RECS.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM CUSVW.
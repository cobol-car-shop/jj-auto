       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           CUSVW.
      *    Programmer: Jonathan Walker
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL CUS-FILE
           ASSIGN TO 'CUSTOMER.IDX'
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS CUST-ID-REC
               ALTERNATE RECORD KEY IS CUST-LNAME-REC.

       DATA DIVISION.
       FILE SECTION.
       FD  CUS-FILE.
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
               10 LINE 5 COLUMN 35        VALUE "View Customer Info".
               10 LINE 6 COLUMN 35        VALUE "-----------------".
               10 LINE 8 COLUMN 20
                         VALUE "Search for Customer by;".
               10 LINE 10 COLUMN 20       VALUE "Customer ID: ".
               10 LINE 11 COLUMN 20       VALUE "Or".
               10 LINE 12 COLUMN 20       VALUE "Customer First Name: ".
               10 LINE 13 COLUMN 20       VALUE "Customer Last  Name: ".
           05 INPUT-FIELDS
                 REVERSE-VIDEO
                 AUTO.
              10 LINE 10 COLUMN 33  PIC 9(5)  TO CUS-ID-IN.
              10 LINE 12 COLUMN 41  PIC X(15) TO CUS-FNAME-IN.
              10 LINE 13 COLUMN 41  PIC X(15) TO CUS-LNAME-IN.

       01  SCREEN-DISPLAY-F.
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
               10  LINE 30 COLUMN 20
                   VALUE "Search for another (Y/N)?".
           05  INPUT-FIELDS
                 REVERSE-VIDEO
                 AUTO.
               10 LINE 30 COLUMN 46       PIC X TO MORE-RECS.

       01  SCREEN-DISPLAY-N.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2
               BACKGROUND-COLOR 0.
           05  OUTPUT-MESSAGE.
               10 LINE 8 COLUMN 20 VALUE "No customer was found".
           05  INPUT-PROMPTS.
               10 LINE 30 COLUMN 20
                  VALUE "Search for another (Y/N)?".
           05  INPUT-FIELDS
                REVERSE-VIDEO
                AUTO.
               10 LINE 30 COLUMN 46       PIC X TO MORE-RECS.

       01  CLEAR-SCREEN.
           05  BLANK SCREEN
               FOREGROUND-COLOR GREEN
               BACKGROUND-COLOR BLACK.

       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           PERFORM UNTIL MORE-RECS = "N" OR "n"
             DISPLAY CLEAR-SCREEN
             DISPLAY SCREEN-SELECT
             ACCEPT SCREEN-SELECT
             MOVE "Y" TO DATA-OK
             IF CUS-ID-IN > 00000
               PERFORM 200-CID-RTN
             ELSE
               PERFORM 300-NAME-RTN
             END-IF
             IF DATA-OK = "F"
               DISPLAY CLEAR-SCREEN
               DISPLAY SCREEN-DISPLAY-F
               ACCEPT SCREEN-DISPLAY-F
             ELSE
               DISPLAY CLEAR-SCREEN
               DISPLAY SCREEN-DISPLAY-N
               ACCEPT  SCREEN-DISPLAY-N
             END-IF
             PERFORM 450-CLRFLD-RTN
           END-PERFORM
           CLOSE CUS-FILE
           STOP RUN.

       200-CID-RTN.
           OPEN INPUT CUS-FILE
           PERFORM UNTIL DATA-OK = "F" OR "N"
             PERFORM 400-READ-RTN
             EVALUATE TRUE
               WHEN CUS-ID-IN = CUST-ID-REC
                 IF CUST-DST-REC = "Y"
                   MOVE "N" TO DATA-OK
                 ELSE
                   MOVE "F" TO DATA-OK
                 END-IF
             END-EVALUATE
           END-PERFORM
           CLOSE CUS-FILE.

       300-NAME-RTN.
           OPEN INPUT CUS-FILE
           PERFORM UNTIL DATA-OK = "F" OR "N"
             PERFORM 400-READ-RTN
             EVALUATE TRUE
               WHEN CUS-FNAME-IN = CUST-FNAME-REC
                 EVALUATE TRUE
                   WHEN CUS-LNAME-IN = CUST-LNAME-REC
                     IF CUST-DST-REC = "Y"
                       MOVE "N" TO DATA-OK
                     ELSE
                       MOVE "F" TO DATA-OK
                     END-IF
                 END-EVALUATE
             END-EVALUATE
           END-PERFORM
           CLOSE CUS-FILE.

       400-READ-RTN.
           READ CUS-FILE
             AT END MOVE "N" TO DATA-OK
           END-READ.

       450-CLRFLD-RTN.
           MOVE 0 TO CUS-ID-IN
           MOVE SPACES TO CUS-FNAME-IN
           MOVE SPACE TO CUS-LNAME-IN.

       END PROGRAM CUSVW.

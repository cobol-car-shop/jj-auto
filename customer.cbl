       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           CUSTOMERAUDV.
      *    Programmer: Jonathan Walker
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-INFO ASSIGN TO 'C:/Users/Leslie/Documents/Pro
      -                               'gramming/CIS334/CUSTOMERAUDV.RPT'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-INFO.
       01  CUST-REC.
           05  CUST-ID-REC                      PIC 9(5).
           05  CUST-FNAME-REC                   PIC X(15).
           05  CUST-LNAME-REC                   PIC X(15).
           05  CUST-INIT-REC                    PIC X.
           05  CUST-PHONE-REC                   PIC 9(10).
           05  CUST-EMAIL-REC                   PIC X(35).
           05  CUST-ADDRS-REC                   PIC X(35).
           05  CUST-CITY-REC                    PIC X(15).
           05  CUST-STATE-REC                   PIC XX.
           05  CUST-ZIP-REC                     PIC 9(5).
       FD  CUSTOMER-INFO.
       01  PRINT-REC                            PIC X(138).
       WORKING-STORAGE SECTION.
       01  TRANS-REC-IN.
           05  TRANS-ID-IN                      PIC 9(5)  VALUE 00001.
           05  TRANS-FNAME-IN                   PIC X(15).
           05  TRANS-LNAME-IN                   PIC X(15).
           05  TRANS-INIT-IN                    PIC X.
           05  TRANS-PHONE-IN                   PIC 9(10).
           05  TRANS-EMAIL-IN                   PIC X(35).
           05  TRANS-ADDRS-IN                   PIC X(35).
           05  TRANS-CITY-IN                    PIC X(15).
           05  TRANS-STATE-IN                   PIC XX.
           05  TRANS-ZIP-IN                     PIC 9(5).
       01  WORK-AREAS.
           05  ARE-THERE-MORE-RECORDS          PIC X(1)
                  VALUE 'Y'.
           05  DATA-OK                         PIC X(1).
           05  WS-DATE.
               10  WS-YEAR                     PIC 9999.
               10  WS-MONTH                    PIC 99.
               10  WS-DAY                      PIC 99.

       01  DETAIL-REC-OUT.
           05  CUST-NO-OUT                     PIC 9(5).
           05  CUST-FNAME-OUT                  PIC X(15).
           05  CUST-LNAME-OUT                  PIC X(15).
           05  CUST-INIT-OUT                   PIC X.
           05  CUST-PHONE-OUT                  PIC 9(10).
           05  CUST-EMAIL-OUT                  PIC X(35).
           05  CUST-ADDR-OUT                   PIC X(35).
           05  CUST-CITY-OUT                   PIC X(15).
           05  CUST-STATE-OUT                  PIC XX.
           05  CUST-ZIP-OUT                    PIC 9(5).

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
       01  SCREEN-1.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2
               BACKGROUND-COLOR 0.
           05  INPUT-PROMPTS.
               10  LINE 8 COLUMN 20       VALUE "FIRST NAME: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "LAST NAME: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "MIDDLE INITIAL: ".
               10  LINE PLUS 2 COLUMN 20      VALUE "PHONE NUMBER: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "EMAIL ADDRESS: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "STREET ADDRESS: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "CITY: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "STATE ABBREVIATION: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "ZIPCODE: ".
           05  INPUT-FIELDS
                   REVERSE-VIDEO
                   AUTO.
               10  LINE 8 COLUMN 39        PIC X(20) TO TRANS-FNAME-IN.
               10  LINE PLUS 2 COLUMN 39   PIC X(20) TO TRANS-LNAME-IN.
               10  LINE PLUS 2 COLUMN 39   PIC X TO TRANS-INIT-IN.
               10  LINE PLUS 2 COLUMN 39   PIC 9(10) TO TRANS-PHONE-IN.
               10  LINE PLUS 2 COLUMN 39   PIC X(35) TO TRANS-EMAIL-IN.
               10  LINE PLUS 2 COLUMN 39   PIC X(35) TO TRANS-ADDRS-IN.
               10  LINE PLUS 2 COLUMN 39   PIC X(15) TO TRANS-CITY-IN.
               10  LINE PLUS 2 COLUMN 39   PIC XX TO TRANS-STATE-IN.
               10  LINE PLUS 2 COLUMN 39   PIC 9(5) TO TRANS-ZIP-IN.
       01  SCREEN-2.
           05  BACKGROUND-COLOR BLACK
               AUTO.
               10  LINE 18 COLUMN 20
                       VALUE "IS CUSTOMER INFOMATION CORRECT"
                       FOREGROUND-COLOR RED
                       HIGHLIGHT.
               10  VALUE " (Y/N)? "
                       FOREGROUND-COLOR WHITE.
               10  PIC X(1) TO DATA-OK
                       FOREGROUND-COLOR RED
                       HIGHLIGHT.
       01  SCREEN-3.
           05  BLANK SCREEN
                   FOREGROUND-COLOR GREEN
                   BACKGROUND-COLOR BLACK.
           05  LINE 10 COLUMN 20
                   BACKGROUND-COLOR BLACK
                   FOREGROUND-COLOR CYAN
                   HIGHLIGHT
                   VALUE "IS THERE MORE DATA".
           05  FOREGROUND-COLOR BLACK
                   BACKGROUND-COLOR WHITE
                   HIGHLIGHT
                   VALUE " (Y/N)? ".
           05  FOREGROUND-COLOR CYAN
                   BACKGROUND-COLOR BLACK
                   AUTO
                   PIC X(1) TO ARE-THERE-MORE-RECORDS.

       PROCEDURE DIVISION.
      ****************************************************
      *     All program logic is controlled by           *
      *          100-MAIN-MODULE                         *
      ****************************************************
       100-MAIN-MODULE.
           OPEN OUTPUT CUSTOMER-INFO
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = "N" OR "n"
               MOVE "N" TO DATA-OK
               PERFORM UNTIL DATA-OK = "Y" OR "y"
                   DISPLAY SCREEN-1
                   ACCEPT SCREEN-1
                   DISPLAY SCREEN-2
                   ACCEPT SCREEN-2
               END-PERFORM
               PERFORM 200-ADD-RTN
               DISPLAY SCREEN-3
               ACCEPT SCREEN-3
           END-PERFORM
           CLOSE    CUSTOMER-INFO
           STOP RUN.

       200-ADD-RTN.
      *    INCREMENT CUST-ID VALUE FROM LAST RECORD IN FILE. GIVE TO
      *    TRANS-ID-IN
           MOVE TRANS-ID-IN TO CUST-NO-OUT
           MOVE TRANS-FNAME-IN TO CUST-FNAME-OUT
           MOVE TRANS-LNAME-IN TO CUST-LNAME-OUT
           MOVE TRANS-INIT-IN TO CUST-INIT-OUT
           MOVE TRANS-PHONE-IN TO CUST-PHONE-OUT
           MOVE TRANS-EMAIL-IN TO CUST-EMAIL-OUT
           MOVE TRANS-ADDRS-IN TO CUST-ADDR-OUT
           MOVE TRANS-CITY-IN TO CUST-CITY-OUT
           MOVE TRANS-STATE-IN TO CUST-STATE-OUT
           MOVE TRANS-ZIP-IN TO CUST-ZIP-OUT
           WRITE PRINT-REC FROM DETAIL-REC-OUT
           ADD 1 TO TRANS-ID-IN.

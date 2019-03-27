       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           CUSUPD.
      *    Programmer: Jonathan Walker
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL CUSTOMER-INFO
           ASSIGN TO 'CUSTOMER.IDX'
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS CUST-ID-REC
               ALTERNATE RECORD KEY IS CUST-LNAME-REC.

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
       01  TRANS-REC-IN.
           05  TRANS-ID-IN                      PIC 9(5)  VALUE 00001.
           05  TRANS-FNAME-IN                   PIC X(15).
           05  TRANS-LNAME-IN                   PIC X(15).
           05  TRANS-PHONE-IN                   PIC 9(10).
           05  TRANS-EMAIL-IN                   PIC X(35).
           05  TRANS-ADDRS-IN                   PIC X(35).
           05  TRANS-CITY-IN                    PIC X(15).
           05  TRANS-STATE-IN                   PIC XX.
           05  TRANS-ZIP-IN                     PIC 9(5).
           05  TRANS-DST-IN                     PIC X.


       01  WORK-AREAS.
           05  MORE-RECS          PIC X(1)
                  VALUE 'Y'.
           05  MSSG-OUT           PIC X(46).
           05  DATA-OK                         PIC X(1).
           05  WS-DATE.
               10  WS-YEAR                     PIC 9999.
               10  WS-MONTH                    PIC 99.
               10  WS-DAY                      PIC 99.

       01  DETAIL-REC-OUT.
           05  CUST-NO-OUT                     PIC 9(5).
           05  CUST-FNAME-OUT                  PIC X(15).
           05  CUST-LNAME-OUT                  PIC X(15).
           05  CUST-PHONE-OUT                  PIC 9(10).
           05  CUST-EMAIL-OUT                  PIC X(35).
           05  CUST-ADDR-OUT                   PIC X(35).
           05  CUST-CITY-OUT                   PIC X(15).
           05  CUST-STATE-OUT                  PIC XX.
           05  CUST-ZIP-OUT                    PIC 9(5).
           05  CUST-DST-OUT                    PIC X.

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
       01  UPD-SCREEN.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2
               BACKGROUND-COLOR 0.
           05  INPUT-PROMPTS.
               10 LINE 5 COLUMN 30        VALUE "Update Customer Info".
               10 LINE 6 COLUMN 30        VALUE "--------------------".
               10 LINE 8 COLUMN 20
                         VALUE "Search for Customer by;".
               10 LINE 12 COLUMN 20       VALUE "Customer ID: ".
               10 LINE 13 COLUMN 20       VALUE "Or".
               10 LINE 14 COLUMN 20       VALUE "Customer First Name: ".
               10 LINE 15 COLUMN 20       VALUE "Customer Last  Name: ".
           05  INPUT-FIELDS.
               10 LINE 12 COLUMN 33       PIC 9(5) TO TRANS-ID-IN.
               10 LINE 14 COLUMN 41       PIC X(15) TO TRANS-FNAME-IN.
               10 LINE 15 COLUMN 41       PIC X(15) TO TRANS-LNAME-IN.

       01  SCREEN-1.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2
               BACKGROUND-COLOR 0.
           05  INPUT-PROMPTS.
               10  LINE 8 COLUMN 20       VALUE "FIRST NAME: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "LAST NAME: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "MIDDLE INITIAL: ".
               10  LINE PLUS 2 COLUMN 20  VALUE "PHONE NUMBER: ".
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
           05  TITLE-BAR
               FOREGROUND-COLOR 1
               BACKGROUND-COLOR 0.
               10  LINE 4 COLUMN 40
                   PIC X(50) FROM MSSG-OUT.
           05  LINE 10 COLUMN 54
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
                   PIC X(1) TO MORE-RECS.

       01  CLEAR-SCREEN.
           05  BLANK SCREEN
               FOREGROUND-COLOR GREEN
               BACKGROUND-COLOR BLACK.

       PROCEDURE DIVISION.
      ****************************************************
      *     All program logic is controlled by           *
      *          100-MAIN-MODULE                         *
      ****************************************************
       100-MAIN-MODULE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           PERFORM UNTIL MORE-RECS = "N" OR "n"
               MOVE "N" TO DATA-OK
               MOVE " " TO MSSG-OUT
               DISPLAY UPD-SCREEN
               ACCEPT  UPD-SCREEN
               IF TRANS-ID-IN > 0
                 PERFORM 300-CID-RTN
               ELSE
                 PERFORM 350-NAME-RTN
               END-IF
               IF DATA-OK = 'F'
                 PERFORM UNTIL DATA-OK = "Y" OR "y"
                   DISPLAY CLEAR-SCREEN
                   DISPLAY SCREEN-1
                   ACCEPT SCREEN-1
                   DISPLAY SCREEN-2
                   ACCEPT SCREEN-2
                 END-PERFORM
                 PERFORM 200-UPDATE-RTN
               END-IF
               DISPLAY SCREEN-3
               ACCEPT SCREEN-3
           END-PERFORM
           STOP RUN.

       200-UPDATE-RTN.
           OPEN EXTEND CUSTOMER-INFO
           MOVE TRANS-ID-IN TO CUST-NO-OUT
           MOVE TRANS-FNAME-IN TO CUST-FNAME-OUT
           MOVE TRANS-LNAME-IN TO CUST-LNAME-OUT
           MOVE TRANS-PHONE-IN TO CUST-PHONE-OUT
           MOVE TRANS-EMAIL-IN TO CUST-EMAIL-OUT
           MOVE TRANS-ADDRS-IN TO CUST-ADDR-OUT
           MOVE TRANS-CITY-IN TO CUST-CITY-OUT
           MOVE TRANS-STATE-IN TO CUST-STATE-OUT
           MOVE TRANS-ZIP-IN TO CUST-ZIP-OUT
           MOVE CUST-DST-REC TO CUST-DST-OUT
           REWRITE CUST-REC.

       300-CID-RTN.
           OPEN INPUT CUSTOMER-INFO
           MOVE ' ' TO DATA-OK
           PERFORM UNTIL DATA-OK = 'F' OR 'N'
             PERFORM 500-READ-RTN
             IF CUST-ID-REC = TRANS-ID-IN
               IF CUST-DST-REC = 'N'
                 PERFORM 400-MOVE-RTN
                 MOVE 'F' TO DATA-OK
               ELSE
                 MOVE 'Customer has been deleted. See Admin for help.'
                      TO MSSG-OUT
               END-IF
             END-IF
           END-PERFORM
           MOVE ' ' TO DATA-OK
           CLOSE CUSTOMER-INFO.

       350-NAME-RTN.
           OPEN INPUT CUSTOMER-INFO
           MOVE ' ' TO DATA-OK
           PERFORM UNTIL DATA-OK = "F" OR "N"
             PERFORM 500-READ-RTN
             EVALUATE TRUE
               WHEN TRANS-FNAME-IN = CUST-FNAME-REC
                 EVALUATE TRUE
                   WHEN TRANS-LNAME-IN = CUST-LNAME-REC
                     IF CUST-DST-REC = 'N'
                       PERFORM 400-MOVE-RTN
                       MOVE "F" TO DATA-OK
                     ELSE
                       MOVE
                        'Customer has been deleted. See Admin for help.'
                        TO MSSG-OUT
                     END-IF
                 END-EVALUATE
             END-EVALUATE
           END-PERFORM
           MOVE ' ' TO DATA-OK
           CLOSE CUSTOMER-INFO.

       400-MOVE-RTN.
           MOVE CUST-ID-REC TO TRANS-ID-IN
           MOVE CUST-FNAME-REC TO TRANS-FNAME-IN
           MOVE CUST-LNAME-REC TO TRANS-LNAME-IN
           MOVE CUST-PHONE-REC TO TRANS-PHONE-IN
           MOVE CUST-EMAIL-REC TO TRANS-EMAIL-IN
           MOVE CUST-ADDRS-REC TO TRANS-ADDRS-IN
           MOVE CUST-CITY-REC TO TRANS-CITY-IN
           MOVE CUST-STATE-REC TO TRANS-STATE-IN
           MOVE CUST-ZIP-REC TO TRANS-ZIP-IN
           MOVE CUST-DST-REC TO TRANS-DST-IN.

       500-READ-RTN.
           READ CUSTOMER-INFO
             AT END MOVE "N" TO DATA-OK
           END-READ.

       END PROGRAM CUSUPD.

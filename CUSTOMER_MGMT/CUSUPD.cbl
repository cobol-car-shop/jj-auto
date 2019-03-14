       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           CUSUPD.
      *    Programmer: Jonathan Walker
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-INFO ASSIGN TO 'C:/Users/Leslie/Documents/Pro
      -                               'gramming/CIS334/CUSINFO.RPT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INDEX-FILE ASSIGN TO 'C:/Users/Leslie/Documents/Progra
      -                      'mming/CIS334/CustIndexFile.DAT'
                             ORGANIZATION IS INDEXED
                             ACCESS IS SEQUENTIAL
                             RECORD KEY IS CUST-ID
                             ALTERNATE KEY IS CUST-LNAME
                             WITH DUPLICATES.

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

       FD  INDEX-FILE.
       01  INDEX-REC.
            05  CUST-ID                          PIC 9(5).
            05  CUST-FNAME                       PIC X(15).
            05  CUST-LNAME                       PIC X(15).
            05  CUST-PHONE                       PIC 9(10).
            05  CUST-EMAIL                       PIC X(35).
            05  CUST-ADDRS                       PIC X(35).
            05  CUST-CITY                        PIC X(15).
            05  CUST-STATE                       PIC XX.
            05  CUST-ZIP                         PIC 9(5).
            05  CUST-DST                         PIC X.

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


       01  WORK-AREAS.
           05  MORE-RECS          PIC X(1)
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
                   PIC X(1) TO MORE-RECS.

       PROCEDURE DIVISION.
      ****************************************************
      *     All program logic is controlled by           *
      *          100-MAIN-MODULE                         *
      ****************************************************
       100-MAIN-MODULE.
           OPEN OUTPUT CUSTOMER-INFO
                       INDEX-FILE
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           PERFORM UNTIL MORE-RECS = "N" OR "n"
               MOVE "N" TO DATA-OK
               PERFORM UNTIL DATA-OK = "Y" OR "y"
                   DISPLAY SCREEN-1
                   ACCEPT SCREEN-1
                   DISPLAY SCREEN-2
                   ACCEPT SCREEN-2
               END-PERFORM
      *    Check to see if info has been changed before committing.
               PERFORM 200-UPDATE-RTN
               DISPLAY SCREEN-3
               ACCEPT SCREEN-3
           END-PERFORM
           CLOSE  CUSTOMER-INFO
                  INDEX-FILE
           STOP RUN.

       200-UPDATE-RTN.
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
           WRITE CUST-REC FROM DETAIL-REC-OUT.

      *    Need to check the output file for duplicate customer info?

      *  250-REC-INDEX-RTN.
      *      WRITE INDEX-REC FROM TRANS-REC-IN
      *            INVALID KEY
      *              MOVE 'DUPLICATE KEY - NOT PROCESSED' TO MESG-OUT
      *            NOT INVALID KEY
      *              MOVE 'SUCCESSFUL PROCESS' TO MESG-OUT
      *      END-WRITE.

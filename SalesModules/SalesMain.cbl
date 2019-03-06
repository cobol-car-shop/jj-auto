      ******************************************************************
      * Author:Jordan Smith
      * Date:3/6/19
      * Purpose:Sales functions and screens for the jj auto store.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
      *need to get/add FD for indexed Car table.
      *file must be indexed by VIN
      *need to find more info on implicit pathing for select statement.
      *need to get/add FD for indexed customer table.
      *File must be indexed by custId alt index of name alt index address

       WORKING-STORAGE SECTION.
       01  MENU-OPTION PIC 9.
       01  VIN         PIC X(17).
       01  LargestCustId PIC 9(5).
       01  WS-Customer-Rec.
           05 WS-CustomerID  PIC 9(5) VALUE 0.
           05 WS-CustomerFirstName PIC X(15).
           05 WS-CustomerLastName  PIC X(15).
           05 WS-CustomerPhone     PIC 9(10).
           05 WS-CustomerAddress   PIC X(30).
           05 WS-CustomerCity      PIC X(15).
           05 WS-CustomerState     PIC A(2).
           05 WS-CustomerZip       PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *Need to have authenticated before going past this point.
      *Authentication might be handled by main menu.
            DISPLAY "Please Select Menu Item /n"
            DISPLAY "1. Search by VIN /n"
            DISPLAY "2. Lookup Customer /n"
            DISPLAY "3. Create New Customer /n"
            ACCEPT MENU-OPTION
               EVALUATE MENU-OPTION
                   WHEN 1
                       PERFORM 200-SearchbyVin
                   WHEN 2
                       PERFORM 210-LookUpCustomer
                   WHEN 3
                       PERFORM 220-CreateNewCustomer
                   WHEN OTHER
                       DISPLAY "Invalid Option, Please enter 1-3."
      *need to design Screen for sales menu
            STOP RUN.
       200-SearchbyVin.
      *prompt for VIN
      *READ VIN into working storage.
      *check it meets criteria for VIN
      *Search indexed file for VIN.
      *Return selected record.
      *display vehicle basic details.
      *Prompt do you wish to return to the main menu Y/N
      *If yes return to sales main menu If no prompt Search new VIN?
      *wait for user input.
       210-LookUpCustomer.
      *prompt for search type name, address, cust ID
      *search indexedfile on desired field.
      *return all records that match
      *display records that match
      *display did you find what you are looking for?
       220-CreateNewCustomer.
      *     OPEN CustomerTable INPUT-OUTPUT
      *Find Largest Customer Id in TABLE
        ADD 1 to LargestCustId giving WS-CustomerID
           Display "Enter customer First name"
           Accept WS-CustomerFirstName
            Display "Enter customer Last name"
           Accept WS-CustomerLastName
           DISPLAY "Enter Customer Phone number"
           Accept WS-CustomerPhone
           DISPLAY "Enter Customer Street Address"
           ACCEPT WS-CustomerAddress
           DISPLAY "Enter Customer State"
           Accept WS-CustomerState
           Display "Enter Customer City"
           ACCEPT WS-CustomerCity
           DISPLAY "Enter Custmoer Zipcode"
           ACCEPT WS-CustomerZip.
      *Write WS-Customer-Rec to Customer-Rec


       END PROGRAM YOUR-PROGRAM-NAME.

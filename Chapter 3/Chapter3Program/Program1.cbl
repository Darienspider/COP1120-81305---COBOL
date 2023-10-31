       identification division.
       program-id. Program1.

       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CUSTOMER-TRANS
       ASSIGN TO 'E:\COP1120-81305-COBOL\Data-In\Ch3_Custtrans.dat'
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT CUSTOMER-MASTER
       ASSIGN TO 'E:\COP1120-81305-COBOL\Data-Out\CustMaster.dat'
       ORGANIZATION IS LINE SEQUENTIAL.

       configuration section.

       data division.
       file section.

       FD CUSTOMER-TRANS RECORDING MODE IS F.
       01 CUST-TRANS.
         05 IDENT-IN PIC X(5).
         05 Sales-In PIC 9(5)V99.

       FD CUSTOMER-MASTER RECORDING MODE IS F.
       01 MASTER-REC.
         05 IDENT-OUT PIC X(5).
         05 Sales-Amt-Out PIC 9(5)V99.
         05 DISCOUNT-PERCENT-OUT PIC 9(2)V99.
         05 Net-Out PIC 9(5)V99.

       working-storage section.
       01 ARE-THERE-MORE-RECORDS PIC X(3) VALUE 'YES'.

       01 WS-DISC-PCT PIC 9(3)V99.
       01 WS-DISCOUNT-AMT PIC 9(7)V99.
       01 WS-Net-Amt PIC 9(7)V99.

       procedure division.
       100-MAIN-MODULE.
           OPEN INPUT CUSTOMER-TRANS
             OUTPUT CUSTOMER-MASTER
           MOVE SPACES TO MASTER-REC
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
               READ CUSTOMER-TRANS
                   AT END
                       MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-PROCESS-DATA
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-TRANS
             CUSTOMER-MASTER
           STOP RUN.

       200-PROCESS-DATA.
           MOVE IDENT-IN TO IDENT-OUT
           MOVE SALES-IN TO Sales-Amt-Out
           IF SALES-IN > 100.00
               MOVE .03 TO DISCOUNT-PERCENT-OUT
           ELSE
               MOVE .02 TO DISCOUNT-PERCENT-OUT
           END-IF
           MULTIPLY SALES-IN BY DISCOUNT-PERCENT-OUT GIVING
             WS-DISCOUNT-AMT
           SUBTRACT WS-DISCOUNT-AMT FROM SALES-IN GIVING NET-OUT
           WRITE MASTER-REC.

       300-CLOSE-RTN.
           Close CUSTOMER-TRANS
             CUSTOMER-MASTER.
       end program Program1
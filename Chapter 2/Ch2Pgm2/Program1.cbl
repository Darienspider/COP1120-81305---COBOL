
       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT TRANS-FILE
       ASSIGN to 'C:\DataFiles\C-IN\Ch2_trans.dat'
       organization is line sequential.
       SELECT SALES-FILE
       ASSIGN to 'C:\DataFiles\C-myout\Ch2i_Sales.rpt'
       organization is line sequential.
       Configuration Section.
       data division.
       FILE SECTION.
       FD TRANS-FILE.
       01 TRANS-REC.
       05 NAME-IN Pic X(20).
       05 Unit-Price-In Pic 9(3)v99.
       05 QTY-Sold-In Pic 9(3).
       FD SALES-FILE.
       01 SALES-REC.
       05 NAME-OUT Pic X(20).
       05 Filler Pic X(05).
       05 Price-Out Pic ZZ9.99.
       05 Filler Pic X(05).
       05 QTY-SOLD-OUT Pic ZZ9.
       05 Filler Pic X(05).
       05 TOT-PRICE-OUT Pic ZZZZZ9.99.
       working-storage section.
       01 MORE-RECORDS Pic X(3) value 'YES'.
       01 CALC-TOT-Price Pic 9(6)v99.
       procedure division.

       100-MAIN-MODULE.
           OPEN INPUT TRANS-FILE
             OUTPUT SALES-FILE.
           PERFORM UNTIL MORE-RECORDS = 'NO '
               READ TRANS-FILE
                   AT END
                       Move 'NO ' to MORE-RECORDS
                   NOT AT END
                       Perform 200-CALC-RTN
               END-READ
           END-PERFORM.
           CLOSE TRANS-FILE
             SALES-FILE.
           goback.

       200-CALC-RTN.
           MOVE NAME-IN TO NAME-OUT.
           Move Unit-Price-In to Price-Out.
           Move QTY-SOLD-IN to QTY-SOLD-OUT.
           Multiply Unit-Price-In by Qty-Sold-In giving CALC-TOT-Price.
           Move CALC-TOT-Price to Tot-Price-Out.
           Write SALES-REC.
       end program Program1.

       environment division.
       Input-Output Section.
       File-Control.
       Select SALES-FILE
       assign to 'C:\Users\Shadarien Williams\Downloads\Ch1_sales.dat'
       organization is line sequential.
       Select PRINT-FILE
       assign to 
       'C:\Users\Shadarien Williams\Downloads\Ch1i_COMM.rpt'
       organization is line sequential.
       configuration section.
       data division.
       File Section.
       FD SALES-FILE.
       01 SALES-REC.
         05 Name-In Pic X(15).
         05 Sale-Amt Pic 999v99.
       FD PRINT-FILE.
       01 PRINT-REC.
         05 Filler Pic X(20) value SPACES.
         05 Name-Out Pic X(15) value SPACES.
         05 Filler Pic X(20) value SPACES.
         05 Comm-Out Pic ZZ9.99.
         05 Filler Pic X(19) value SPACES.
       working-storage section.
       01 More-Recs Pic XXX value 'YES'.
       01 WS-COMM Pic 9(3)v99 value 0.
       01 WS-TOT-COMM Pic 9(5)v99 value 0.
       01 WS-Rec-Cnt Pic 999 value 0.
       01 PRINT-HDR.
         05 Filler Pic X(35) value SPACES.
         05 Filler Pic X(10) value 'COMMISSION'.
         05 Filler Pic X(10) value ' REPORT '.
         05 Filler Pic X(25) value SPACES.
       01 PRINT-TRLR.
         05 Filler Pic X(12) value SPACES.
         05 Filler Pic X(10) value ' Recs In: '.
         05 Rec-Cnt Pic ZZ9 value 0.
         05 Filler Pic X(18) value ' Total Commission'.
         05 Filler Pic X(15) value ' Paid Out = $ '.
         05 Total-Comm Pic ZZZ9.99 value 0.
         05 Filler Pic X(15) value SPACES.
       procedure division.
       100-MAIN-MODULE.
           Open Input SALES-FILE
             Output PRINT-FILE.
           Write PRINT-REC from PRINT-HDR.
           Move SPACES to Print-REC.
           PERFORM until More-Recs = 'NO '
               READ SALES-FILE
                   AT END
                       Move 'NO ' to More-Recs
                   Not At END
                       Perform 200-CALC-COMM
               End-READ
           End-PERFORM.
           PERFORM 300-CLOSE-RTN.
           Close SALES-FILE
             PRINT-FILE.
           goback.
       200-CALC-COMM.
           Initialize PRINT-REC.
           Add WS-Rec-Cnt, 1 giving WS-Rec-Cnt.
           Move Name-In to Name-Out.
           If Sale-Amt > 100.00
               Multiply Sale-Amt by 0.03 giving WS-COMM
           Else
               Multiply Sale-Amt by 0.02 giving WS-COMM
           End-If.
           Move WS-COMM to Comm-Out.
           Write PRINT-REC.
           Add WS-TOT-COMM, WS-COMM giving WS-TOT-COMM.
       300-CLOSE-RTN.
           Move WS-Rec-Cnt to Rec-Cnt.
           Move WS-TOT-COMM to Total-Comm.
           Write PRINT-REC from PRINT-TRLR AFTER 2.
       end program Program1.
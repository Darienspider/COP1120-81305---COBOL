       identification division.
       program-id. Program1.
       Author. Shadarien Williams.

       environment division.

       input-output section.
       file-control.
           select payroll-master
           assign to 'E:\COP1120-81305-COBOL\Data-In\Ch7_payroll.dat'
           organization is line sequential.

           select payroll-list
           assign to
           'E:\COP1120-81305-COBOL\Data-Out\Ch7_payrollOut.dat'
           organization is line sequential.

           
       configuration section.

       data division.
              *> key bits = 80
       file section.
       FD payroll-master.
       01 payroll-rec.
         05 employee-no            PIC X(05).
         05 employee-name          PIC X(25).
         05 FILLER                 PIC X(04).
         05 annual-salary          PIC 9(06).
         05 FILLER                 PIC X(13).
         05 dues                   PIC 9(03)v9(2). *> 2 decimal places
         05 insurance              PIC 9(03)v9(2). *> 2 decimal places
         05 FILLER                 PIC X(17).

       FD payroll-list.
       01 print-rec                PIC X(80).


       working-storage section.

       01 flags-and-indicators.
         05 are-there-more-records PIC xxx value 'yes'.
         05 ws-pp-cnt              PIC 9(03) value zeros.
         05 ws-record-cnt          PIC 9999 value zeros.
         05 ws-line-cnt            PIC 9999 value zeros.

         05 date-field.
           10 year-field           PIC 9(04).
           10 month-field          PIC 9(02).
           10 day-field            PIC 9(02).

       01 header1.
         05 FILLER             PIC X(06) value " PAGE ".
         05 out-pp             PIC ZZ9.
         05 FILLER             PIC X(22) value spaces.
         05 FILLER             PIC X(15) value " Payroll Report ".
         05 FILLER             PIC X(21) value spaces.
         05 date-field-format.
           10 out-mm           PIC Z9.
           10 filler           PIC X(01) value "/".
           10 out-dd           PIC Z9.
           10 filler           PIC X(01) value "/"
           10 out-year         PIC 9(04).
         05 filler             PIC X(03) value spaces.

       01 header2.
         05 FILLER             PIC X(01) value spaces.
         05 filler             PIC X(08) value 'employee'.
         05 filler             PIC X(06) value spaces.
         05 filler             PIC X(04) value 'name'.
         05 filler             PIC X(12) value spaces.
         05 filler             PIC X(03) value 'old'.
         05 filler             PIC X(05) value spaces.
         05 filler             PIC X(03) value 'new'.
         05 filler             PIC X(06) value spaces.
         05 filler             PIC X(03) value 'old'.
         05 filler             PIC X(06) value spaces.
         05 filler             PIC X(03) value 'new'.
         05 filler             PIC X(05) value spaces.
         05 filler             PIC X(03) value 'old'.
         05 filler             PIC X(06) value spaces.
         05 filler             PIC X(03) value 'new'.
         05 filler             PIC X(03) value spaces.

       01 header3.
         05 filler             pic X(04) value spaces.
         05 filler             pic X(03) value 'no'.
         05 filler             pic X(23) value spaces.
         05 filler             pic X(06) value 'salary'.
         05 filler             pic X(02) value spaces.
         05 filler             pic X(06) value 'salary'.
         05 filler             pic X(04) value spaces.
         05 filler             pic X(04) value 'dues'.
         05 filler             pic X(05) value spaces.
         05 filler             pic X(04) value 'dues'.
         05 filler             pic X(03) value spaces.
         05 filler             pic X(06) value 'insur'.
         05 filler             pic X(03) value spaces.
         05 filler             pic X(06) value 'insur'.
         05 filler             pic X(01) value spaces.




       01 blank-line           PIC X(80) value spaces.

       01 data-line.
         05 filler             PIC X(01) value spaces.
         05 employee-no-out    PIC ZZZZ9.
         05 filler             PIC X(02) value spaces.
         05 employee-name-out  PIC X(20).
         05 filler             PIC X(01) value spaces.
         05 old-salary         PIC ZZZ,ZZ9.
         05 filler             PIC X(01) value spaces.
         05 new-ann-sal-out    PIC ZZZ,ZZ9.
         05 filler             PIC X(03) value spaces.
         05 old-dues           PIC ZZ9.99.
         05 filler             PIC X(01) value spaces.
         05 new-dues-out       PIC Z,ZZ9.99.
         05 filler             PIC X(02) value spaces.
         05 old-insurance      PIC ZZ9.99.
         05 filler             PIC X(01) value spaces.
         05 new-insurance-out  PIC Z,ZZ9.99.
         05 filler             PIC X(01) value spaces.


       procedure division.
       100-main-module.
           perform 150-housekeeping-start.

           perform until are-there-more-records = 'no '
               read payroll-master
                   at end
                       move 'no ' to are-there-more-records
                   not at end
                       perform 200-payroll-rtn
               end-read
           end-perform

           perform 350-house-keeping-finish.

           goback.

       150-housekeeping-start.
           open input payroll-master
             output payroll-list

           move function current-date to date-field
           move day-field to out-dd
           move month-field to out-mm
           move year-field to out-year.

           perform 300-write-headers.

       200-payroll-rtn.
           add 1 to ws-record-cnt.
           move spaces to print-rec
           move employee-no to employee-no-out
           move employee-name to employee-name-out
           move annual-salary to old-salary
           compute new-ann-sal-out =
             annual-salary + (annual-salary * 0.07)
           *> employee salary increased by 7%
             

           move dues to old-dues
           compute new-dues-out = dues + (dues * 0.04)
           *> union dues increased by 4%


           move insurance to old-insurance
           compute new-insurance-out =
             insurance + (insurance * .03)
           *> insurance increased by 3%

           if ws-line-cnt < 60
               continue
           else
               perform 300-write-headers
           end-if

           write print-rec from data-line
             AFTER advancing 1 line.
           add 1 to ws-line-cnt.




       300-write-headers.
           add 1 to ws-pp-cnt.
           move ws-pp-cnt to out-pp.

           write print-rec from header1 after advancing page.
           write print-rec from blank-line after advancing 2 lines
           write print-rec from header2 after advancing 1 line.
           write print-rec from header3 after advancing 1 line.
           write print-rec from spaces after advancing 1 line.
           move 6 to ws-line-cnt.

       350-house-keeping-finish.
           close payroll-master payroll-list


       end program Program1.

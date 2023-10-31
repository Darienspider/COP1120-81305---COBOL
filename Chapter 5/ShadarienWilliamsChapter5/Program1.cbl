       identification division.
       program-id. Program1.
       author. Shadarien Williams.

       environment division.
       input-output section.
       file-control.
       *> FACULTY file
       select in-fac-file
       assign to 'E:\COP1120-81305-COBOL\Data-In\CH5PP.dat'
       organization is line sequential.

       *> output file
       select out-report-file
       assign to 'E:\COP1120-81305-COBOL\Data-Out\Ch5-OutPut.txt'
       organization is line sequential.

       configuration section.

       data division.
       file section.

       fd in-fac-file.
       01 FACULTY.
         05 IN-EMPLOYEE-ID pic x(3).
         05 IN-EMP-LAST-NAME pic x(20).
         05 IN-EMP-FIRST-NAME pic x(10).
         05 IN-EMP-RANK pic x(2).
         05 IN-EMP-SALARY pic 9(6)v99.

       fd out-report-file recording mode is f.
       01 OUT-REPORT pic x(80).
       *>05 OUT-TOTAL pic 9(9) value 0.

       working-storage section.
       01 WS-WORK-AREAS.
       *> COUNTERS
         05 ARE-THERE-MORE-RECORDS pic x(3) VALUE 'YES'.
         05 WS-PROFESSOR-COUNTER pic 9(3) VALUE ZEROS.
         05 WS-ASSISTANT-COUNTER pic 9(3) VALUE ZEROS.
         05 WS-INSTRUCTOR-COUNTER pic 9(3) VALUE ZEROS.
         05 WS-ASSOCIATE-COUNTER pic 9(3) VALUE ZEROS.
         05 WS-TOTAL-FAC-COUNT pic 9(3) VALUE ZEROS.

       *> COSTS
         05 WS-PROFESSOR-TOTAL-COST pic 9(8)V99 VALUE ZEROS.
         05 WS-ASSOCIATE-TOTAL-COST pic 9(8)V99 VALUE ZEROS.
         05 WS-INSTRUCTOR-TOTAL-COST pic 9(8)V99 VALUE ZEROS.
         05 WS-ASSISTANT-TOTAL-COST pic 9(8)V99 VALUE ZEROS.
         05 WS-TOTAL-FAC-COST pic 9(8)V99 VALUE ZEROS.
         05 new-sal pic 9(7)V99 value zeros.

       01 HL-HEADER-1.
         05 FILLER pic x(25) VALUE 'University Payroll Report '.

       01 HL-HEADER-2.
         05 pic x(30) VALUE "RANK".
         05 pic x(20) VALUE "Emp. Count ".
         05 pic x(25) VALUE "Cost of Proposed Increase".
         05 pic x(5).

       01 TL-TOTAL-LINE.
         05 TL-RANK pic x(10) VALUE spaces.
         05 pic x(26) VALUE spaces.
         05 TL-NO-OF-EMPLOYEES pic ZZ9.
         05 pic x(17) VALUE spaces.
         05 TL-COST pic $Z,ZZZ,ZZ9.99.
         05 PIC X(11).


       01 TL-FINAL-TOTAL-LINE.
         05                pic x(28) VALUE
            'TOTAL UNIVERSITY BUDGET For: '.
         05 EMPS-AFFECTED  PIC zz9.
         05                PIC x(27) Value 
         ' Emps WILL BE INCREASED BY: '
         05 TL-TOTAL-COST  pic $ZZZ,ZZZ,ZZ9.99.
         05 PIC X(7).

       procedure division.
       100-MAIN-MODULE.
           open input in-fac-file
             output out-report-file
           display "Opened Files "
           perform until are-there-more-records = 'NO '
               read in-fac-file
                   at end
                       move 'NO ' to are-there-more-records
                   not at end
                       perform 200-CALC-RTN
               end-read
           end-perform

           perform 300-FINAL-RTN
           close in-fac-file out-report-file
           stop run.

       200-CALC-RTN.

           if in-emp-rank = 'FP'
             then
               *> calculate increase and add to professor total
               multiply in-emp-salary by .043 giving new-sal
               add new-sal to ws-professor-total-cost
               *> add 1 to professor counter
               add 1 to ws-professor-counter
           end-if

           if in-emp-rank = 'AP'
             then
               *> calculate increase and add to professor total
               multiply in-emp-salary by .052 giving new-sal
               add new-sal to ws-assistant-total-cost
               *> add 1 to professor counter
               add 1 to ws-assistant-counter
           end-if

           if in-emp-rank = 'AS'
             then
               *> calculate increase and add to professor total
               multiply in-emp-salary by .048 giving new-sal
               add new-sal to ws-associate-total-cost
               *> add 1 to professor counter
               add 1 to ws-associate-counter
           end-if

           if in-emp-rank = 'IP'
             then
               *> calculate increase and add to professor total
               multiply in-emp-salary by.057 giving new-sal
               add new-sal to ws-instructor-total-cost
               *> add 1 to professor counter
               add 1 to ws-instructor-counter
           end-if.

       300-FINAL-RTN.

           write out-report from HL-HEADER-1
           write out-report from HL-HEADER-2

           MOVE 'FULL' TO TL-RANK
           MOVE WS-PROFESSOR-COUNTER TO TL-NO-OF-EMPLOYEES
           MOVE WS-PROFESSOR-TOTAL-COST TO TL-COST
           WRITE OUT-REPORT FROM TL-TOTAL-LINE AFTER ADVANCING 1 LINES

           MOVE 'ASSOCIATE' TO TL-RANK
           MOVE WS-ASSOCIATE-COUNTER TO TL-NO-OF-EMPLOYEES
           MOVE WS-ASSOCIATE-TOTAL-COST TO TL-COST
           WRITE OUT-REPORT FROM TL-TOTAL-LINE AFTER ADVANCING 1 LINES

           MOVE 'ASSISTANT' TO TL-RANK
           MOVE WS-ASSISTANT-COUNTER TO TL-NO-OF-EMPLOYEES
           MOVE WS-ASSISTANT-TOTAL-COST TO TL-COST
           WRITE OUT-REPORT FROM TL-TOTAL-LINE AFTER ADVANCING 1 LINES

           MOVE 'INSTRUCTOR' TO TL-RANK
           MOVE WS-INSTRUCTOR-COUNTER TO TL-NO-OF-EMPLOYEES
           MOVE WS-INSTRUCTOR-TOTAL-COST TO TL-COST
           WRITE OUT-REPORT FROM TL-TOTAL-LINE AFTER ADVANCING 1 LINES

           ADD WS-PROFESSOR-TOTAL-COST, WS-ASSISTANT-TOTAL-COST,
             WS-ASSOCIATE-TOTAL-COST, WS-INSTRUCTOR-TOTAL-COST GIVING
             WS-TOTAL-FAC-COST

           ADD WS-PROFESSOR-COUNTER, WS-ASSISTANT-COUNTER,
             WS-ASSOCIATE-COUNTER, WS-INSTRUCTOR-COUNTER GIVING
             WS-TOTAL-FAC-COUNT

           move WS-TOTAL-FAC-COUNT to EMPS-AFFECTED
           MOVE WS-TOTAL-FAC-COST TO TL-total-COST
           WRITE OUT-REPORT FROM TL-FINAL-TOTAL-LINE

           goback.

       end program Program1.

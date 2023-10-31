       identification division.
       program-id. Program1.

       environment division.
       FILE-CONTROL.
           SELECT IN-EMP-FILE assign to
           'E:\COP1120-81305-COBOL\Data-In\Ch10_payroll.dat'
           organization is line sequential.

           SELECT OUT-SALARY-RPT assign to
           'E:\COP1120-81305-COBOL\Data-OUT\Ch10_payroll.rpt'
           organization is line sequential.


       configuration section.

       data division.
       FILE SECTION. 
       FD IN-EMP-FILE.
       01 IN-EMP-REC.
         05 IN-EMPNO               PIC 9(5).
         05 IN-EMPNAME             PIC X(20).
         05 IN-TERR-NO             PIC 9(02).
         05 FILLER                 PIC 9(06).
         05 IN-ANNSAL              PIC 9(06).
         05 FILLER                 PIC X(45).

       FD OUT-SALARY-RPT.
       01 SAL-OUT-REC              PIC X(80) VALUE spaces.


       working-storage section.
       01 FLAGS-AND-INDICATORS.
         05 ARE-THERE-MORE-RECORDS PIC XXX VALUES 'YES'.
         05 FIRST-RECORD           PIC XXX VALUES 'YES'.
         05 DATE-FIELD.
           10 YEAR-FIELD           PIC 9(4).
           10 MONTH-FIELD          PIC 9(2).
           10 DAY-FIELD            PIC 9(2).
         05 WS-TERR-TOT            PIC 9(9) VALUE ZEROS.
         05 WS-COMP-TOT            PIC 9(9) VALUE ZEROS.
         05 WS-TERR-HOLD           PIC x(2).

       01 ws-title-rec.
         05                        pic x(16) value spaces.
         05                        pic x(27) value 
         'TOTAL SALARIES BY TERRITORY'.
         05  FILLER                pic x(16) value spaces.
         05 DATE-FIELD-FORMAT.
           10 OUT-MM               PIC Z9.
           10 FILLER               PIC X(1) VALUE '/'.
           10 OUT-DD               PIC Z9.
           10 FILLER               PIC X(1) VALUE '/'.
           10 OUT-CCYY             PIC 9(4).
         05 FILLER                 PIC X(10) VALUE spaces.

       01 HEADING2.
         05                        PIC X(10) VALUE SPACES.
         05                        PIC X(9) VALUE 'Territory'.
         05                        PIC X(10) VALUE SPACES.
         05                        PIC X(14) VALUE 'Total Salaries'.
         05                        PIC X(37) VALUE SPACES.

       01 ws-blank-line        pic x(80) value spaces.

       01 det-terr-total-rec.
         05                    PIC X(14) VALUE SPACES.
         05  territory-out     PIC X(02) VALUE SPACES.
         05  filler            PIC X(12) VALUE SPACES.
         05 total-salary       PIC $ZZZ,ZZ,ZZ9.99.
         05                    PIC X(38) VALUE SPACES.

       01 GRtotal-line.
         05                    PIC X(3) VALUE SPACES.
         05                    PIC X(25) VALUE SPACES.
         05                    PIC x(17) value '_________________'.
         05                    PIC X(35) VALUE SPACES.


       01 GRtotal-comp.
         05                    PIC X(3) VALUE SPACES.
         05                    PIC X(25) VALUE 'Total company salaries'
         .
         05  total-comp-sal    PIC $ZZZ,ZZZ,ZZ9.99.
         05                    PIC X(37) VALUE SPACES.





       procedure division.
       100-main-module.
           perform 150-housekeeping-start.
           perform until ARE-THERE-MORE-RECORDS = 'No '
               read IN-EMP-FILE
                   at end
                       move 'No ' to ARE-THERE-MORE-RECORDS
                       perform 250-control-break
                   not at end
                       perform 200-calc-rtn
           end-perform
           perform 350-housekeeping-finish.
           goback.

       150-housekeeping-start.
           open input IN-EMP-FILE output OUT-SALARY-RPT.
           move function current-date to DATE-FIELD
           move DAY-FIELD to OUT-DD
           move MONTH-FIELD to out-mm
           move YEAR-FIELD to OUT-CCYY.

           write sal-out-rec from ws-title-rec after advancing 2 lines.
           write SAL-OUT-REC from ws-blank-line after advancing 2 lines.
           write sal-out-rec from heading2 after advancing 2 lines.

       200-calc-rtn.
           evaluate true
               when FIRST-RECORD = "YES"
                   move in-terr-no to WS-TERR-HOLD
                   move 'no ' to FIRST-RECORD
               when IN-TERR-NO not = WS-TERR-HOLD
                   perform 250-control-break
           end-evaluate.

           compute WS-TERR-TOT = WS-TERR-TOT + IN-ANNSAL.

       250-control-break.
           move WS-TERR-HOLD to territory-out.
           move WS-TERR-TOT to total-salary.
           write sal-out-rec from det-terr-total-rec after advancing 2
             lines.
           compute WS-COMP-TOT = WS-COMP-TOT + WS-TERR-TOT.
           move IN-TERR-NO to WS-TERR-HOLD.
           move 0 to WS-TERR-TOT.

       350-housekeeping-finish.
           move WS-COMP-TOT to total-comp-sal
           write SAL-OUT-REC from GRtotal-line after advancing 2 lines.
           write SAL-OUT-REC from GRtotal-comp after advancing 2 lines.
           write SAL-OUT-REC from ws-blank-line after advancing 2 lines.
           move 0 to WS-TERR-TOT.

           close IN-EMP-FILE OUT-SALARY-RPT


       end program Program1.

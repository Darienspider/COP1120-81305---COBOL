       identification division.
       program-id. Program1.
      *>> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       environment division.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       *> payroll file
       SELECT IN-EMPLOYEE-FILE
       ASSIGN TO 'E:\COP1120-81305-COBOL\Data-In\Ch4_payroll.dat'
       organization IS LINE sequential.

       *> output file
       SELECT OUT-SALARY-FILE
       ASSIGN TO 
       'E:\COP1120-81305-COBOL\Data-Out\Ch4_Salary_File_Out.dat'
       organization IS LINE sequential.

       configuration section.

       data division.
       file section.

       FD IN-EMPLOYEE-FILE RECORDING MODE IS F.
       01 EMPLOYEE.
         05 IN-EMPLOYEE-ID PIC X(5).
         05 IN-EMPLOYEE-NAME        PIC X(20).
         05 IN-LOC-CODE.
           10 IN-TERR-NO            PIC 99.
           10 IN-OFF-NO PIC 99.
         05 IN-ANNSAL PIC 9(6).
         05 IN-SOCSEC-NO PIC 9(9).
         05 IN-NUM-DEPEND PIC 99.
         05 IN-JOB-CLASS PIC 99.
         05 FILLER PIC X(32).


       FD OUT-SALARY-FILE RECORDING MODE IS F.
       01 SALARY
         05 OUT-EMPLOYEE-ID          PIC X(5).
         05 FILLER                   PIC X(02) VALUE SPACES.

         05 OUT-EMPLOYEE-NAME        PIC X(18).
         05 FILLER                   PIC X(01) VALUE SPACE.

         05 OUT-TERR-NO PIC XX.
         05 FILLER PIC X(05) VALUE SPACES.

         05 OUT-OFFICE-NO PIC XX.
         05 FILLER PIC X(4) VALUE SPACES.

         05 OUT-SALARY PIC ZZ,ZZZ9.
         05 FILLER PIC X(7) VALUE SPACES.

         05 OUT-SOCSEC-NO PIC X(9).
         05 FILLER PIC X(4) VALUE SPACES.

         05 OUT-NUM-DEP PIC Z9.
         05 FILLER PIC X(4) VALUE SPACES.

         05 OUT-JOB-CLASS PIC Z9.
         05 FILLER PIC X(04) VALUE SPACE.
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

       working-storage section.
       01 WS-WORK-AREAS.
         05 ARE-THERE-MORE-RECORDS   PIC X(3) VALUE 'YES'.

       01 WS-TITLE-REC.
         05 FILLER PIC X(29) VALUE SPACES.
         05 FILLER PIC X(08) VALUE 'PAYROLL '.
         05 FILLER PIC X(06) VALUE 'REPORT'.


       01 WS-HDR-REC.
         05 FILLER PIC X(7) VALUE 'EMP# '.
         05 FILLER PIC X(15) VALUE 'EMPLYEE NAME'.
         05 FILLER PIC X(08) VALUE '   TERR  '.
         05 FILLER PIC X(7) VALUE '  OFF# '.
         05 FILLER PIC X(13) VALUE ' ANN SALARY'.
         05 FILLER PIC X(12) VALUE '    SOCIAL   '.
         05 FILLER PIC X(10) VALUE '   DEPT# '.
         05 FILLER PIC X(15) VALUE 'Job'.
         05 FILLER PIC X(6) VALUE spaces.




       procedure division.
       100-MAIN-MODULE.
           OPEN INPUT IN-EMPLOYEE-FILE
             OUTPUT OUT-SALARY-FILE
           WRITE salary from WS-TITLE-REC.
           WRITE salary from WS-HDR-REC.

           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
                   READ IN-EMPLOYEE-FILE 
                       AT END MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                       NOT AT END PERFORM 200-PROCESS-DATA
                   END-READ
           END-PERFORM
           *>> CLOSE BOTH FILES
           CLOSE IN-EMPLOYEE-FILE OUT-SALARY-FILE
           DISPLAY 'END OF JOB'
           STOP RUN.
       200-PROCESS-DATA.
           MOVE spaces to SALARY.
           MOVE IN-EMPLOYEE-ID to OUT-EMPLOYEE-ID
           MOVE IN-EMPLOYEE-NAME to OUT-EMPLOYEE-NAME
           MOVE IN-TERR-NO to OUT-TERR-NO
           MOVE IN-OFF-NO to OUT-OFFICE-NO
           MOVE IN-ANNSAL to OUT-SALARY
           MOVE IN-SOCSEC-NO to OUT-SOCSEC-NO
           MOVE IN-NUM-DEPEND to OUT-NUM-DEP
           MOVE IN-JOB-CLASS to out-job-class
           WRITE salary

       end program Program1.

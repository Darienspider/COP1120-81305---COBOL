       program-id. Program1 as "ConsoleApplication1.Program1".
       Author. Shadarien Williams.
      *>> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       environment division.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       *> payroll file
       SELECT IN-SALARY-FILE
       ASSIGN TO 'E:\COP1120-81305-COBOL\Data-In\Ch6_402mod.dat'
       organization IS LINE sequential.

       *> output file
       SELECT OUT-SALARY-FILE
       ASSIGN TO
       'E:\COP1120-81305-COBOL\Data-Out\Ch6_Salary_File_Out.dat'
       organization IS LINE sequential.

       configuration section.

       data division.
       FILE SECTION.

       FD IN-SALARY-FILE RECORDING MODE IS F.
       01 PYROL-MSTR.
         05 IN-EMPLOYEE-NUMBER         PIC 9(5)  USAGE IS DISPLAY.
         05 IN-EMPLOYEE-NAME           PIC X(20).
         05 LOC-CODE.
           10 TERR                     PIC X(2).
           10 OFFICE-NUMBER            PIC X(2).
         05 ANN-SALARY                 PIC 9(6).
         05 SSN                        PIC X(9).
         05 FILLER                     PIC X(36).

       FD OUT-SALARY-FILE RECORDING MODE IS F.
       01 RPT-REC.
         05 FILLER                     PIC X(4).
         05 RPT-EMP-NO                 PIC ZZZZ9.
         05 FILLER                     PIC X(2).
         05 RPT-EMP-NAME               PIC X(20).
         05 FILLER                     PIC X(1).
         05 RPT-TERR                   PIC Z9.
         05 FILLER                     PIC X(2).
         05 RPT-OFFICE-NUMBER          PIC Z9.
         05 FILLER                     PIC X(3).
         05 RPT-ANN-SALARY             PIC $$$$,$$9.
         05 FILLER                     PIC X(3).
         05 RPT-SSN                    PIC x(11).
         05 FILLER                     PIC X(17).

       working-storage section.
       01 ASSORTED-FLAGS.
         05 MORE-RECORDS               PIC X(3) VALUE 'YES'.
         05 WS-PAGE-NUMBER             PIC 9(3) VALUE ZERO.
         05 EMPLOYEE-COUNT             PIC 9(03) VALUE ZERO.
         05 LINE-COUNT                 PIC 9(02) VALUE ZERO.
         05 WS-DATE.
           10 WS-YY                    PIC 9(2).
           10 WS-MM                    PIC 9(2).
           10 WS-DD                    PIC 9(2).

       01 PRINT-TITLE1.
         05 FILLER                     PIC X(20) VALUE SPACES.
         05                            PIC X(08) VALUE 'PAYROLL'.
         05 FILLER                     PIC X(08) VALUE 'LISTING'.
         05 FILLER                     PIC X(8) VALUE SPACES.
         05 FILLER                     PIC X(5) VALUE 'PAGE:'.
         05 RPT-PGNO                   PIC ZZ9.
         05 FILLER                     PIC X(03) VALUE SPACES.
         05 RPT-DATE.
           10 RPT-MO                   PIC 9(02).
           10 FILLER                   PIC X VALUE '/'.
           10 RPT-DY                   PIC 9(2).
           10 FILLER                   PIC X VALUE '/'.
           10 RPT-YR                   PIC 9(4).
         05 FILLER                     PIC X(15) VALUE SPACES.

       01 PRINT-BRK1.
         05 FILLER                     PIC X(80) VALUE SPACES.

       01 PRINT-TOT1.
         05 FILLER                     PIC X(10) VALUE SPACES.
         05 FILLER                     PIC X(10) VALUE 'TOTAL # EM'.
         05 FILLER                     PIC X(10) VALUE 'PLOYEES LI'.
         05 FILLER                     PIC X(7) VALUE 'STED: '.
         05 TOTCNT                     PIC ZZ9.
         05 FILLER                     PIC X(40).

       01 PRINT-HDR1.
         05 FILLER                     PIC X(03) VALUE SPACES.
         05 FILLER                     PIC X(06) VALUE 'EMP NO'.
         05 FILLER                     PIC X(02) VALUE SPACES.
         05 FILLER                     PIC X(13) VALUE 'EMPLOYEE NAME.
         05 FILLER                     PIC X(07) VALUE SPACES.
         05 FILLER                     PIC X(08) VALUE 'TERR/OFF'.
         05 FILLER                     PIC X(04) VALUE SPACES.
         05 FILLER                     PIC X(07) VALUE 'SALARY'.
         05 FILLER                     PIC X(2) VALUE SPACES
         05 FILLER                     PIC X(11) VALUE 'SOC SEC NUM'.
         05 FILLER         PIC X(17) VALUE SPACES.


       procedure division.

       100-MAIN-MODULE.
           PERFORM 110-STARTUP-MODULE.
           PERFORM UNTIL MORE-RECORDS ='NO '
               READ IN-SALARY-FILE
                   AT END
                       MOVE 'NO ' TO MORE-RECORDS
                   NOT AT END
                       PERFORM 200-LOOP-RTN
               END-READ
           END-PERFORM
           PERFORM 300-CLOSE-MODULE
           goback.

       110-STARTUP-MODULE.
           OPEN INPUT IN-SALARY-FILE OUTPUT OUT-SALARY-FILE.
           MOVE 1 TO WS-PAGE-NUMBER
           ACCEPT WS-DATE FROM DATE
           MOVE WS-MM TO RPT-MO
           MOVE WS-DD TO RPT-DY
           ADD WS-YY 2000 GIVING RPT-YR
           MOVE WS-PAGE-NUMBER TO RPT-PGNO

           WRITE RPT-REC FROM PRINT-TITLE1.
           WRITE RPT-REC FROM PRINT-BRK1.
           WRITE RPT-REC FROM PRINT-HDR1.
           MOVE 3 TO LINE-COUNT.

       200-LOOP-RTN.
           IF LINE-COUNT < 60
               CONTINUE
           ELSE
               PERFORM 210-PAGE-BREAK-MODULE
           END-IF

           MOVE SPACES TO RPT-REC.
           MOVE IN-EMPLOYEE-NUMBER TO RPT-EMP-NO
           MOVE IN-EMPLOYEE-NAME TO RPT-EMP-NAME
           MOVE TERR TO RPT-TERR
           MOVE OFFICE-NUMBER TO RPT-OFFICE-NUMBER
           MOVE ANN-SALARY TO RPT-ANN-SALARY
           STRING SSN(1:3) '-' SSN(4:2) '-' SSN(6:4) INTO RPT-SSN
           ADD 1 TO EMPLOYEE-COUNT
           WRITE RPT-REC.

       210-PAGE-BREAK-MODULE.
           ADD 1 TO WS-PAGE-NUMBER.
           MOVE WS-PAGE-NUMBER TO RPT-PGNO.
           WRITE RPT-REC FROM PRINT-TITLE1 AFTER page.
           WRITE RPT-REC FROM PRINT-BRK1.
           WRITE RPT-REC FROM PRINT-BRK1.
           WRITE RPT-REC FROM PRINT-HDR1.
           MOVE 3 TO LINE-COUNT.

       300-CLOSE-MODULE.
           MOVE EMPLOYEE-COUNT TO TOTCNT.
           WRITE RPT-REC FROM PRINT-BRK1.
           WRITE RPT-REC FROM PRINT-TOT1.
           CLOSE IN-SALARY-FILE OUT-SALARY-FILE.

       end program Program1.

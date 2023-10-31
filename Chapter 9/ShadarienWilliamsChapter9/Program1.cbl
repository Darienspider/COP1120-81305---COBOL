       identification division.
       program-id. Program1.

       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ITEM-FILE ASSIGN TO
           'E:\COP1120-81305-COBOL\Data-In\Ch0903.dat' organization is
           line sequential.

           SELECT INFLATION-RPT ASSIGN TO
             'E:\COP1120-81305-COBOL\Data-OUT\Ch0903output.RPT' 
             organization
             is
             line sequential.

       configuration section.

       data division.

       FD ITEM-FILE RECORD CONTAINS 34 CHARACTERS.
       01 ITEM-REC.
         05 ITEM-NUMBER                            PIC X(5).
         05 ITEM-DESC                              PIC X(20).
         05 ITEM-COST                              PIC 9(3)V99.

       FD INFLATION-RPT.
       01 INFLATION-REC                            PIC X(80).




       working-storage section.
       01 FLAGS-AND-INDICATORS.
           05 ARE-THERE-MORE-RECORDS               PIC XXX VALUE 'YES'.
           05 DATE-FIELD.
               10 YEAR-FIELD                       PIC 9(4).
               10 MONTH-FIELD                      PIC 9(2).
               10 DAY-FIELD                        PIC 9(2).
           05 COST-CALC                            PIC 9(6)V9(2).
           05 PAGENUM                              PIC 9(03) VALUE ZERO.
         05 YR-INDEX                               PIC 99 VALUE ZERO. 

       01 BLANK-LINE           PIC X(80) VALUE SPACES.

       01 HEADER-01.
         05                                        PIC X(19) VALUE 
         SPACES.
         05                                        PIC X(17) VALUE 
         'INFLATION REPORT'.
         05                                        PIC X(12) VALUE 
         SPACES.
         05 DATE-TODAY.
           10 OUT-MONTH                            PIC Z9.
           10                                      PIC X VALUE '/'.
           10 OUT-DAY                              PIC Z9.
           10                                      PIC X VALUE '/'.
           10 OUT-YEAR                             PIC 9(4).
         05                                        PIC X(4) VALUE 
         SPACES.
         05                                        PIC X(10) VALUE 
         SPACES.
         05                                        PIC X(5) VALUE 
         'page'.
         05  PAGE-NO                               PIC ZZ9 VALUE ZERO.

       01 HEADER-02.
           05                                      PIC X(4) VALUE 
           SPACES.
           05                                      PIC X(13) VALUE 
           'ITEM NUMBER: '.
           05                                      PIC X(05) VALUE 
           SPACES.
           05 ITEM-NO                              PIC 9(5).
           05                                      PIC X(53) VALUE 
           SPACES.

       01 HEADER-03.
         05                                        PIC X(4) VALUE 
         SPACES.
         05                                        PIC X(18) VALUE 
         'ITEM DESCRIPTION: '.
         05 IN-DESC                                PIC X(20) VALUE 
         SPACES.
         05                                        PIC X(38) VALUE 
         SPACES.

       01 HEADER-04.
         05                    PIC X(4) VALUE SPACES.
         05                    PIC X(13) VALUE 'ITEM COST: '.
         05                    PIC X(05) VALUE SPACES.
         05 IN-COST            PIC $ZZ,ZZ9.99.
         05                    PIC X(48) VALUE SPACES.

       01 HEADER-05.
         05                    PIC X(25) VALUE SPACES.
         05                    PIC X(4) VALUE 'YEAR'.
         05                    PIC X(5) VALUE SPACES.
         05                    PIC X(14) VALUE 'INFLATION RATE'.
         05                    PIC X(4) VALUE SPACES.
         05                    PIC X(22) VALUE 'ITEM COST W/ INFLATION'.
         05                    PIC X(6) VALUE SPACES.

       01 DATA-01.
         05                    PIC X(26) VALUE SPACES.
         05  YEAR-CALC         PIC Z9.
         05                    PIC X(12) VALUE SPACES.
         05  INFLATION-CALC    PIC Z9. 
         05                    PIC X VALUE '%'.
         05                    PIC X(14) VALUE SPACES.
         05 COST-FIELD-CALC    PIC $ZZZ,ZZ9.99.
         05                    PIC X(12) VALUE SPACES.
                









       procedure division.

       100-main-module.
           PERFORM 150-HOUSEKEEPING-START.
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
               READ ITEM-FILE
                   AT END
                       MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-CALC-RPT-RTN
               END-READ
           END-PERFORM.

           PERFORM 100-HOUSEKEEPING-FINISH.
           goback.

       150-HOUSEKEEPING-START.
           OPEN INPUT ITEM-FILE OUTPUT INFLATION-RPT.
           MOVE FUNCTION current-date TO DATE-FIELD.
           MOVE DAY-FIELD TO OUT-DAY
           MOVE MONTH-FIELD TO OUT-MONTH
           MOVE YEAR-FIELD TO OUT-YEAR.

       200-CALC-RPT-RTN.
           ADD 1 TO PAGENUM
           MOVE PAGENUM TO PAGE-NO
           MOVE ITEM-NUMBER TO ITEM-NO
           MOVE ITEM-DESC TO IN-DESC
           MOVE ITEM-COST TO IN-COST, COST-CALC.

           PERFORM 400-HEADING-RTN

           PERFORM VARYING YR-INDEX FROM 1 BY 1
             UNTIL YR-INDEX > 10
               IF YR-INDEX <= 5
                   COMPUTE COST-CALC ROUNDED = COST-CALC * 1.08
                   MOVE 8 TO INFLATION-CALC
               ELSE
                   COMPUTE COST-CALC ROUNDED = COST-CALC * 1.06
                   MOVE 6 TO INFLATION-CALC
               END-IF
               MOVE YR-INDEX TO YEAR-CALC
               MOVE COST-CALC TO COST-FIELD-CALC
               WRITE INFLATION-REC FROM DATA-01 AFTER ADVANCING 1
           END-PERFORM.
           WRITE INFLATION-REC FROM BLANK-LINE AFTER advancing 1.

       400-HEADING-RTN.
           WRITE INFLATION-REC FROM BLANK-LINE AFTER advancing page.
           WRITE INFLATION-REC FROM HEADER-01 AFTER advancing 1.
           WRITE INFLATION-REC FROM BLANK-LINE AFTER advancing 1.
           WRITE INFLATION-REC FROM HEADER-02 AFTER advancing 1.
           WRITE INFLATION-REC FROM HEADER-03 AFTER advancing 1.
           WRITE INFLATION-REC FROM HEADER-03 AFTER advancing 1.
           WRITE INFLATION-REC FROM HEADER-04 AFTER advancing 1.
           WRITE INFLATION-REC FROM BLANK-LINE AFTER advancing 1.
           WRITE INFLATION-REC FROM HEADER-05 AFTER advancing 1.




       100-HOUSEKEEPING-FINISH.

           CLOSE ITEM-FILE INFLATION-RPT

       end program Program1.

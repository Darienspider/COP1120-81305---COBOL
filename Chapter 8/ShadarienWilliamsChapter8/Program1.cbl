       identification division.
       program-id. Program1.

       environment division.
       file-control.
           select Student-File
           ASSIGN to "E:\COP1120-81305-COBOL\Data-In\Ch8_0808.dat"
           organization is line sequential.

           SELECT Student-Report
           ASSIGN to "E:\COP1120-81305-COBOL\Data-Out\Ch8_0808.rpt"
           organization is line sequential.

       configuration section.

       data division.
       file section.
       FD Student-file.
       01 STUDENT-REC.
         05 SOC-SEC-NO         PIC X(9).
         05 STUDENT-NAME       PIC X(21).
         05 CLASS-CODE         PIC X(1).
           88 FRESHMEN         VALUE '1'.
           88 SOPHMORE         VALUE '2'.
           88 JUNIOR           VALUE '3'.
           88 SENIOR           VALUE '4'.
         05 SCHOOL-CODE        PIC X(1).
           88 BUSINESS         VALUE '1'.
           88 LIBERAL-ARTS     VALUE '2'.
           88 ENGINEERING      VALUE '3'.
         05 GPA                PIC 9v99.
         05 CREDITS            PIC 9(3).

       FD Student-Report.
       01 Rpt-Rec              PIC X(80).

       working-storage section.
       01 Flags-and-Indicators.
         05 are-there-more-records Pic x(3) value 'Yes'.
         05 date-field.
           10 year-field       PIC 9(04).
           10 month-field      PIC 9(02).
           10 day_field        PIC 9(02).
         05 REC-COUNT          PIC 9(04) VALUE ZERO.
         05 hicred-total       PIC 9(04) VALUE ZERO.
         05 credits-total      PIC 9(04) VALUE ZERO.
         05 gpa-2-total        PIC 9(04) VALUE ZERO.
         05 gpa-3-total        PIC 9(04) VALUE ZERO.
         05 gpa-4-total        PIC 9(04) VALUE ZERO.
         05 busi-total         PIC 9(04) VALUE ZERO.
         05 arts-total         PIC 9(04) VALUE ZERO.
         05 engg-total         PIC 9(04) VALUE ZERO.
         05 ccode-1-total      PIC 9(04) VALUE ZERO.
         05 ccode-2-total      PIC 9(04) VALUE ZERO.
         05 ccode-3-total      PIC 9(04) VALUE ZERO.
         05 ccode-4-total      PIC 9(04) VALUE ZERO.
         05 percent-calc       PIC 9(4)v9(6).

       01 BLANK-LINE           PIC x(80) VALUE spaces.

       01 heading-1.
         05 filler             pic x(02) value spaces.
         05 date-field-format.
           10 OUT-MM           PIC 9(02).
           10 FILLER           PIC x(01) value '/'.
           10 OUT-DD           PIC Z9.
           10 FILLER           PIC x(01) value '/'.
           10 OUT-CCYY         PIC 9(04).
         05 FILLER             PIC X(53) value
               '       PASS-Em STATE College Student Summary Report   '.
         05 Filler             PIC x(09) value '   #stud  '.
         05 OUT-CNT            PIC zzz9.
         05 Filler             PIC x(02) VALUE SPACES.

       01 heading-2.
         05                    PIC x(04) Value SPACES.
         05                    PIC x(43) Value
                        'Percentage of students with gpa <2.0         '.
         05                    PIC x(12) value spaces.
         05 percent-2          PIC zz9.99.
         05                    PIC x(01) value '%'.
         05                    pic x(14) value spaces.

       01 heading-3.
         05                    PIC x(04) Value SPACES.
         05                    PIC x(43) Value
                     'Percentage of students with gpa 2.0 - 3.0       '.
         05                    PIC x(12) value spaces.
         05 percent-3          PIC zz9.99.
         05                    PIC x(01) value '%'.
         05                    pic x(14) value spaces.

       01 heading-4.
         05                    PIC x(04) Value SPACES.
         05                    PIC x(43) Value
                      'Percentage of students with gpa > 3.0          '.
         05                    PIC x(12) value spaces.
         05 percent-4          PIC zz9.99.
         05                    PIC x(01) value '%'.
         05                    pic x(14) value spaces.

       01 heading-5.
         05                    PIC x(04) Value SPACES.
         05                    PIC x(43) Value
                  'Percentage w/ gpa > 3.0 AND Business Majors        '.
         05                    PIC x(12) value spaces.
         05 percent-5          PIC zz9.99.
         05                    PIC x(01) value '%'.
         05                    pic x(14) value spaces.

       01 heading-6.
         05                    PIC x(04) Value SPACES.
         05                    PIC x(43) Value
                         'Percentage w/ gpa > 3.0 AND Liberal Arts    '.
         05                    PIC x(12) value spaces.
         05 percent-6          PIC zz9.99.
         05                    PIC x(01) value '%'.
         05                    pic x(14) value spaces.

       01 heading-7.
         05                    PIC x(04) Value SPACES.
         05                    PIC x(43) Value
                      'Percentage w/ gpa > 3.0 AND Engineer Majors    '.
         05                    PIC x(12) value spaces.
         05 percent-7          PIC zz9.99.
         05                    PIC x(01) value '%'.
         05                    pic x(14) value spaces.

       01 heading-8.
         05                    PIC x(04) Value SPACES.
         05                    PIC x(43) Value
                          'Percentage w/ > 100 Credits & gpa < 2.0    '.
         05                    PIC x(12) value spaces.
         05 percent-8          PIC zz9.99.
         05                    PIC x(01) value '%'.
         05                    pic x(14) value spaces.

       01 heading-9.
         05                    PIC x(04) Value SPACES.
         05                    PIC x(43) Value
                             'Percentage w/ gpa > 3.0 and Freshmen    '.
         05                    PIC x(12) value spaces.
         05 percent-9          PIC zz9.99.
         05                    PIC x(01) value '%'.
         05                    pic x(14) value spaces.

       01 heading-10.
         05                    PIC x(04) Value SPACES.
         05                    PIC x(43) Value
                              'Percentage w/ gpa > 3.0 and Sophomores '.
         05                    PIC x(12) value spaces.
         05 percent-10         PIC zz9.99.
         05                    PIC x(01) value '%'.
         05                    pic x(14) value spaces.

       01 heading-11.
         05                    PIC x(04) Value SPACES.
         05                    PIC x(43) Value
                               'Percentage w/ gpa > 3.0 and Juniors '.
         05                    PIC x(12) value spaces.
         05 percent-11         PIC zz9.99.
         05                    PIC x(01) value '%'.
         05                    pic x(14) value spaces.

       01 heading-12.
         05                    PIC x(04) Value SPACES.
         05                    PIC x(43) Value
                               'Percentage w/ gpa > 3.0 and Seniors '.
         05                    PIC x(12) value spaces.
         05 percent-12         PIC zz9.99.
         05                    PIC x(01) value '%'.
         05                    pic x(14) value spaces.

       procedure division.
       100-main-module.
           perform 150-housekeeping-start.
           perform until are-there-more-records = 'no '
               read Student-File
                   at end
                       move 'no ' to are-there-more-records
                   not at end
                       perform 200-student-count-rtn
               end-read
           end-perform.
           Perform 350-housekeeping-finish.

           goback.

       150-housekeeping-start.
           open input Student-File output Student-Report.
           move function current-date to date-field
           move day_field to OUT-DD
           move month-field to OUT-MM
           move year-field to OUT-CCYY.

       200-student-count-rtn.
           add 1 to REC-COUNT.

           if CREDITS > 100
               add 1 to hicred-total
           end-if.

           if gpa < 2.00
               add 1 to gpa-2-total
               if credits > 100
                   add 1 to credits-total
               end-if
           else
               if gpa > 3.00
                   add 1 to gpa-4-total
               else
                   add 1 to gpa-3-total
               end-if
           end-if.

           if gpa > 3.00
               evaluate SCHOOL-CODE
                   when '1'
                       add 1 to busi-total
                   when '2'
                       add 1 to arts-total
                   when '3'
                       add 1 to engg-total
                   when other
                       continue
               end-evaluate

               evaluate CLASS-CODE
                   when '1'
                       add 1 to ccode-1-total
                   when '2'
                       add 1 to ccode-2-total
                   when '3'
                       add 1 to ccode-3-total
                   when '4'
                       add 1 to ccode-4-total
               end-evaluate
           end-if.

       350-housekeeping-finish.
           perform 400-write-report.
           close Student-File Student-Report.

       400-write-report.
           move REC-COUNT to out-cnt.
           write rpt-rec from heading-1
           write Rpt-Rec from BLANK-LINE after advancing 1.
           write Rpt-Rec from BLANK-LINE after advancing 1.

           divide gpa-2-total by REC-COUNT giving percent-calc
           multiply 100 by percent-calc giving percent-2
           write Rpt-Rec from heading-2 after advancing 1.

           divide gpa-3-total by REC-COUNT giving percent-calc
           multiply 100 by percent-calc giving percent-3
           write Rpt-Rec from heading-3 after advancing 1.

           divide gpa-4-total by REC-COUNT giving percent-calc
           multiply 100 by percent-calc giving percent-4
           write Rpt-Rec from heading-4 after advancing 1.
           write Rpt-Rec from BLANK-LINE after advancing 1.

           divide busi-total by gpa-4-total giving percent-calc
           multiply 100 by percent-calc giving percent-5
           write Rpt-Rec from heading-5 after advancing 1.

           divide arts-total by gpa-4-total giving percent-calc
           multiply 100 by percent-calc giving percent-6
           write Rpt-Rec from heading-6 after advancing 1.

           divide engg-total by gpa-4-total giving percent-calc
           multiply 100 by percent-calc giving percent-7
           write Rpt-Rec from heading-7 after advancing 1.

           divide credits-total by gpa-2-total giving percent-calc
           multiply 100 by percent-calc giving percent-8
           write Rpt-Rec from heading-8 after advancing 1.


           divide ccode-1-total by gpa-4-total giving percent-calc
           multiply 100 by percent-calc giving percent-9
           write Rpt-Rec from heading-9 after advancing 1.

           divide ccode-2-total by gpa-4-total giving percent-calc
           multiply 100 by percent-calc giving percent-10
           write Rpt-Rec from heading-10 after advancing 1.

           divide ccode-3-total by gpa-4-total giving percent-calc
           multiply 100 by percent-calc giving percent-11
           write Rpt-Rec from heading-11 after advancing 1.

           divide ccode-4-total by gpa-4-total giving percent-calc
           multiply 100 by percent-calc giving percent-12
           write Rpt-Rec from heading-12 after advancing 1.

       end program Program1.

VIEW-APPLICATIONS.
           MOVE 0 TO WS-APP-COUNT
           MOVE "N" TO WS-APP-EOF

           OPEN INPUT APP-FILE
           IF WS-APP-ST = "35"
               MOVE "--- My Applications ---" TO WS-TEXT
               PERFORM PRT
               MOVE "Total Applications: 0" TO WS-TEXT
               PERFORM PRT
               MOVE "You have not applied to any jobs or internships yet."
                   TO WS-TEXT
               PERFORM PRT
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-APP-EOF = "Y"
               READ APP-FILE
                   AT END
                       MOVE "Y" TO WS-APP-EOF
                   NOT AT END
                       IF FUNCTION TRIM(APP-USER TRAILING)
                          = FUNCTION TRIM(WS-UIN TRAILING)
                           ADD 1 TO WS-APP-COUNT
                       END-IF
               END-READ
           END-PERFORM
           CLOSE APP-FILE

           MOVE "--- My Applications ---" TO WS-TEXT
           PERFORM PRT

           MOVE SPACES TO WS-TEXT
           STRING "Total Applications: " DELIMITED BY SIZE
                  WS-APP-COUNT DELIMITED BY SIZE
                  INTO WS-TEXT
           END-STRING
           PERFORM PRT
           MOVE "----------------------------------------" TO WS-TEXT
           PERFORM PRT

           IF WS-APP-COUNT = 0
               MOVE "You have not applied to any jobs or internships yet."
                   TO WS-TEXT
               PERFORM PRT
               EXIT PARAGRAPH
           END-IF

           MOVE "N" TO WS-APP-EOF
           OPEN INPUT APP-FILE

           PERFORM UNTIL WS-APP-EOF = "Y"
               READ APP-FILE
                   AT END
                       MOVE "Y" TO WS-APP-EOF
                   NOT AT END
                       IF FUNCTION TRIM(APP-USER TRAILING)
                          = FUNCTION TRIM(WS-UIN TRAILING)
                           MOVE APP-JOB-ID TO WS-REPORT-JOB-ID
                           PERFORM PRINT-APPLICATION-DETAIL
                       END-IF
               END-READ
           END-PERFORM
           CLOSE APP-FILE.

       PRINT-APPLICATION-DETAIL.
           MOVE "N" TO WS-JOB-FOUND
           MOVE "N" TO WS-JOB-EOF

           OPEN INPUT JOB-FILE
           IF WS-JOB-ST = "35"
               MOVE "Title: Job record unavailable" TO WS-TEXT
               PERFORM PRT
               MOVE "Employer: Job record unavailable" TO WS-TEXT
               PERFORM PRT
               MOVE "Location: Job record unavailable" TO WS-TEXT
               PERFORM PRT
               MOVE "----------------------------------------" TO WS-TEXT
               PERFORM PRT
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-JOB-EOF = "Y" OR WS-JOB-FOUND = "Y"
               READ JOB-FILE
                   AT END
                       MOVE "Y" TO WS-JOB-EOF
                   NOT AT END
                       IF JOB-ID = WS-REPORT-JOB-ID
                           MOVE "Y" TO WS-JOB-FOUND

                           MOVE SPACES TO WS-TEXT
                           STRING "Title: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-TITLE TRAILING)
                                      DELIMITED BY SIZE
                                  INTO WS-TEXT
                           END-STRING
                           PERFORM PRT

                           MOVE SPACES TO WS-TEXT
                           STRING "Employer: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-EMPLOYER TRAILING)
                                      DELIMITED BY SIZE
                                  INTO WS-TEXT
                           END-STRING
                           PERFORM PRT

                           MOVE SPACES TO WS-TEXT
                           STRING "Location: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-LOCATION TRAILING)
                                      DELIMITED BY SIZE
                                  INTO WS-TEXT
                           END-STRING
                           PERFORM PRT

                           MOVE "----------------------------------------"
                               TO WS-TEXT
                           PERFORM PRT
                       END-IF
               END-READ
           END-PERFORM
           CLOSE JOB-FILE

           IF WS-JOB-FOUND = "N"
               MOVE "Title: Job record unavailable" TO WS-TEXT
               PERFORM PRT
               MOVE "Employer: Job record unavailable" TO WS-TEXT
               PERFORM PRT
               MOVE "Location: Job record unavailable" TO WS-TEXT
               PERFORM PRT
               MOVE "----------------------------------------" TO WS-TEXT
               PERFORM PRT
           END-IF.

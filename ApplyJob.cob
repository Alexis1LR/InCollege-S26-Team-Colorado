BROWSE-JOBS.
           PERFORM UNTIL WS-RUN = "N"
               MOVE "--- Available Job Listings ---" TO WS-TEXT
               PERFORM PRT

               MOVE "N" TO WS-JOB-EOF
               OPEN INPUT JOB-FILE
               IF WS-JOB-ST = "35"
                   MOVE "No jobs currently available." TO WS-TEXT
                   PERFORM PRT
                   CLOSE JOB-FILE
                   EXIT PARAGRAPH
               END-IF

               PERFORM UNTIL WS-JOB-EOF = "Y"
                   READ JOB-FILE
                       AT END
                           MOVE "Y" TO WS-JOB-EOF
                       NOT AT END
                           MOVE SPACES TO WS-TEXT
                           STRING
                               FUNCTION TRIM(JOB-TITLE TRAILING) DELIMITED BY SIZE
                               " at " DELIMITED BY SIZE
                               FUNCTION TRIM(JOB-EMPLOYER TRAILING) DELIMITED BY SIZE
                               " (" DELIMITED BY SIZE
                               FUNCTION TRIM(JOB-LOCATION TRAILING) DELIMITED BY SIZE
                               ")" DELIMITED BY SIZE
                               INTO WS-TEXT
                           END-STRING
                           PERFORM PRT
                   END-READ
               END-PERFORM
               CLOSE JOB-FILE

               MOVE "-----------------------------" TO WS-TEXT
               PERFORM PRT
               MOVE "Enter job number to view details, or 0 to go back:" TO WS-TEXT
               PERFORM PRT

               PERFORM READIN
               IF WS-EOF = "Y" EXIT PARAGRAPH END-IF
               PERFORM ECHOIN

               MOVE IN-REC(1:5) TO WS-SELECTED-JOB-TXT
               IF FUNCTION TEST-NUMVAL(WS-SELECTED-JOB-TXT) = 0
                   COMPUTE WS-SELECTED-JOB = FUNCTION NUMVAL(WS-SELECTED-JOB-TXT)
                   IF WS-SELECTED-JOB = 0
                       EXIT PARAGRAPH
                   ELSE
                       PERFORM VIEW-JOB-DETAILS
                   END-IF
               ELSE
                   MOVE "Invalid input. Please enter a number." TO WS-TEXT
                   PERFORM PRT
               END-IF
           END-PERFORM.

       VIEW-JOB-DETAILS.
           MOVE "N" TO WS-JOB-FOUND
           MOVE "N" TO WS-JOB-EOF

           OPEN INPUT JOB-FILE
           PERFORM UNTIL WS-JOB-EOF = "Y" OR WS-JOB-FOUND = "Y"
               READ JOB-FILE
                   AT END
                       MOVE "Y" TO WS-JOB-EOF
                   NOT AT END
                       IF JOB-ID = WS-SELECTED-JOB
                           MOVE "Y" TO WS-JOB-FOUND
                           MOVE "--- Job Details ---" TO WS-TEXT
                           PERFORM PRT

                           MOVE SPACES TO WS-TEXT
                           STRING "Title: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-TITLE TRAILING) DELIMITED BY SIZE
                                  INTO WS-TEXT
                           END-STRING
                           PERFORM PRT

                           MOVE SPACES TO WS-TEXT
                           STRING "Description: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-DESCRIPTION TRAILING) DELIMITED BY SIZE
                                  INTO WS-TEXT
                           END-STRING
                           PERFORM PRT

                           MOVE SPACES TO WS-TEXT
                           STRING "Employer: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-EMPLOYER TRAILING) DELIMITED BY SIZE
                                  INTO WS-TEXT
                           END-STRING
                           PERFORM PRT

                           MOVE SPACES TO WS-TEXT
                           STRING "Location: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-LOCATION TRAILING) DELIMITED BY SIZE
                                  INTO WS-TEXT
                           END-STRING
                           PERFORM PRT

                           MOVE SPACES TO WS-TEXT
                           STRING "Salary: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-SALARY TRAILING) DELIMITED BY SIZE
                                  INTO WS-TEXT
                           END-STRING
                           PERFORM PRT

                           MOVE "-------------------" TO WS-TEXT
                           PERFORM PRT

                           PERFORM JOB-ACTION-MENU
                       END-IF
               END-READ
           END-PERFORM
           CLOSE JOB-FILE

           IF WS-JOB-FOUND = "N"
               MOVE "Job not found." TO WS-TEXT
               PERFORM PRT
           END-IF.

       JOB-ACTION-MENU.
           MOVE "1. Apply for this Job" TO WS-TEXT
           PERFORM PRT
           MOVE "2. Back to Job List" TO WS-TEXT
           PERFORM PRT
           MOVE "Enter your choice:" TO WS-TEXT
           PERFORM PRT

           PERFORM READIN
           IF WS-EOF = "Y" EXIT PARAGRAPH END-IF
           PERFORM ECHOIN
           MOVE IN-REC(1:1) TO WS-SUB-CHOICE

           IF WS-SUB-CHOICE = '1'
               *> Open the file to append the new application
               OPEN EXTEND APP-FILE

               *> If the file doesn't exist (Status 35), create it first
               IF WS-APP-ST = "35"
                   OPEN OUTPUT APP-FILE
                   CLOSE APP-FILE
                   OPEN EXTEND APP-FILE
               END-IF

               *> Move the Job ID and Logged-in Username into the record
               MOVE WS-SELECTED-JOB TO APP-JOB-ID
               MOVE WS-UIN TO APP-USER

               *> Write to the file and close it
               WRITE APP-REC
               CLOSE APP-FILE

               *> Print the confirmation message
               MOVE SPACES TO WS-TEXT
               STRING "Your application for " DELIMITED BY SIZE
                      FUNCTION TRIM(JOB-TITLE TRAILING) DELIMITED BY SIZE
                      " at " DELIMITED BY SIZE
                      FUNCTION TRIM(JOB-EMPLOYER TRAILING) DELIMITED BY SIZE
                      " has been submitted." DELIMITED BY SIZE
                      INTO WS-TEXT
               END-STRING
               PERFORM PRT
           END-IF.
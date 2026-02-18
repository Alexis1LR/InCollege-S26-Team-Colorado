      *> ===============================================================
      *> VIEWREQ_SRC: View Pending Connection Requests (Copybook)
      *> ===============================================================
      *> ---------------------------------------------------------------
      *> VIEW-PENDING-REQUESTS: displays pending requests for logged user
      *> ---------------------------------------------------------------
       VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-TEXT
           PERFORM PRT
           MOVE 0 TO WS-PENDING-COUNT
           OPEN INPUT CONN-FILE
           IF WS-CONN-ST = "35"
               OPEN OUTPUT CONN-FILE
               CLOSE CONN-FILE
               MOVE
             "You have no pending connection requests at this time."
                 TO WS-TEXT
               PERFORM PRT
               MOVE "-----------------------------------" TO WS-TEXT
               PERFORM PRT
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-CONN-ST NOT = "00"
               READ CONN-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF CONN-STATUS = "P" AND CONN-RECIP = WS-UIN
                           ADD 1 TO WS-PENDING-COUNT
                           MOVE SPACES TO WS-TEXT
                           STRING
                               "  From: " DELIMITED BY SIZE
                               CONN-SENDER DELIMITED BY "  "
                               INTO WS-TEXT
                           END-STRING
                           PERFORM PRT
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONN-FILE
           IF WS-PENDING-COUNT = 0
               MOVE
             "You have no pending connection requests at this time."
                 TO WS-TEXT
               PERFORM PRT
           END-IF
           MOVE "-----------------------------------" TO WS-TEXT
           PERFORM PRT.

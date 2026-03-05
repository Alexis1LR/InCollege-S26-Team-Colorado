      *> ===============================================================
      *> VIEWNET_SRC: View My Network (Copybook)
      *> Covers: GCOL-101 (menu option added in InCollege.cob POST-MENU),
      *>         GCOL-102 (display established network list)
      *> ===============================================================

      *> ---------------------------------------------------------------
      *> VIEW-MY-NETWORK: reads Connections.dat for "C" records where
      *>   WS-UIN is either sender or recip, then cross-references
      *>   Accounts.dat to get the other user's full name + University/Major
      *> ---------------------------------------------------------------
       VIEW-MY-NETWORK.
           MOVE "--- Your Network ---" TO WS-TEXT
           PERFORM PRT
           MOVE 0 TO WS-PENDING-COUNT

           OPEN INPUT CONN-FILE
           IF WS-CONN-ST = "35"
               OPEN OUTPUT CONN-FILE
               CLOSE CONN-FILE
               MOVE "You have no established connections yet."
                 TO WS-TEXT
               PERFORM PRT
               MOVE "--------------------" TO WS-TEXT
               PERFORM PRT
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-CONN-ST NOT = "00"
               READ CONN-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF CONN-STATUS = "C"
                           IF CONN-SENDER = WS-UIN
      *>-- WS-UIN is the sender; other party is CONN-RECIP
                               MOVE CONN-RECIP TO WS-FOUND-ACCT-USER
                               PERFORM LOOKUP-AND-DISPLAY-CONN
                               ADD 1 TO WS-PENDING-COUNT
                           ELSE
                               IF CONN-RECIP = WS-UIN
      *>-- WS-UIN is the recipient; other party is CONN-SENDER
                                   MOVE CONN-SENDER TO WS-FOUND-ACCT-USER
                                   PERFORM LOOKUP-AND-DISPLAY-CONN
                                   ADD 1 TO WS-PENDING-COUNT
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONN-FILE

           IF WS-PENDING-COUNT = 0
               MOVE "You have no established connections yet."
                 TO WS-TEXT
               PERFORM PRT
           END-IF
           MOVE "--------------------" TO WS-TEXT
           PERFORM PRT.

      *> ---------------------------------------------------------------
      *> LOOKUP-AND-DISPLAY-CONN: given WS-FOUND-ACCT-USER, scans
      *>   Accounts.dat to find the matching record and prints:
      *>     "Connected with: FirstName LastName (University: X, Major: Y)"
      *> ---------------------------------------------------------------
       LOOKUP-AND-DISPLAY-CONN.
           OPEN INPUT ACCT-FILE
           PERFORM UNTIL WS-ACCT-ST NOT = "00"
               READ ACCT-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF ACCT-USER = WS-FOUND-ACCT-USER
                           MOVE SPACES TO WS-TEXT
                           STRING
                               "Connected with: "  DELIMITED BY SIZE
                               ACCT-FNAME          DELIMITED BY SPACE
                               " "                 DELIMITED BY SIZE
                               ACCT-LNAME          DELIMITED BY SPACE
                               " (University: "    DELIMITED BY SIZE
                               ACCT-UNIV           DELIMITED BY "  "
                               ", Major: "         DELIMITED BY SIZE
                               ACCT-MAJOR          DELIMITED BY "  "
                               ")"                 DELIMITED BY SIZE
                               INTO WS-TEXT
                           END-STRING
                           PERFORM PRT
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCT-FILE.

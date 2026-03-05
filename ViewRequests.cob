      *> ===============================================================
      *> VIEWREQ_SRC: View Pending Connection Requests (Copybook)
      *> Covers: GCOL-96 (view pending), GCOL-97 (accept), GCOL-98 (reject),
      *>         GCOL-99 (establish permanent), GCOL-100 (confirm outcome)
      *> ===============================================================

      *> ---------------------------------------------------------------
      *> VIEW-PENDING-REQUESTS: displays each pending request with
      *>   Accept/Reject prompt for the logged-in user
      *> ---------------------------------------------------------------
       VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-TEXT
           PERFORM PRT
           MOVE 0 TO WS-PENDING-COUNT

      *>-- First pass: count how many pending requests exist for this user
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
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONN-FILE

           IF WS-PENDING-COUNT = 0
               MOVE
             "You have no pending connection requests at this time."
                 TO WS-TEXT
               PERFORM PRT
               MOVE "-----------------------------------" TO WS-TEXT
               PERFORM PRT
               EXIT PARAGRAPH
           END-IF

      *>-- Second pass: display each request and prompt accept/reject
           PERFORM PROCESS-EACH-PENDING.

      *> ---------------------------------------------------------------
      *> PROCESS-EACH-PENDING: loops through Connections.dat and for
      *>   each pending request addressed to WS-UIN, prompts the user
      *>   to accept or reject. Rewrites the file without processed recs.
      *> ---------------------------------------------------------------
       PROCESS-EACH-PENDING.
           MOVE 0 TO WS-PENDING-COUNT

      *>-- Open CONN-FILE for input and CONN-TMP for rewrite output
           OPEN INPUT  CONN-FILE
           OPEN OUTPUT CONN-TMP
           MOVE "N" TO WS-CONN-EXISTS

           PERFORM UNTIL WS-CONN-ST NOT = "00"
               READ CONN-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF CONN-STATUS = "P" AND CONN-RECIP = WS-UIN
      *>-- Show the request to the user (GCOL-96)
                           ADD 1 TO WS-PENDING-COUNT
                           MOVE SPACES TO WS-TEXT
                           STRING
                               "Request from: " DELIMITED BY SIZE
                               CONN-SENDER      DELIMITED BY SPACE
                               INTO WS-TEXT
                           END-STRING
                           PERFORM PRT
                           MOVE "1. Accept" TO WS-TEXT
                           PERFORM PRT
                           MOVE "2. Reject" TO WS-TEXT
                           PERFORM PRT
                           MOVE SPACES TO WS-TEXT
                           STRING
                               "Enter your choice for "
                                   DELIMITED BY SIZE
                               WS-FOUND-ACCT-USER DELIMITED BY SPACE
                               ":"         DELIMITED BY SIZE
                               INTO WS-TEXT
                           END-STRING
                           PERFORM PRT
                           PERFORM READIN
                           IF WS-EOF = "Y"
                               WRITE CONN-TMP-REC FROM CONN-REC
                               EXIT PERFORM
                           END-IF
                           PERFORM ECHOIN
                           MOVE IN-REC(1:1) TO WS-CHOICE

                           IF WS-CHOICE = "1"
      *>-- ACCEPT (GCOL-97, GCOL-99):
      *>   Replace the pending record with an established connection ("C")
      *>   and ALSO write a reverse-direction "C" record so either user can
      *>   find the connection easily in VIEW-MY-NETWORK.
                               MOVE "C" TO CONN-STATUS
                               WRITE CONN-TMP-REC FROM CONN-REC
      *>   Reverse record (WS-UIN -> requester)
                               MOVE CONN-SENDER TO WS-FOUND-ACCT-USER
                               MOVE CONN-RECIP  TO CONN-SENDER
                               MOVE WS-FOUND-ACCT-USER TO CONN-RECIP
                               MOVE "C"         TO CONN-STATUS
                               WRITE CONN-TMP-REC FROM CONN-REC
      *>-- Confirmation (GCOL-100)
                               MOVE SPACES TO WS-TEXT
                               STRING
                                   "Connection request from "
                                       DELIMITED BY SIZE
                                   CONN-SENDER DELIMITED BY SPACE
                                   " accepted!"  DELIMITED BY SIZE
                                   INTO WS-TEXT
                               END-STRING
                               PERFORM PRT
                           ELSE
      *>-- REJECT (GCOL-98): just skip writing this record (remove it)
      *>-- Confirmation (GCOL-100)
                               MOVE SPACES TO WS-TEXT
                               STRING
                                   "Connection request from "
                                       DELIMITED BY SIZE
                                   CONN-SENDER DELIMITED BY SPACE
                                   " rejected."  DELIMITED BY SIZE
                                   INTO WS-TEXT
                               END-STRING
                               PERFORM PRT
                           END-IF
                       ELSE
      *>-- Not a pending request for this user: keep it as-is
                           WRITE CONN-TMP-REC FROM CONN-REC
                       END-IF
               END-READ
           END-PERFORM

           CLOSE CONN-FILE
           CLOSE CONN-TMP

      *>-- Replace Connections.dat with updated temp file
           CALL "SYSTEM" USING "mv Connections.tmp Connections.dat"

           MOVE "-----------------------------------" TO WS-TEXT
           PERFORM PRT.

      *> ---------------------------------------------------------------
      *> WRITE-ESTABLISHED-CONN: appends a "C" (connected) record to
      *>   Connections.dat after the rewrite is done.
      *>   WS-UIN = logged-in user, WS-FOUND-ACCT-USER = other party.
      *>   Both orderings are written so VIEW-MY-NETWORK can find either.
      *>   (GCOL-99: both users consider each other part of their network)
      *> ---------------------------------------------------------------
       WRITE-ESTABLISHED-CONN.
      *> Deprecated: established connections are now written during temp rewrite.
           EXIT PARAGRAPH.

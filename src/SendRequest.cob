      *> ===============================================================
      *> SENDREQ_SRC: Connection Request Sending Logic (Copybook)
      *> ===============================================================
      *> ---------------------------------------------------------------
      *> SEND-REQUEST-MENU: options after viewing a found user's profile
      *> ---------------------------------------------------------------
       SEND-REQUEST-MENU.
           MOVE "1. Send Connection Request" TO WS-TEXT
           PERFORM PRT
           MOVE "2. Back to Main Menu" TO WS-TEXT
           PERFORM PRT
           MOVE "Enter your choice:" TO WS-TEXT
           PERFORM PRT
           PERFORM READIN
           IF WS-EOF = "Y"
               EXIT PARAGRAPH
           END-IF
           PERFORM ECHOIN
           MOVE IN-REC(1:1) TO WS-CHOICE
           IF WS-CHOICE = "1"
               PERFORM SEND-CONNECTION-REQUEST
           END-IF.
      *> ---------------------------------------------------------------
      *> SEND-CONNECTION-REQUEST: validates and sends a request
      *> ---------------------------------------------------------------
       SEND-CONNECTION-REQUEST.
           MOVE "N" TO WS-CONN-EXISTS
      *> Don't allow connecting to yourself
           IF WS-FOUND-ACCT-USER = WS-UIN
               MOVE "You cannot send a connection request to yourself."
                 TO WS-TEXT
               PERFORM PRT
               EXIT PARAGRAPH
           END-IF
      *> Check existing connections and pending requests
           OPEN INPUT CONN-FILE
           IF WS-CONN-ST = "35"
               OPEN OUTPUT CONN-FILE
               CLOSE CONN-FILE
               OPEN INPUT CONN-FILE
           END-IF
           PERFORM UNTIL WS-CONN-ST NOT = "00"
               READ CONN-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
      *> Check if already connected (status C, either direction)
                       IF CONN-STATUS = "C"
                           IF (CONN-SENDER = WS-UIN AND
                               CONN-RECIP = WS-FOUND-ACCT-USER)
                           OR (CONN-SENDER = WS-FOUND-ACCT-USER AND
                               CONN-RECIP = WS-UIN)
                               MOVE "A" TO WS-CONN-EXISTS
                           END-IF
                       END-IF
      *> Check if we already sent a pending request
                       IF CONN-STATUS = "P"
                           IF CONN-SENDER = WS-UIN AND
                              CONN-RECIP = WS-FOUND-ACCT-USER
                               MOVE "S" TO WS-CONN-EXISTS
                           END-IF
                       END-IF
      *> Check if they already sent us a pending request
                       IF CONN-STATUS = "P"
                           IF CONN-SENDER = WS-FOUND-ACCT-USER AND
                              CONN-RECIP = WS-UIN
                               MOVE "R" TO WS-CONN-EXISTS
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONN-FILE
           EVALUATE WS-CONN-EXISTS
               WHEN "A"
                   MOVE "You are already connected with this user."
                     TO WS-TEXT
                   PERFORM PRT
               WHEN "S"
                   MOVE
                 "You have already sent a connection request to this user."
                     TO WS-TEXT
                   PERFORM PRT
               WHEN "R"
                   MOVE
               "This user has already sent you a connection request."
                     TO WS-TEXT
                   PERFORM PRT
               WHEN "N"
                   OPEN EXTEND CONN-FILE
                   MOVE WS-UIN TO CONN-SENDER
                   MOVE WS-FOUND-ACCT-USER TO CONN-RECIP
                   MOVE "P" TO CONN-STATUS
                   WRITE CONN-REC
                   CLOSE CONN-FILE
                   MOVE SPACES TO WS-TEXT
                   STRING
                       "Connection request sent to "
                           DELIMITED BY SIZE
                       WS-SEARCH-NAME DELIMITED BY "  "
                       "." DELIMITED BY SIZE
                       INTO WS-TEXT
                   END-STRING
                   PERFORM PRT
           END-EVALUATE.

	  *> ===============================================================
	  *> SENDMESSAGE_SRC: Messaging menu and send flow
	  *> ===============================================================

	  *> ---------------------------------------------------------------
	  *> MESSAGES-MENU: entry point for messaging options
	  *> ---------------------------------------------------------------
	   MESSAGES-MENU.
		   PERFORM UNTIL WS-RUN = "N"
			   MOVE "--- Messages Menu ---" TO WS-TEXT
			   PERFORM PRT
			   MOVE "1. Send a New Message" TO WS-TEXT
			   PERFORM PRT
			   MOVE "2. View My Messages" TO WS-TEXT
			   PERFORM PRT
			   MOVE "3. Back to Main Menu" TO WS-TEXT
			   PERFORM PRT
			   MOVE "Enter your choice:" TO WS-TEXT
			   PERFORM PRT
			   PERFORM READIN
			   IF WS-EOF = "Y"
				   MOVE "N" TO WS-RUN
				   EXIT PARAGRAPH
			   END-IF
			   PERFORM ECHOIN
			   MOVE IN-REC(1:1) TO WS-MSG-CHOICE
			   EVALUATE WS-MSG-CHOICE
				   WHEN '1'
					   PERFORM SEND-MESSAGE
				   WHEN '2'
					   PERFORM VIEW-MY-MESSAGES
				   WHEN '3'
					   EXIT PERFORM
				   WHEN OTHER
					   MOVE "Invalid choice, please try again." TO WS-TEXT
					   PERFORM PRT
			   END-EVALUATE
		   END-PERFORM.

	  *> ---------------------------------------------------------------
	  *> SEND-MESSAGE: validates connection, captures content, writes file
	  *> ---------------------------------------------------------------
	   SEND-MESSAGE.
		   MOVE "Enter recipient's username (must be a connection):" TO WS-TEXT
		   PERFORM PRT
		   PERFORM READIN
		   IF WS-EOF = "Y"
			   EXIT PARAGRAPH
		   END-IF
		   PERFORM ECHOIN
		   MOVE IN-REC(1:20) TO WS-MSG-RECIP
		   PERFORM VALIDATE-MESSAGE-CONNECTION
		   IF WS-MSG-CONNECTED NOT = "Y"
			   MOVE "You can only message users you are connected with." TO WS-TEXT
			   PERFORM PRT
			   EXIT PARAGRAPH
		   END-IF
		   MOVE "Enter your message (max 200 chars):" TO WS-TEXT
		   PERFORM PRT
		   PERFORM READIN
		   IF WS-EOF = "Y"
			   EXIT PARAGRAPH
		   END-IF
		   PERFORM ECHOIN
		   MOVE IN-REC(1:200) TO WS-MSG-CONTENT
		   PERFORM WRITE-MESSAGE
		   MOVE SPACES TO WS-TEXT
		   STRING
			   "Message sent to " DELIMITED BY SIZE
			   FUNCTION TRIM(WS-MSG-RECIP TRAILING) DELIMITED BY SIZE
			   " successfully!" DELIMITED BY SIZE
			   INTO WS-TEXT
		   END-STRING
		   PERFORM PRT.

	  *> ---------------------------------------------------------------
	  *> VALIDATE-MESSAGE-CONNECTION: ensure recipient is connected
	  *> ---------------------------------------------------------------
	   VALIDATE-MESSAGE-CONNECTION.
		   MOVE "N" TO WS-MSG-CONNECTED
		   OPEN INPUT CONN-FILE
		   IF WS-CONN-ST = "35"
			   CLOSE CONN-FILE
			   EXIT PARAGRAPH
		   END-IF
		   PERFORM UNTIL WS-CONN-ST NOT = "00"
			   READ CONN-FILE
				   AT END
					   EXIT PERFORM
				   NOT AT END
					   IF CONN-STATUS = "C"
						   IF (CONN-SENDER = WS-UIN AND
							   CONN-RECIP = WS-MSG-RECIP)
						   OR (CONN-SENDER = WS-MSG-RECIP AND
							   CONN-RECIP = WS-UIN)
							   MOVE "Y" TO WS-MSG-CONNECTED
							   EXIT PERFORM
						   END-IF
					   END-IF
			   END-READ
		   END-PERFORM
		   CLOSE CONN-FILE.

	  *> ---------------------------------------------------------------
	  *> WRITE-MESSAGE: append message with timestamp to persistent file
	  *> ---------------------------------------------------------------
	   WRITE-MESSAGE.
		   OPEN EXTEND MSG-FILE
		   IF WS-MSG-ST = "35"
			   OPEN OUTPUT MSG-FILE
			   CLOSE MSG-FILE
			   OPEN EXTEND MSG-FILE
		   END-IF
		   MOVE FUNCTION CURRENT-DATE(1:14) TO WS-TIMESTAMP
		   MOVE WS-UIN TO MSG-SENDER
		   MOVE WS-MSG-RECIP TO MSG-RECIP
		   MOVE WS-TIMESTAMP TO MSG-TIMESTAMP
		   MOVE WS-MSG-CONTENT TO MSG-CONTENT
		   WRITE MSG-REC
		   CLOSE MSG-FILE.

	  *> ---------------------------------------------------------------
	  *> VIEW-MY-MESSAGES: display all messages received by logged-in user
	  *> ---------------------------------------------------------------
	   VIEW-MY-MESSAGES.
		   MOVE "--- Your Messages ---" TO WS-TEXT
		   PERFORM PRT
		   MOVE 0 TO WS-MSG-COUNT
		   OPEN INPUT MSG-FILE
		   IF WS-MSG-ST = "35"
			   MOVE "You have no messages at this time." TO WS-TEXT
			   PERFORM PRT
			   MOVE "---------------------" TO WS-TEXT
			   PERFORM PRT
			   CLOSE MSG-FILE
			   EXIT PARAGRAPH
		   END-IF
		   PERFORM UNTIL WS-MSG-ST NOT = "00"
			   READ MSG-FILE
				   AT END
					   EXIT PERFORM
				   NOT AT END
					   IF MSG-RECIP = WS-UIN
						   ADD 1 TO WS-MSG-COUNT
						   MOVE SPACES TO WS-TEXT
						   STRING
							   "From: " DELIMITED BY SIZE
							   FUNCTION TRIM(MSG-SENDER TRAILING)
								   DELIMITED BY SIZE
							   INTO WS-TEXT
						   END-STRING
						   PERFORM PRT
						   MOVE SPACES TO WS-TEXT
						   STRING
							   "Message: " DELIMITED BY SIZE
							   FUNCTION TRIM(MSG-CONTENT TRAILING)
								   DELIMITED BY SIZE
							   INTO WS-TEXT
						   END-STRING
						   PERFORM PRT
						   MOVE SPACES TO WS-MSG-DISPLAY-TS
						   STRING
							   MSG-TIMESTAMP(1:4) "-"
							   MSG-TIMESTAMP(5:2) "-"
							   MSG-TIMESTAMP(7:2) " "
							   MSG-TIMESTAMP(9:2) ":"
							   MSG-TIMESTAMP(11:2) ":"
							   MSG-TIMESTAMP(13:2)
							   INTO WS-MSG-DISPLAY-TS
						   END-STRING
						   MOVE SPACES TO WS-TEXT
						   STRING
							   "Sent: " DELIMITED BY SIZE
							   FUNCTION TRIM(WS-MSG-DISPLAY-TS TRAILING)
								   DELIMITED BY SIZE
							   INTO WS-TEXT
						   END-STRING
						   PERFORM PRT
						   MOVE "---" TO WS-TEXT
						   PERFORM PRT
					   END-IF
			   END-READ
		   END-PERFORM
		   CLOSE MSG-FILE
		   IF WS-MSG-COUNT = 0
			   MOVE "You have no messages at this time." TO WS-TEXT
			   PERFORM PRT
		   END-IF
		   MOVE "---------------------" TO WS-TEXT
		   PERFORM PRT.

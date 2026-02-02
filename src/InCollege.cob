       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-ST.
           SELECT OUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-ST.
           SELECT ACCT-FILE ASSIGN TO "Accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCT-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  IN-FILE.
       01  IN-REC              PIC X(200).

       FD  OUT-FILE.
       01  OUT-REC             PIC X(200).

       FD  ACCT-FILE.
       01  ACCT-REC.
           05 ACCT-USER        PIC X(20).
           05 ACCT-PASS        PIC X(12).
           *>New profile info
           05 ACCT-PROFILE-DATA PIC X(300).
           05 ACCT-FNAME        PIC X(20).
           05 ACCT-LNAME        PIC X(20).
           05 ACCT-UNIV         PIC X(40).
           05 ACCT-MAJOR        PIC X(30).
           05 ACCT-GRAD-YEAR    PIC X(4).
           05 ACCT-ABOUT        PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-IN-ST            PIC XX VALUE "00".
       01  WS-OUT-ST           PIC XX VALUE "00".
       01  WS-ACCT-ST          PIC XX VALUE "00".

       01  WS-EOF              PIC X  VALUE "N".
       01  WS-RUN              PIC X  VALUE "Y".
       01  WS-LOG              PIC X  VALUE "N".

       01  WS-TEXT             PIC X(200).
       01  WS-CHOICE           PIC X.

       01  WS-UIN              PIC X(20).
       01  WS-PIN              PIC X(12).

       01  WS-COUNT            PIC 9 VALUE 0.
       01  I                   PIC 9 VALUE 0.
       01  K                   PIC 99 VALUE 0.

       01  WS-OK               PIC X VALUE "N".
       01  WS-LEN              PIC 99 VALUE 0.
       01  WS-UP               PIC X VALUE "N".
       01  WS-DI               PIC X VALUE "N".
       01  WS-SP               PIC X VALUE "N".
       01  WS-CH               PIC X VALUE SPACE.
       01  WS-PUSH-FLAG        PIC X VALUE "N".
       01  WS-PUSH-REC         PIC X(200) VALUE SPACES.
       *>store index of current user logged in for easier indexing of profile data
       01  WS-CURRENT-IDX       PIC 9.

       01  WS-ACCTS.
           05 WS-A OCCURS 5 TIMES.
              10 WS-USER       PIC X(20) VALUE SPACES.
              10 WS-PASS       PIC X(12) VALUE SPACES.


       01  WS-PROFILE-DATA.
           05 WS-FNAME          PIC X(20) VALUE SPACES.
           05 WS-LNAME          PIC X(20) VALUE SPACES.
           05 WS-UNIV           PIC X(40) VALUE SPACES.
           05 WS-MAJOR          PIC X(30) VALUE SPACES.
           05 WS-GRAD-YEAR      PIC X(4)  VALUE SPACES.
           05 WS-ABOUT          PIC X(200) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM STARTUP
           PERFORM TOP-MENU UNTIL WS-RUN = "N"
           PERFORM SHUTDOWN
           STOP RUN.

       STARTUP.
           OPEN INPUT IN-FILE
           OPEN OUTPUT OUT-FILE
           PERFORM LOAD-ACCTS

           *> Optional empty-file check (SAFE now)
           PERFORM READIN
           IF WS-EOF = "Y"
               MOVE "N" TO WS-RUN
           ELSE
               MOVE IN-REC TO WS-PUSH-REC
               MOVE "Y" TO WS-PUSH-FLAG
           END-IF.

       SHUTDOWN.
           MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO WS-TEXT
           PERFORM PRT
           CLOSE IN-FILE
           CLOSE OUT-FILE.

       *>Stores string in WS-TEXT into output file
       *>Prints string to terminal
       PRT.
           MOVE WS-TEXT TO OUT-REC
           DISPLAY WS-TEXT
           WRITE OUT-REC.

       READIN.
           IF WS-PUSH-FLAG = "Y"
               MOVE WS-PUSH-REC TO IN-REC
               MOVE "N" TO WS-PUSH-FLAG
               EXIT PARAGRAPH
           END-IF

           READ IN-FILE
               AT END
                   MOVE "Y" TO WS-EOF
                   MOVE SPACES TO IN-REC
           END-READ.

       ECHOIN.
           MOVE IN-REC TO OUT-REC
           DISPLAY IN-REC
           WRITE OUT-REC.

       LOAD-ACCTS.
           MOVE 0 TO WS-COUNT
           OPEN INPUT ACCT-FILE
           IF WS-ACCT-ST = "35"
               OPEN OUTPUT ACCT-FILE
               CLOSE ACCT-FILE
               OPEN INPUT ACCT-FILE
           END-IF
           PERFORM UNTIL WS-COUNT >= 5
               READ ACCT-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       ADD 1 TO WS-COUNT
                       MOVE ACCT-USER TO WS-USER(WS-COUNT)
                       MOVE ACCT-PASS TO WS-PASS(WS-COUNT)
               END-READ
           END-PERFORM
           CLOSE ACCT-FILE.

       SAVE-ACCT.
           OPEN EXTEND ACCT-FILE
      *>    IF WS-ACCT-ST = "35"
      *>       OPEN OUTPUT ACCT-FILE
      *>        CLOSE ACCT-FILE
      *>       OPEN EXTEND ACCT-FILE
      *>    END-IF
           MOVE WS-UIN TO ACCT-USER
           MOVE WS-PIN TO ACCT-PASS
           MOVE WS-FNAME TO ACCT-FNAME
           MOVE WS-LNAME TO ACCT-LNAME
           MOVE WS-UNIV TO ACCT-UNIV
           MOVE WS-MAJOR TO ACCT-MAJOR
           MOVE WS-GRAD-YEAR TO ACCT-GRAD-YEAR
           MOVE WS-ABOUT TO ACCT-ABOUT
           WRITE ACCT-REC
           CLOSE ACCT-FILE.

       TOP-MENU.
           MOVE "Welcome to InCollege!" TO WS-TEXT
           PERFORM PRT
           MOVE "Log In" TO WS-TEXT
           PERFORM PRT
           MOVE "Create New Account" TO WS-TEXT
           PERFORM PRT
           MOVE "Enter your choice:" TO WS-TEXT
           PERFORM PRT

           PERFORM READIN
           IF WS-EOF = "Y"
               MOVE "N" TO WS-RUN
               EXIT PARAGRAPH
           END-IF
           PERFORM ECHOIN

           MOVE IN-REC(1:1) TO WS-CHOICE
           IF WS-CHOICE = "1"
               PERFORM LOGIN-FLOW
           ELSE
               IF WS-CHOICE = "2"
                   PERFORM CREATE-FLOW
               ELSE
                   MOVE "Invalid choice, please try again." TO WS-TEXT
                   PERFORM PRT
               END-IF
           END-IF.

       CREATE-FLOW.
           IF WS-COUNT >= 5
               MOVE SPACES TO WS-TEXT
               STRING
                 "All permitted accounts have been created, "
                 DELIMITED BY SIZE
                 "please come back later"
                 DELIMITED BY SIZE
                 INTO WS-TEXT
               END-STRING
               PERFORM PRT
               EXIT PARAGRAPH
           END-IF

           MOVE "N" TO WS-OK
           PERFORM UNTIL WS-OK = "Y"
               MOVE "Please enter your username:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               IF WS-EOF = "Y"
                   MOVE "N" TO WS-RUN
                   EXIT PARAGRAPH
               END-IF
               PERFORM ECHOIN
               MOVE IN-REC(1:20) TO WS-UIN
               PERFORM CHECK-USER
               IF WS-OK = "N"
                   MOVE "Username already exists, please try again."
                     TO WS-TEXT
                   PERFORM PRT
               END-IF
           END-PERFORM

           MOVE "N" TO WS-OK
           PERFORM UNTIL WS-OK = "Y"
               MOVE "Please enter your password:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               IF WS-EOF = "Y"
                   MOVE "N" TO WS-RUN
                   EXIT PARAGRAPH
               END-IF
               PERFORM ECHOIN
               MOVE IN-REC(1:12) TO WS-PIN
               PERFORM CHECK-PASS
               IF WS-OK = "N"
                   MOVE SPACES TO WS-TEXT
                   STRING
                     "Password does not meet requirements, "
                     DELIMITED BY SIZE
                     "please try again."
                     DELIMITED BY SIZE
                     INTO WS-TEXT
                   END-STRING
                   PERFORM PRT
               END-IF
           END-PERFORM

           ADD 1 TO WS-COUNT
           MOVE WS-UIN TO WS-USER(WS-COUNT)
           MOVE WS-PIN TO WS-PASS(WS-COUNT)
           PERFORM SAVE-ACCT
           MOVE "Account created successfully." TO WS-TEXT
           PERFORM PRT.

       CHECK-USER.
           MOVE "Y" TO WS-OK
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-COUNT
               IF WS-UIN = WS-USER(I)
                   MOVE "N" TO WS-OK
               END-IF
           END-PERFORM.

       CHECK-PASS.
           MOVE "N" TO WS-OK
           MOVE 0 TO WS-LEN
           MOVE "N" TO WS-UP
           MOVE "N" TO WS-DI
           MOVE "N" TO WS-SP

           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 12
               MOVE WS-PIN(K:1) TO WS-CH
               IF WS-CH NOT = SPACE
                   ADD 1 TO WS-LEN
               END-IF
           END-PERFORM

           IF WS-LEN < 8
               EXIT PARAGRAPH
           END-IF
           IF WS-LEN > 12
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING K FROM 1 BY 1 UNTIL K > WS-LEN
               MOVE WS-PIN(K:1) TO WS-CH
               IF WS-CH >= "A" AND WS-CH <= "Z"
                   MOVE "Y" TO WS-UP
               END-IF
               IF WS-CH >= "0" AND WS-CH <= "9"
                   MOVE "Y" TO WS-DI
               END-IF
               IF WS-CH >= "a" AND WS-CH <= "z"
                   CONTINUE
               ELSE
                   IF WS-CH >= "A" AND WS-CH <= "Z"
                       CONTINUE
                   ELSE
                       IF WS-CH >= "0" AND WS-CH <= "9"
                           CONTINUE
                       ELSE
                           MOVE "Y" TO WS-SP
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF WS-UP = "Y"
               IF WS-DI = "Y"
                   IF WS-SP = "Y"
                       MOVE "Y" TO WS-OK
                   END-IF
               END-IF
           END-IF.

       LOGIN-FLOW.
           PERFORM UNTIL WS-LOG = "Y" OR WS-RUN = "N"
               MOVE "Please enter your username:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               IF WS-EOF = "Y"
                   MOVE "N" TO WS-RUN
                   EXIT PARAGRAPH
               END-IF
               PERFORM ECHOIN
               MOVE IN-REC(1:20) TO WS-UIN

               MOVE "Please enter your password:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               IF WS-EOF = "Y"
                   MOVE "N" TO WS-RUN
                   EXIT PARAGRAPH
               END-IF
               PERFORM ECHOIN
               MOVE IN-REC(1:12) TO WS-PIN

               PERFORM CHECK-CRED
               IF WS-OK = "Y"
                   MOVE "Y" TO WS-LOG
                   MOVE "You have successfully logged in." TO WS-TEXT
                   PERFORM PRT
                   PERFORM POST-MENU
               ELSE
                   MOVE SPACES TO WS-TEXT
                   STRING
                     "Incorrect username/password, "
                     DELIMITED BY SIZE
                     "please try again"
                     DELIMITED BY SIZE
                     INTO WS-TEXT
                   END-STRING
                   PERFORM PRT
               END-IF
           END-PERFORM.

       CHECK-CRED.
           MOVE "N" TO WS-OK
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-COUNT
               IF WS-UIN = WS-USER(I)
                   IF WS-PIN = WS-PASS(I)
                       MOVE "Y" TO WS-OK
                   END-IF
               END-IF
           END-PERFORM.

       POST-MENU.
           *> Print real username instead of [Username]
           MOVE SPACES TO WS-TEXT
           STRING
               "Welcome, " DELIMITED BY SIZE
               WS-UIN       DELIMITED BY SPACE
               "!"           DELIMITED BY SIZE
               INTO WS-TEXT
           END-STRING
           PERFORM PRT

           PERFORM UNTIL WS-RUN = "N"
               MOVE "1. Search for a job" TO WS-TEXT
               PERFORM PRT
               MOVE "2. Find someone you know" TO WS-TEXT
               PERFORM PRT
               MOVE "3. Learn a new skill" TO WS-TEXT
               PERFORM PRT
               MOVE "4. Create/Edit My Profile" TO WS-TEXT
               PERFORM PRT
               MOVE "Logout" TO WS-TEXT
               PERFORM PRT
               MOVE "Enter your choice:" TO WS-TEXT
               PERFORM PRT

               PERFORM READIN
               IF WS-EOF = "Y"
                   MOVE "N" TO WS-RUN
                   EXIT PARAGRAPH
               END-IF
               PERFORM ECHOIN
               MOVE IN-REC(1:1) TO WS-CHOICE

               IF WS-CHOICE = '1'
                   MOVE SPACES TO WS-TEXT
                   STRING
                     "Job search/internship is "
                     DELIMITED BY SIZE
                     "under construction."
                     DELIMITED BY SIZE
                     INTO WS-TEXT
                   END-STRING
                   PERFORM PRT
               ELSE
                   IF WS-CHOICE = '2'
                       MOVE SPACES TO WS-TEXT
                       STRING
                         "Find someone you know is "
                         DELIMITED BY SIZE
                         "under construction."
                         DELIMITED BY SIZE
                         INTO WS-TEXT
                       END-STRING
                       PERFORM PRT
                   ELSE
                       IF WS-CHOICE = '3'
                           PERFORM SKILL-MENU
                       ELSE
                           IF WS-CHOICE = '4'
                           PERFORM PROFILE-CREATE
                       ELSE
                           IF IN-REC(1:6) = "Logout"
                               MOVE "N" TO WS-RUN
                           ELSE
                               MOVE "Invalid choice, please try again."
                                 TO WS-TEXT
                               PERFORM PRT
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       SKILL-MENU.
           MOVE "Learn a New Skill:" TO WS-TEXT
           PERFORM PRT
           MOVE "Skill 1" TO WS-TEXT
           PERFORM PRT
           MOVE "Skill 2" TO WS-TEXT
           PERFORM PRT
           MOVE "Skill 3" TO WS-TEXT
           PERFORM PRT
           MOVE "Skill 4" TO WS-TEXT
           PERFORM PRT
           MOVE "Skill 5" TO WS-TEXT
           PERFORM PRT
           MOVE "Go Back" TO WS-TEXT
           PERFORM PRT
           MOVE "Enter your choice:" TO WS-TEXT
           PERFORM PRT

           PERFORM READIN
           IF WS-EOF = "Y"
               EXIT PARAGRAPH
           END-IF
           PERFORM ECHOIN
           MOVE IN-REC(1:1) TO WS-CHOICE

           IF WS-CHOICE >= '1' AND WS-CHOICE <= '5'
               MOVE "This skill is under construction." TO WS-TEXT
               PERFORM PRT
           END-IF.

       PROFILE-CREATE.
           MOVE "Create/Edit Profile" TO WS-TEXT
           PERFORM PRT
           MOVE "Enter First Name (required):" TO WS-TEXT
           PERFORM PRT
           PERFORM READIN
           PERFORM ECHOIN
           MOVE IN-REC(1:20) TO WS-FNAME
           MOVE "Enter Last Name (required):" TO WS-TEXT
           PERFORM PRT
           PERFORM READIN
           PERFORM ECHOIN
           MOVE IN-REC(1:20) TO WS-LNAME
           MOVE "Enter University/College Attended (required)" TO WS-TEXT
           PERFORM PRT
           PERFORM READIN
           PERFORM ECHOIN
           MOVE IN-REC(1:20) TO WS-UNIV
           MOVE "Enter Major (required):" TO WS-TEXT
           PERFORM PRT
           PERFORM READIN
           PERFORM ECHOIN
           MOVE IN-REC(1:20) TO WS-MAJOR
           MOVE "Enter Graduation Year (YYYY) (required)" TO WS-TEXT *> must meet requirements
           PERFORM PRT
           PERFORM READIN
           PERFORM ECHOIN
           MOVE IN-REC(1:20) TO WS-GRAD-YEAR
           MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip)" TO WS-TEXT *>msut meed requirements
           PERFORM PRT
           PERFORM READIN
           PERFORM ECHOIN
           MOVE IN-REC(1:20) TO WS-ABOUT.
           MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO WS-TEXT *>must meet rqeuirements
           PERFORM PRT
           PERFORM READIN
           PERFORM ECHOIN
           PERFORM UPDATE-PROFILE-FILE.


       UPDATE-PROFILE-FILE.
           OPEN EXTEND ACCT-FILE.
           MOVE WS-UIN TO ACCT-USER
           MOVE WS-PIN TO ACCT-PASS
           STRING
               WS-FNAME DELIMITED BY SIZE
               WS-LNAME DELIMITED BY SIZE
               WS-UNIV  DELIMITED BY SIZE
               WS-MAJOR DELIMITED BY SIZE
               WS-GRAD-YEAR DELIMITED BY SIZE
               WS-ABOUT DELIMITED BY SIZE
               INTO ACCT-PROFILE-DATA
           END-STRING.
           WRITE ACCT-REC.
           CLOSE ACCT-FILE.
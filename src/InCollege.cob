      *> ===============================================================
      *> INCOLLEGE.CBL  (Epic #2: User Profile Management)
      *> ===============================================================

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
           SELECT ACCT-TMP  ASSIGN TO "Accounts.tmp"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCT-TMP-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  IN-FILE.
       01  IN-REC              PIC X(200).

       FD  OUT-FILE.
       01  OUT-REC             PIC X(200).

      *> Accounts.dat record layout: credentials + profile + 3 exp + 3 edu
       FD  ACCT-FILE.
       01  ACCT-REC.
           05 ACCT-USER          PIC X(20).
           05 ACCT-PASS          PIC X(12).

           05 ACCT-FNAME         PIC X(20).
           05 ACCT-LNAME         PIC X(20).
           05 ACCT-UNIV          PIC X(40).
           05 ACCT-MAJOR         PIC X(30).
           05 ACCT-GRAD-YEAR     PIC 9(4).
           05 ACCT-ABOUT         PIC X(200).

           05 ACCT-EXP-COUNT     PIC 9.
           05 ACCT-EXP OCCURS 3 TIMES.
              10 ACCT-EXP-TITLE  PIC X(30).
              10 ACCT-EXP-COMP   PIC X(40).
              10 ACCT-EXP-DATES  PIC X(20).
              10 ACCT-EXP-DESC   PIC X(100).

           05 ACCT-EDU-COUNT     PIC 9.
           05 ACCT-EDU OCCURS 3 TIMES.
              10 ACCT-EDU-DEG    PIC X(30).
              10 ACCT-EDU-SCH    PIC X(40).
              10 ACCT-EDU-YEARS  PIC X(15).

      *> Temp file uses the exact same record layout for easy copy
       FD  ACCT-TMP.
       01  ACCT-TMP-REC            PIC X(1500).

       WORKING-STORAGE SECTION.
       01  WS-IN-ST            PIC XX VALUE "00".
       01  WS-OUT-ST           PIC XX VALUE "00".
       01  WS-ACCT-ST          PIC XX VALUE "00".
       01  WS-ACCT-TMP-ST      PIC XX VALUE "00".

       01  WS-EOF              PIC X  VALUE "N".
       01  WS-RUN              PIC X  VALUE "Y".
       01  WS-LOG              PIC X  VALUE "N".

       01  WS-TEXT             PIC X(200) VALUE SPACES.
       01  WS-CHOICE           PIC X     VALUE SPACE.

       01  WS-UIN              PIC X(20) VALUE SPACES.
       01  WS-PIN              PIC X(12) VALUE SPACES.

       01  WS-COUNT            PIC 9 VALUE 0.
       01  I                   PIC 9 VALUE 0.
       01  J                   PIC 9 VALUE 0.
       01  K                   PIC 99 VALUE 0.

       01  WS-OK               PIC X VALUE "N".
       01  WS-LEN              PIC 99 VALUE 0.
       01  WS-UP               PIC X VALUE "N".
       01  WS-DI               PIC X VALUE "N".
       01  WS-SP               PIC X VALUE "N".
       01  WS-CH               PIC X VALUE SPACE.

      *> Pushback buffer
       01  WS-PUSH-FLAG        PIC X VALUE "N".
       01  WS-PUSH-REC         PIC X(200) VALUE SPACES.

      *> Track which WS-A index is logged in
       01  WS-CURRENT-IDX      PIC 9 VALUE 0.

       01  WS-ACCTS.
           05 WS-A OCCURS 5 TIMES.
              10 WS-USER       PIC X(20) VALUE SPACES.
              10 WS-PASS       PIC X(12) VALUE SPACES.

      *> In-memory profile for the current user
       01  WS-PROFILE.
           05 WS-FNAME          PIC X(20)  VALUE SPACES.
           05 WS-LNAME          PIC X(20)  VALUE SPACES.
           05 WS-UNIV           PIC X(40)  VALUE SPACES.
           05 WS-MAJOR          PIC X(30)  VALUE SPACES.
           05 WS-GRAD-YEAR      PIC 9(4)   VALUE 0.
           05 WS-ABOUT          PIC X(200) VALUE SPACES.

           05 WS-EXP-COUNT      PIC 9 VALUE 0.
           05 WS-EXP OCCURS 3 TIMES.
              10 WS-EXP-TITLE   PIC X(30)  VALUE SPACES.
              10 WS-EXP-COMPANY PIC X(40)  VALUE SPACES.
              10 WS-EXP-DATES   PIC X(20)  VALUE SPACES.
              10 WS-EXP-DESC    PIC X(100) VALUE SPACES.

           05 WS-EDU-COUNT      PIC 9 VALUE 0.
           05 WS-EDU OCCURS 3 TIMES.
              10 WS-EDU-DEGREE  PIC X(30) VALUE SPACES.
              10 WS-EDU-SCHOOL  PIC X(40) VALUE SPACES.
              10 WS-EDU-YEARS   PIC X(15) VALUE SPACES.

       01  WS-YEAR-TXT          PIC X(20) VALUE SPACES.
       01  WS-YEAR-NUM          PIC 9(4)  VALUE 0.
       01  WS-VALID             PIC X VALUE "N".

      *> Commands for OS-level file swap
       01  WS-CMD               PIC X(120) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM STARTUP
           PERFORM TOP-MENU UNTIL WS-RUN = "N"
           PERFORM SHUTDOWN
           STOP RUN.

      *> ---------------------------------------------------------------
      *> STARTUP: open files, load accounts, prime input (pushback)
      *> ---------------------------------------------------------------
       STARTUP.
           OPEN INPUT IN-FILE
           OPEN OUTPUT OUT-FILE
           PERFORM LOAD-ACCTS

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

      *> ---------------------------------------------------------------
      *> PRT: display and write to output file
      *> ---------------------------------------------------------------
       PRT.
           MOVE WS-TEXT TO OUT-REC
           DISPLAY WS-TEXT
           WRITE OUT-REC.

      *> ---------------------------------------------------------------
      *> READIN: reads next line from input; supports pushback
      *> ---------------------------------------------------------------
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

      *> ---------------------------------------------------------------
      *> ECHOIN: echoes input line to terminal and output file
      *> ---------------------------------------------------------------
       ECHOIN.
           MOVE IN-REC TO OUT-REC
           DISPLAY IN-REC
           WRITE OUT-REC.

      *> ---------------------------------------------------------------
      *> LOAD-ACCTS: loads up to 5 accounts into memory (user/pass)
      *> ---------------------------------------------------------------
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

      *> ---------------------------------------------------------------
      *> SAVE-ACCT: appends a new account record with blank profile
      *> ---------------------------------------------------------------
       SAVE-ACCT.
           OPEN EXTEND ACCT-FILE

           MOVE WS-UIN TO ACCT-USER
           MOVE WS-PIN TO ACCT-PASS

           MOVE SPACES TO ACCT-FNAME ACCT-LNAME ACCT-UNIV
                        ACCT-MAJOR ACCT-ABOUT
           MOVE 0      TO ACCT-GRAD-YEAR
           MOVE 0      TO ACCT-EXP-COUNT
           MOVE 0      TO ACCT-EDU-COUNT

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
               MOVE SPACES TO ACCT-EXP-TITLE(J) ACCT-EXP-COMP(J)
                              ACCT-EXP-DATES(J) ACCT-EXP-DESC(J)
               MOVE SPACES TO ACCT-EDU-DEG(J) ACCT-EDU-SCH(J)
                              ACCT-EDU-YEARS(J)
           END-PERFORM

           WRITE ACCT-REC
           CLOSE ACCT-FILE.

      *> ---------------------------------------------------------------
      *> TOP-MENU: Welcome / Login / Create New Account
      *> ---------------------------------------------------------------
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

      *> ---------------------------------------------------------------
      *> CREATE-FLOW: create up to 5 accounts, unique username, valid pass
      *> ---------------------------------------------------------------
       CREATE-FLOW.
           IF WS-COUNT >= 5
               MOVE "All permitted accounts have been created, please come back later"
                 TO WS-TEXT
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
                   MOVE "Password does not meet requirements, please try again."
                     TO WS-TEXT
                   PERFORM PRT
               END-IF
           END-PERFORM

           ADD 1 TO WS-COUNT
           MOVE WS-UIN TO WS-USER(WS-COUNT)
           MOVE WS-PIN TO WS-PASS(WS-COUNT)
           PERFORM SAVE-ACCT

           MOVE "Account created successfully." TO WS-TEXT
           PERFORM PRT.

      *> ---------------------------------------------------------------
      *> CHECK-USER: WS-OK = N if username already exists
      *> ---------------------------------------------------------------
       CHECK-USER.
           MOVE "Y" TO WS-OK
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-COUNT
               IF WS-UIN = WS-USER(I)
                   MOVE "N" TO WS-OK
               END-IF
           END-PERFORM.

      *> ---------------------------------------------------------------
      *> CHECK-PASS: WS-OK = Y if 8-12 chars, 1 uppercase, 1 digit, 1 special
      *> ---------------------------------------------------------------
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

           IF WS-LEN < 8 OR WS-LEN > 12
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

               *> If not alphanumeric, consider it a special char
               IF NOT ( (WS-CH >= "a" AND WS-CH <= "z")
                    OR (WS-CH >= "A" AND WS-CH <= "Z")
                    OR (WS-CH >= "0" AND WS-CH <= "9") )
                   MOVE "Y" TO WS-SP
               END-IF
           END-PERFORM

           IF WS-UP = "Y" AND WS-DI = "Y" AND WS-SP = "Y"
               MOVE "Y" TO WS-OK
           END-IF.

      *> ---------------------------------------------------------------
      *> LOGIN-FLOW: loop until success or EOF
      *> ---------------------------------------------------------------
       LOGIN-FLOW.
           MOVE "N" TO WS-LOG
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
                   MOVE "Incorrect username/password, please try again"
                     TO WS-TEXT
                   PERFORM PRT
               END-IF
           END-PERFORM.

      *> ---------------------------------------------------------------
      *> CHECK-CRED: validates username/pass and sets current user index
      *> ---------------------------------------------------------------
       CHECK-CRED.
           MOVE "N" TO WS-OK
           MOVE 0   TO WS-CURRENT-IDX
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-COUNT
               IF WS-UIN = WS-USER(I)
                   IF WS-PIN = WS-PASS(I)
                       MOVE "Y" TO WS-OK
                       MOVE I   TO WS-CURRENT-IDX
                   END-IF
               END-IF
           END-PERFORM.

      *> ---------------------------------------------------------------
      *> POST-MENU
      *> ---------------------------------------------------------------
       POST-MENU.
           MOVE SPACES TO WS-TEXT
           STRING
               "Welcome, " DELIMITED BY SIZE
               WS-UIN       DELIMITED BY SPACE
               "!"          DELIMITED BY SIZE
               INTO WS-TEXT
           END-STRING
           PERFORM PRT

           PERFORM UNTIL WS-RUN = "N"
               MOVE "1. Create/Edit My Profile" TO WS-TEXT
               PERFORM PRT
               MOVE "2. View My Profile" TO WS-TEXT
               PERFORM PRT
               MOVE "3. Search for a job" TO WS-TEXT
               PERFORM PRT
               MOVE "4. Find someone you know" TO WS-TEXT
               PERFORM PRT
               MOVE "5. Learn a New Skill" TO WS-TEXT
               PERFORM PRT
               MOVE "6. Logout" TO WS-TEXT
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

               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM PROFILE-CREATE-EDIT
                   WHEN '2' PERFORM PROFILE-VIEW
                   WHEN '3'
                       MOVE "Job search/internship is under construction."
                         TO WS-TEXT
                       PERFORM PRT
                   WHEN '4'
                       MOVE "Find someone you know is under construction."
                         TO WS-TEXT
                       PERFORM PRT
                   WHEN '5' PERFORM SKILL-MENU
                   WHEN '6' MOVE "N" TO WS-RUN
                   WHEN OTHER
                       MOVE "Invalid choice, please try again." TO WS-TEXT
                       PERFORM PRT
               END-EVALUATE
           END-PERFORM.

      *> ---------------------------------------------------------------
      *> SKILL-MENU
      *> ---------------------------------------------------------------
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

      *> ===============================================================
      *> PROFILE MANAGEMENT
      *> ===============================================================

      *> ---------------------------------------------------------------
      *> PROFILE-CREATE-EDIT: prompts user for profile fields and saves
      *> ---------------------------------------------------------------
       PROFILE-CREATE-EDIT.
           MOVE "--- Create/Edit Profile ---" TO WS-TEXT
           PERFORM PRT

           PERFORM PROFILE-LOAD-FOR-USER

      *> Required field: First Name
           MOVE "N" TO WS-VALID
           PERFORM UNTIL WS-VALID = "Y"
               MOVE "Enter First Name:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               IF WS-EOF = "Y" EXIT PARAGRAPH END-IF
               PERFORM ECHOIN
               IF IN-REC = SPACES
                   MOVE "This field is required." TO WS-TEXT
                   PERFORM PRT
               ELSE
                   MOVE IN-REC(1:20) TO WS-FNAME
                   MOVE "Y" TO WS-VALID
               END-IF
           END-PERFORM

      *> Required field: Last Name
           MOVE "N" TO WS-VALID
           PERFORM UNTIL WS-VALID = "Y"
               MOVE "Enter Last Name:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               IF WS-EOF = "Y" EXIT PARAGRAPH END-IF
               PERFORM ECHOIN
               IF IN-REC = SPACES
                   MOVE "This field is required." TO WS-TEXT
                   PERFORM PRT
               ELSE
                   MOVE IN-REC(1:20) TO WS-LNAME
                   MOVE "Y" TO WS-VALID
               END-IF
           END-PERFORM

      *> Required field: University/College Attended
           MOVE "N" TO WS-VALID
           PERFORM UNTIL WS-VALID = "Y"
               MOVE "Enter University/College Attended:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               IF WS-EOF = "Y" EXIT PARAGRAPH END-IF
               PERFORM ECHOIN
               IF IN-REC = SPACES
                   MOVE "This field is required." TO WS-TEXT
                   PERFORM PRT
               ELSE
                   MOVE IN-REC(1:40) TO WS-UNIV
                   MOVE "Y" TO WS-VALID
               END-IF
           END-PERFORM

      *> Required field: Major
           MOVE "N" TO WS-VALID
           PERFORM UNTIL WS-VALID = "Y"
               MOVE "Enter Major:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               IF WS-EOF = "Y" EXIT PARAGRAPH END-IF
               PERFORM ECHOIN
               IF IN-REC = SPACES
                   MOVE "This field is required." TO WS-TEXT
                   PERFORM PRT
               ELSE
                   MOVE IN-REC(1:30) TO WS-MAJOR
                   MOVE "Y" TO WS-VALID
               END-IF
           END-PERFORM

      *> Required field: Graduation Year (YYYY numeric)
           PERFORM PROMPT-GRAD-YEAR

      *> Optional: About Me (blank allowed)
           MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):"
             TO WS-TEXT
           PERFORM PRT
           PERFORM READIN
           IF WS-EOF = "Y" EXIT PARAGRAPH END-IF
           PERFORM ECHOIN
           MOVE IN-REC(1:200) TO WS-ABOUT

      *> Optional lists
           PERFORM PROMPT-EXPERIENCE
           PERFORM PROMPT-EDUCATION

      *> Persist to Accounts.dat
           PERFORM PROFILE-SAVE-FOR-USER

           MOVE "Profile saved successfully!" TO WS-TEXT
           PERFORM PRT
           .

      *> ---------------------------------------------------------------
      *> PROMPT-GRAD-YEAR: ensures input is 4 digits and in range
      *> ---------------------------------------------------------------
       PROMPT-GRAD-YEAR.
           MOVE "N" TO WS-VALID
           PERFORM UNTIL WS-VALID = "Y"
               MOVE "Enter Graduation Year (YYYY):" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               IF WS-EOF = "Y" EXIT PARAGRAPH END-IF
               PERFORM ECHOIN

               MOVE IN-REC(1:20) TO WS-YEAR-TXT
               IF WS-YEAR-TXT(1:4) IS NUMERIC
                   MOVE WS-YEAR-TXT(1:4) TO WS-YEAR-NUM
                   IF WS-YEAR-NUM >= 1900 AND WS-YEAR-NUM <= 2100
                       MOVE WS-YEAR-NUM TO WS-GRAD-YEAR
                       MOVE "Y" TO WS-VALID
                   ELSE
                       MOVE "Invalid year range." TO WS-TEXT
                       PERFORM PRT
                   END-IF
               ELSE
                   MOVE "Graduation year must be a 4-digit number." TO WS-TEXT
                   PERFORM PRT
               END-IF
           END-PERFORM
           .

      *> ---------------------------------------------------------------
      *> PROMPT-EXPERIENCE: up to 3 entries; type DONE to stop
      *> ---------------------------------------------------------------
       PROMPT-EXPERIENCE.
           MOVE 0 TO WS-EXP-COUNT
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
               MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):"
                 TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               IF WS-EOF = "Y" EXIT PARAGRAPH END-IF
               PERFORM ECHOIN

               IF IN-REC(1:4) = "DONE"
                   EXIT PERFORM
               END-IF

               ADD 1 TO WS-EXP-COUNT

               MOVE "Experience - Title:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               PERFORM ECHOIN
               MOVE IN-REC(1:30) TO WS-EXP-TITLE(WS-EXP-COUNT)

               MOVE "Experience - Company/Organization:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               PERFORM ECHOIN
               MOVE IN-REC(1:40) TO WS-EXP-COMPANY(WS-EXP-COUNT)

               MOVE "Experience - Dates (e.g., Summer 2024):" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               PERFORM ECHOIN
               MOVE IN-REC(1:20) TO WS-EXP-DATES(WS-EXP-COUNT)

               MOVE "Experience - Description (optional, max 100 chars, blank to skip):"
                 TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               PERFORM ECHOIN
               MOVE IN-REC(1:100) TO WS-EXP-DESC(WS-EXP-COUNT)
           END-PERFORM
           .

      *> ---------------------------------------------------------------
      *> PROMPT-EDUCATION: up to 3 entries; type DONE to stop
      *> ---------------------------------------------------------------
       PROMPT-EDUCATION.
           MOVE 0 TO WS-EDU-COUNT
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
               MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):"
                 TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               IF WS-EOF = "Y" EXIT PARAGRAPH END-IF
               PERFORM ECHOIN

               IF IN-REC(1:4) = "DONE"
                   EXIT PERFORM
               END-IF

               ADD 1 TO WS-EDU-COUNT

               MOVE "Education - Degree:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               PERFORM ECHOIN
               MOVE IN-REC(1:30) TO WS-EDU-DEGREE(WS-EDU-COUNT)

               MOVE "Education - University/College:" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               PERFORM ECHOIN
               MOVE IN-REC(1:40) TO WS-EDU-SCHOOL(WS-EDU-COUNT)

               MOVE "Education - Years Attended (e.g., 2023-2025):" TO WS-TEXT
               PERFORM PRT
               PERFORM READIN
               PERFORM ECHOIN
               MOVE IN-REC(1:15) TO WS-EDU-YEARS(WS-EDU-COUNT)
           END-PERFORM
           .

      *> ---------------------------------------------------------------
      *> PROFILE-VIEW: prints the current user's profile (loads from file)
      *> ---------------------------------------------------------------
       PROFILE-VIEW.
           PERFORM PROFILE-LOAD-FOR-USER

           MOVE "--- Your Profile ---" TO WS-TEXT
           PERFORM PRT

           MOVE SPACES TO WS-TEXT
           STRING "Name: " DELIMITED BY SIZE
                  WS-FNAME DELIMITED BY SPACE
                  " "      DELIMITED BY SIZE
                  WS-LNAME DELIMITED BY SPACE
                  INTO WS-TEXT
           END-STRING
           PERFORM PRT

           MOVE SPACES TO WS-TEXT
           STRING "University: " DELIMITED BY SIZE
                  WS-UNIV       DELIMITED BY SIZE
                  INTO WS-TEXT
           END-STRING
           PERFORM PRT

           MOVE SPACES TO WS-TEXT
           STRING "Major: " DELIMITED BY SIZE
                  WS-MAJOR  DELIMITED BY SIZE
                  INTO WS-TEXT
           END-STRING
           PERFORM PRT

           MOVE SPACES TO WS-TEXT
           STRING "Graduation Year: " DELIMITED BY SIZE
                  WS-GRAD-YEAR      DELIMITED BY SIZE
                  INTO WS-TEXT
           END-STRING
           PERFORM PRT

           MOVE SPACES TO WS-TEXT
           STRING "About Me: " DELIMITED BY SIZE
                  WS-ABOUT    DELIMITED BY SIZE
                  INTO WS-TEXT
           END-STRING
           PERFORM PRT


           IF WS-EXP-COUNT = 0
               MOVE "Experience: None" TO WS-TEXT
               PERFORM PRT
           ELSE
               MOVE "Experience:" TO WS-TEXT
               PERFORM PRT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-EXP-COUNT
                   MOVE SPACES TO WS-TEXT
                   STRING " Title: " DELIMITED BY SIZE
                          WS-EXP-TITLE(J) DELIMITED BY SIZE
                          INTO WS-TEXT
                   END-STRING
                   PERFORM PRT

                   MOVE SPACES TO WS-TEXT
                   STRING " Company: " DELIMITED BY SIZE
                          WS-EXP-COMPANY(J) DELIMITED BY SIZE
                          INTO WS-TEXT
                   END-STRING
                   PERFORM PRT

                   MOVE SPACES TO WS-TEXT
                   STRING " Dates: " DELIMITED BY SIZE
                          WS-EXP-DATES(J) DELIMITED BY SIZE
                          INTO WS-TEXT
                   END-STRING
                   PERFORM PRT

                   MOVE SPACES TO WS-TEXT
                   STRING " Description: " DELIMITED BY SIZE
                          WS-EXP-DESC(J)   DELIMITED BY SIZE
                          INTO WS-TEXT
                   END-STRING
                   PERFORM PRT
               END-PERFORM
           END-IF


           IF WS-EDU-COUNT = 0
               MOVE "Education: None" TO WS-TEXT
               PERFORM PRT
           ELSE
               MOVE "Education:" TO WS-TEXT
               PERFORM PRT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-EDU-COUNT
                   MOVE SPACES TO WS-TEXT
                   STRING " Degree: " DELIMITED BY SIZE
                          WS-EDU-DEGREE(J) DELIMITED BY SIZE
                          INTO WS-TEXT
                   END-STRING
                   PERFORM PRT

                   MOVE SPACES TO WS-TEXT
                   STRING " University: " DELIMITED BY SIZE
                          WS-EDU-SCHOOL(J) DELIMITED BY SIZE
                          INTO WS-TEXT
                   END-STRING
                   PERFORM PRT

                   MOVE SPACES TO WS-TEXT
                   STRING " Years: " DELIMITED BY SIZE
                          WS-EDU-YEARS(J) DELIMITED BY SIZE
                          INTO WS-TEXT
                   END-STRING
                   PERFORM PRT
               END-PERFORM
           END-IF

           MOVE "--------------------" TO WS-TEXT
           PERFORM PRT
           .

      *> ---------------------------------------------------------------
      *> PROFILE-LOAD-FOR-USER: loads profile data for WS-UIN from file
      *> ---------------------------------------------------------------
       PROFILE-LOAD-FOR-USER.
      *> Reset defaults
           MOVE SPACES TO WS-FNAME WS-LNAME WS-UNIV WS-MAJOR WS-ABOUT
           MOVE 0 TO WS-GRAD-YEAR
           MOVE 0 TO WS-EXP-COUNT
           MOVE 0 TO WS-EDU-COUNT

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
               MOVE SPACES TO WS-EXP-TITLE(J) WS-EXP-COMPANY(J)
                              WS-EXP-DATES(J) WS-EXP-DESC(J)
               MOVE SPACES TO WS-EDU-DEGREE(J) WS-EDU-SCHOOL(J)
                              WS-EDU-YEARS(J)
           END-PERFORM

           OPEN INPUT ACCT-FILE
           PERFORM UNTIL 1 = 2
               READ ACCT-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF ACCT-USER = WS-UIN
                           MOVE ACCT-FNAME TO WS-FNAME
                           MOVE ACCT-LNAME TO WS-LNAME
                           MOVE ACCT-UNIV  TO WS-UNIV
                           MOVE ACCT-MAJOR TO WS-MAJOR
                           MOVE ACCT-GRAD-YEAR TO WS-GRAD-YEAR
                           MOVE ACCT-ABOUT TO WS-ABOUT

                           MOVE ACCT-EXP-COUNT TO WS-EXP-COUNT
                           MOVE ACCT-EDU-COUNT TO WS-EDU-COUNT

                           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                               MOVE ACCT-EXP-TITLE(J) TO WS-EXP-TITLE(J)
                               MOVE ACCT-EXP-COMP(J)  TO WS-EXP-COMPANY(J)
                               MOVE ACCT-EXP-DATES(J) TO WS-EXP-DATES(J)
                               MOVE ACCT-EXP-DESC(J)  TO WS-EXP-DESC(J)

                               MOVE ACCT-EDU-DEG(J)   TO WS-EDU-DEGREE(J)
                               MOVE ACCT-EDU-SCH(J)   TO WS-EDU-SCHOOL(J)
                               MOVE ACCT-EDU-YEARS(J) TO WS-EDU-YEARS(J)
                           END-PERFORM
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCT-FILE
           .

      *> ---------------------------------------------------------------
      *> PROFILE-SAVE-FOR-USER: rewrites Accounts.dat by copying to temp
      *> and replacing the logged-in user's record with updated fields.
      *> ---------------------------------------------------------------
       PROFILE-SAVE-FOR-USER.
           OPEN INPUT  ACCT-FILE
           OPEN OUTPUT ACCT-TMP

           PERFORM UNTIL 1 = 2
               READ ACCT-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF ACCT-USER = WS-UIN
      *> Replace this record with updated profile (and keep credentials)
                           MOVE WS-UIN TO ACCT-USER
                           MOVE WS-PIN TO ACCT-PASS

                           MOVE WS-FNAME TO ACCT-FNAME
                           MOVE WS-LNAME TO ACCT-LNAME
                           MOVE WS-UNIV  TO ACCT-UNIV
                           MOVE WS-MAJOR TO ACCT-MAJOR
                           MOVE WS-GRAD-YEAR TO ACCT-GRAD-YEAR
                           MOVE WS-ABOUT TO ACCT-ABOUT

                           MOVE WS-EXP-COUNT TO ACCT-EXP-COUNT
                           MOVE WS-EDU-COUNT TO ACCT-EDU-COUNT

                           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                               MOVE WS-EXP-TITLE(J)   TO ACCT-EXP-TITLE(J)
                               MOVE WS-EXP-COMPANY(J) TO ACCT-EXP-COMP(J)
                               MOVE WS-EXP-DATES(J)   TO ACCT-EXP-DATES(J)
                               MOVE WS-EXP-DESC(J)    TO ACCT-EXP-DESC(J)

                               MOVE WS-EDU-DEGREE(J)  TO ACCT-EDU-DEG(J)
                               MOVE WS-EDU-SCHOOL(J)  TO ACCT-EDU-SCH(J)
                               MOVE WS-EDU-YEARS(J)   TO ACCT-EDU-YEARS(J)
                           END-PERFORM
                       END-IF

      *> Write the (possibly updated) record to temp
                       WRITE ACCT-TMP-REC FROM ACCT-REC
               END-READ
           END-PERFORM

           CLOSE ACCT-FILE
           CLOSE ACCT-TMP

      *> Swap temp into Accounts.dat (works in typical Linux environments)
           MOVE "rm -f Accounts.dat" TO WS-CMD
           CALL "SYSTEM" USING WS-CMD

           MOVE "mv Accounts.tmp Accounts.dat" TO WS-CMD
           CALL "SYSTEM" USING WS-CMD
           .

      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-HEYBOB                PIC X(60) VALUES SPACES.
       01 WS-RESULT                PIC X(40) VALUES SPACES.
       01 WS-HEYBOB-REV            PIC X(60) VALUES SPACES.
       01 CONTADOR                 PIC 9(2).
       01 WS-SPACE-COUNT           PIC 9(2) VALUE 0.
       01 THEQUESTION              PIC 9.
         88 ISQUESTION   VALUE 1.
         88 NOTQUESTION  VALUE 0.
       01 THEUPPERCASE             PIC 99.
       01 THELOWERCASE             PIC 99.

       PROCEDURE DIVISION.


       BOB.
       INITIALIZE CONTADOR
                  THEQUESTION
                  THEUPPERCASE
                  THELOWERCASE.

       PERFORM VARYING CONTADOR FROM 1 BY 1 UNTIL CONTADOR > 60
       MOVE WS-HEYBOB(CONTADOR:1) TO WS-HEYBOB-REV((61 - CONTADOR):1)
       END-PERFORM.

       INSPECT WS-HEYBOB-REV TALLYING WS-SPACE-COUNT FOR LEADING SPACE.
       MOVE FUNCTION TRIM(WS-HEYBOB-REV) TO WS-HEYBOB-REV.

       IF WS-HEYBOB-REV(1:1) = '?' THEN
         MOVE 1 TO THEQUESTION
       END-IF.

       INITIALIZE THEUPPERCASE
                  THELOWERCASE.


       PERFORM VARYING CONTADOR FROM 1 BY 1 UNTIL CONTADOR > 60
           IF WS-HEYBOB-REV(CONTADOR:1) >= 'A' AND
              WS-HEYBOB-REV(CONTADOR:1) <= 'Z' THEN
              ADD 1 TO THEUPPERCASE
           ELSE
             IF WS-HEYBOB-REV(CONTADOR:1) >= 'a' AND
                WS-HEYBOB-REV(CONTADOR:1) <= 'z' THEN
                ADD 1 TO THELOWERCASE
             END-IF
           END-IF
         END-PERFORM.

         IF ISQUESTION AND THELOWERCASE = 0 AND THEUPPERCASE > 0 THEN
           MOVE "Calm down, I know what I'm doing!" TO WS-RESULT
         ELSE
           IF ISQUESTION THEN
              MOVE "Sure." TO WS-RESULT
           ELSE
             IF THEUPPERCASE > 0 AND THELOWERCASE = 0 THEN
                MOVE "Whoa, chill out!" TO WS-RESULT
             ELSE
               IF LENGTH OF FUNCTION TRIM(WS-HEYBOB-REV) = 0
                 MOVE "Fine. Be that way!" TO WS-RESULT
               ELSE
                 MOVE "Whatever." TO WS-RESULT
               END-IF
             END-IF
           END-IF
         END-IF.

       BOB-END.

       END-OF-PROGRAM.
            STOP RUN.
       END PROGRAM BOB.

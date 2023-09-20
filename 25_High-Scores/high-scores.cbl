       IDENTIFICATION DIVISION.
       PROGRAM-ID. high-scores.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PROPERTY       PIC A(20).
       01 WS-SCORES.
           02 SCORES        PIC X(3) OCCURS 20 TIMES
                            INDEXED BY IDX.

       01 WS-RESULT-STRING  PIC X(60).
       01 WS-RESULT-VALUE   PIC 999.

       01 CONTADOR PIC 99.
       01 TEMP PIC ZZ9.
       01 TEMP2 PIC 999.
       01 LATEST PIC 999.
       01 PERSONALBEST PIC 999.
       01 PERSONALTHREE.
           05 THETOP PIC ZZZ OCCURS 3 TIMES.

       PROCEDURE DIVISION.

       HIGH-SCORES.

       INITIALIZE TEMP CONTADOR LATEST PERSONALBEST PERSONALTHREE
                  WS-RESULT-STRING WS-RESULT-VALUE.

       PERFORM VARYING CONTADOR FROM 1 BY 1 UNTIL CONTADOR > 20
         EVALUATE TEMP
           WHEN > THETOP(1)
             MOVE THETOP(2) TO THETOP(3)
             MOVE THETOP(1) TO THETOP(2)
             MOVE TEMP      TO THETOP(1)
           WHEN > THETOP(2)
             MOVE THETOP(2) TO THETOP(3)
             MOVE TEMP      TO THETOP(2)
           WHEN > THETOP(3)
             MOVE TEMP      TO THETOP(3)
         END-EVALUATE

         MOVE SCORES(CONTADOR) TO TEMP
         MOVE TEMP TO TEMP2
         IF TEMP2 > 0 THEN MOVE TEMP2 TO LATEST END-IF
       END-PERFORM.
       MOVE THETOP(1) TO PERSONALBEST.

       EVALUATE WS-PROPERTY
         WHEN "scores"
           STRING WS-SCORES DELIMITED BY SIZE INTO WS-RESULT-STRING
         WHEN "latest"
           MOVE LATEST TO WS-RESULT-VALUE
         WHEN "personalBest"
           MOVE PERSONALBEST TO WS-RESULT-VALUE
         WHEN "personalTopThree"
           MOVE PERSONALTHREE TO WS-RESULT-STRING
       END-EVALUATE.
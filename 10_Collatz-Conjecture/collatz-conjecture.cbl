      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. collatz-conjecture.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER PIC S9(8).
       01 WS-STEPS PIC 9(4).
       01 WS-ERROR PIC X(35).

       PROCEDURE DIVISION.

       COLLATZ-CONJECTURE.

       INITIALIZE WS-STEPS
                  WS-ERROR.

       IF WS-NUMBER <= 0 THEN
         MOVE "Only positive integers are allowed" TO WS-ERROR
       ELSE
         PERFORM UNTIL WS-NUMBER = 1
           IF FUNCTION MOD(WS-NUMBER, 2) NOT = 0
             COMPUTE WS-NUMBER = (WS-NUMBER * 3) + 1
             ADD 1 TO WS-STEPS
           ELSE
             COMPUTE WS-NUMBER = WS-NUMBER /2
             ADD 1 TO WS-STEPS
           END-IF
         END-PERFORM
       END-IF.

       DISPLAY WS-STEPS.
       DISPLAY WS-ERROR.

       COLLATZ-CONJECTURE-END.



       END-OF-PROGRAM.
            STOP RUN.
       END PROGRAM collatz-conjecture.

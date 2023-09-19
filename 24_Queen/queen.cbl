       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUEEN-ATTACK.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Inputs
       01 WS-QUEEN PIC X(9).
       01 WS-WHITE_QUEEN PIC X(9).
       01 WS-BLACK_QUEEN PIC X(9).
       01 WS-PROPERTY PIC X(11).
       01 TEMP PIC S9 OCCURS 4 TIMES.
       01 DIAGONALX PIC 9.
       01 DIAGONALY PIC 9.
      *Outputs
       01 WS-RESULT PIC 9.

       PROCEDURE DIVISION.

       QUEEN-ATTACK.

       INITIALIZE WS-RESULT.

       IF WS-PROPERTY = "create" THEN
         UNSTRING WS-QUEEN DELIMITED BY "," INTO TEMP(1), TEMP(2)
         IF TEMP(1) >= 0 AND TEMP(1) <=7 AND TEMP(2) >= 0
           AND TEMP(2) <=7 THEN MOVE 1 TO WS-RESULT END-IF
       END-IF.

       IF WS-PROPERTY = "canAttack" THEN
         UNSTRING WS-WHITE_QUEEN DELIMITED BY "," INTO TEMP(1), TEMP(2)
         UNSTRING WS-BLACK_QUEEN DELIMITED BY "," INTO TEMP(3), TEMP(4)

         COMPUTE DIAGONALX = FUNCTION ABS(TEMP(1) - TEMP(3))
         COMPUTE DIAGONALY = FUNCTION ABS(TEMP(2) - TEMP(4))

         IF TEMP(1) = TEMP(3) OR TEMP(2) = TEMP(4) THEN
           MOVE 1 TO WS-RESULT
         ELSE IF DIAGONALX = DIAGONALY THEN
           MOVE 1 TO WS-RESULT
       END-IF.
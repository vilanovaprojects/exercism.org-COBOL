      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAP.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-YEAR PIC 9(4).
       01 RESTO4 PIC 9(4).
       01 RESTO100 PIC 9(4).
       01 RESTO400 PIC 9(4).
       01 WS-RESULT PIC 9.
       PROCEDURE DIVISION.


       LEAP.
      * Enter solution here
       DIVIDE WS-YEAR BY 4 GIVING RESTO4 REMAINDER RESTO4.
       DIVIDE WS-YEAR BY 100 GIVING RESTO100 REMAINDER RESTO100.
       DIVIDE WS-YEAR BY 400 GIVING RESTO400 REMAINDER RESTO400.

       IF (RESTO4 = 0 AND RESTO100 = 0 AND RESTO400 = 0) THEN
           MOVE 1 TO WS-RESULT
       ELSE
         IF (RESTO4 = 0 AND RESTO100 NOT = 0) THEN
           MOVE 1 TO WS-RESULT
         ELSE
           MOVE 0 TO WS-RESULT
         END-IF
       END-IF.
       LEAP-EXIT.
         EXIT.

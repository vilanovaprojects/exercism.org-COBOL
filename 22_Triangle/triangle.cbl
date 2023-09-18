       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIANGLE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Incoming
       01 WS-SIDES PIC X(20).
       01 WS-PROPERTY PIC X(11).
       01 STRINGS.
         05 STRING-A PIC X(3).
         05 STRING-B PIC X(3).
         05 STRING-C PIC X(3).

       01 A PIC 9(2)V99.
       01 B PIC 9(2)V99.
       01 C PIC 9(2)V99.

      *Outgoing
       01 WS-RESULT PIC 9.
       PROCEDURE DIVISION.


       TRIANGLE.

       INITIALIZE WS-RESULT STRINGS A B C.

       UNSTRING WS-SIDES DELIMITED BY "," INTO STRING-A
                                               STRING-B
                                               STRING-C.
       MOVE STRING-A TO A.
       MOVE STRING-B TO B.
       MOVE STRING-C TO C.

       IF A + B >= C AND B + C >= A AND A + C >= B
         AND A + B + C > 0 THEN
         EVALUATE WS-PROPERTY
           WHEN "equilateral"
             IF A = B AND B = C THEN MOVE 1 TO WS-RESULT
           WHEN "isosceles"
             IF A = B OR B = C OR A = C THEN MOVE 1 TO WS-RESULT
           WHEN "scalene"
             IF A NOT = B AND B NOT = C AND A NOT = C THEN
               MOVE 1 TO WS-RESULT
         END-EVALUATE
       END-IF.

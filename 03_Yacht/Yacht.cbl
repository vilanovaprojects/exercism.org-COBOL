      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT-PROGRAM.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT PIC 99 VALUE 0.
       01 WS-CATEGORY PIC X(15).
       01 WS-DICE PIC 9(5).
       01 WS-DICE-ARRAY REDEFINES WS-DICE.
         05 WS-DICE-ELEMENT PIC 9 OCCURS 5 TIMES.
       01 Counter PIC 9.
       01 checknumber PIC 9.
       01 checknumber2 PIC 9.
       01 fullhouseresult PIC 99.
       01 fourakindresult PIC 99.
       01 stairsvar1 PIC 9.
       01 stairsvar2 PIC 9.
       01 stairscount PIC 99.
       01 stairslong PIC 99.
         88 Littlestairs value 15.
         88 Bigstairs value 20.


       PROCEDURE DIVISION.

       INITIALIZE WS-RESULT
                  Counter.


       YACHT.
         EVALUATE WS-CATEGORY
           WHEN "ones"
             MOVE 1 TO checknumber
             PERFORM NUMBERS12345
             PERFORM MULTIPLER
             PERFORM END-OF-PROGRAM
           WHEN "twos"
             MOVE 2 TO checknumber
             PERFORM NUMBERS12345
             PERFORM MULTIPLER
             PERFORM END-OF-PROGRAM
           WHEN "threes"
             MOVE 3 TO checknumber
             PERFORM NUMBERS12345
             PERFORM MULTIPLER
             PERFORM END-OF-PROGRAM
           WHEN "fours"
             MOVE 4 TO checknumber
             PERFORM NUMBERS12345
             PERFORM MULTIPLER
             PERFORM END-OF-PROGRAM
           WHEN "fives"
             MOVE 5 TO checknumber
             PERFORM NUMBERS12345
             PERFORM MULTIPLER
             PERFORM END-OF-PROGRAM
           WHEN "sixes"
             MOVE 6 TO checknumber
             PERFORM NUMBERS12345
             PERFORM MULTIPLER
             PERFORM END-OF-PROGRAM
           WHEN "full house"
             PERFORM FULLHOUSE
             PERFORM NUMBERS12345
              IF WS-RESULT > 3 THEN PERFORM ZEROANDEND END-IF
             COMPUTE fullhouseresult = WS-RESULT * checknumber
             MOVE checknumber2 to checknumber
             PERFORM NUMBERS12345
              IF WS-RESULT > 3 THEN PERFORM ZEROANDEND END-IF
             COMPUTE fullhouseresult = fullhouseresult +
                     (WS-RESULT * checknumber)
             DISPLAY fullhouseresult
             PERFORM END-OF-PROGRAM

           WHEN "four of a kind"
             PERFORM FOURAKIND
             DISPLAY fourakindresult
             PERFORM END-OF-PROGRAM

           WHEN "little straight"
             MOVE 15 TO stairslong
             PERFORM STAIRS
             MOVE 30 TO WS-RESULT
             DISPLAY WS-RESULT
             PERFORM END-OF-PROGRAM

           WHEN "big straight"
             MOVE 20 TO stairslong
             PERFORM STAIRS
             MOVE 30 TO WS-RESULT
             DISPLAY WS-RESULT
             PERFORM END-OF-PROGRAM

           WHEN "choice"
             PERFORM CHOICE
             DISPLAY WS-RESULT
             PERFORM END-OF-PROGRAM

           WHEN "yacht"
             PERFORM YACHTSET
             MOVE 50 to WS-RESULT
             DISPLAY WS-RESULT
             PERFORM END-OF-PROGRAM


           WHEN OTHER
             PERFORM CATEGORY-ERROR
         END-EVALUATE.
       YACHT-EMD.



      ******************************************************
      *-------------------NUMBERS-------------------
      ******************************************************
       NUMBERS12345.
         MOVE ZEROES to WS-RESULT.
         PERFORM VARYING Counter FROM 1 BY 1 UNTIL Counter > 5
           IF WS-DICE-ELEMENT(Counter) = checknumber
             ADD 1 TO WS-RESULT
           END-IF
         END-PERFORM.
       NUMBERS12345-END.


       MULTIPLER.
         MULTIPLY WS-RESULT by checknumber.
         DISPLAY WS-RESULT.
         DISPLAY checknumber.
       MULTIPLER-END.

      ******************************************************
      *-------------------FULLHOUSE-------------------
      ******************************************************
       FULLHOUSE.
         MOVE WS-DICE-ELEMENT(1) TO checknumber
         PERFORM VARYING Counter FROM 1 BY 1 UNTIL Counter > 5
           IF WS-DICE-ELEMENT(Counter) NOT EQUAL checknumber THEN
             MOVE WS-DICE-ELEMENT(Counter) TO checknumber2
             PERFORM VARYING Counter FROM Counter BY 1 UNTIL Counter > 5
               IF WS-DICE-ELEMENT(Counter) NOT EQUAL checknumber
                AND WS-DICE-ELEMENT(Counter) NOT EQUAL checknumber2 THEN
                 PERFORM ZEROANDEND
               END-IF
             END-PERFORM
           END-IF
         END-PERFORM.
       FULLHOUSE-END.

      ******************************************************
      *-------------------FOURAKIND-------------------
      ******************************************************
       FOURAKIND.
         PERFORM FULLHOUSE.
         PERFORM NUMBERS12345.
         IF WS-RESULT EQUAL 4
           THEN Compute fourakindresult = WS-RESULT * checknumber
         ELSE
           MOVE checknumber2 to checknumber
           PERFORM NUMBERS12345
           IF WS-RESULT EQUAL 4
             THEN Compute fourakindresult = WS-RESULT * checknumber
           ELSE
             PERFORM ZEROANDEND
           END-IF
         END-IF.
       FOURAKIND-END.



      ******************************************************
      *-------------------STAIRS-------------------
      ******************************************************
       STAIRS.
         PERFORM VARYING Counter FROM 1 BY 1 UNTIL Counter > 5

           IF WS-DICE-ELEMENT(Counter) EQUAL 2 THEN
             ADD 1 TO stairsvar1
           END-IF

           IF WS-DICE-ELEMENT(Counter) EQUAL 3 THEN
             ADD 1 TO stairsvar2
           END-IF

           IF WS-DICE-ELEMENT(Counter) EQUALS checknumber THEN
             PERFORM ZEROANDEND
           END-IF
           MOVE WS-DICE-ELEMENT(Counter) TO checknumber
           ADD WS-DICE-ELEMENT(Counter) TO stairscount
         END-PERFORM.
         IF stairscount not EQUAL stairslong OR stairsvar1 not EQUAL 1
            OR stairsvar2 not EQUAL 1 THEN
           PERFORM ZEROANDEND
         END-IF.
       STAIRS-END.



      ******************************************************
      *-------------------CHOICE-------------------
      ******************************************************
       CHOICE.
         PERFORM VARYING Counter FROM 1 BY 1 UNTIL Counter > 5
           ADD WS-DICE-ELEMENT(Counter) TO WS-RESULT
         END-PERFORM.
       CHOICE-END.


      ******************************************************
      *-------------------YACHTSET-------------------
      ******************************************************
       YACHTSET.
         MOVE WS-DICE-ELEMENT(1) TO checknumber.
         PERFORM VARYING Counter FROM 2 BY 1 UNTIL Counter > 5
           IF WS-DICE-ELEMENT(Counter) NOT EQUALS checknumber THEN
             PERFORM ZEROANDEND
           END-IF
           MOVE WS-DICE-ELEMENT(Counter) TO checknumber
         END-PERFORM.
       YACHTSET-END.





      ******************************************************
      *-------------------ERROR AND CLOSE-------------------
      ******************************************************
       CATEGORY-ERROR.
         DISPLAY "ERROR".
         PERFORM ZEROANDEND.

       ZEROANDEND.
         MOVE ZERO TO WS-RESULT.
         DISPLAY WS-RESULT.
         PERFORM END-OF-PROGRAM.

       END-OF-PROGRAM.
            STOP RUN.
       END PROGRAM YACHT-PROGRAM.

      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. nucleotide-count.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DNA PIC X(128).
       01 WS-A PIC 9(4).
       01 WS-C PIC 9(4).
       01 WS-G PIC 9(4).
       01 WS-T PIC 9(4).
       01 WS-ERROR PIC X(36).
       01 I PIC 9(3).

       PROCEDURE DIVISION.
       NUCLEOTIDE-COUNT.
       INITIALIZE WS-ERROR
                  WS-A
                  WS-C
                  WS-G
                  WS-T.
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF WS-DNA
         EVALUATE WS-DNA(I:1)
           WHEN "A"
             ADD 1 TO WS-A
           WHEN "C"
             ADD 1 TO WS-C
           WHEN "G"
             ADD 1 TO WS-G
           WHEN "T"
             ADD 1 TO WS-T
           WHEN OTHER
             MOVE "ERROR: Invalid nucleotide in strand" TO WS-ERROR
         END-EVALUATE
       END-PERFORM.

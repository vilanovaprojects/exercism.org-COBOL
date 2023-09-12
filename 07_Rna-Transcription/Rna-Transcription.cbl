      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rna-transcription.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COMPLEMENT PIC X(64).
       01 WS-DNA PIC X(4) VALUE "ACGT".
       01 COUNTER PIC 99.

       PROCEDURE DIVISION.


       RNA-TRANSCRIPTION.
         PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 64
           EVALUATE WS-COMPLEMENT(COUNTER:1)
             WHEN "G"
               MOVE "C" TO WS-COMPLEMENT(COUNTER:1)
             WHEN "C"
               MOVE "G" TO WS-COMPLEMENT(COUNTER:1)
             WHEN "T"
               MOVE "A" TO WS-COMPLEMENT(COUNTER:1)
             WHEN "A"
               MOVE "U" TO WS-COMPLEMENT(COUNTER:1)
             WHEN " "
               MOVE " " TO WS-COMPLEMENT(COUNTER:1)
             WHEN OTHER
               MOVE "*" TO WS-COMPLEMENT(COUNTER:1)
           END-EVALUATE
         END-PERFORM.
       RNA-TRANSCRIPTION-END.


       END-OF-PROGRAM.
            STOP RUN.
       END PROGRAM rna-transcription.

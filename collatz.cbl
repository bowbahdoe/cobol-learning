       IDENTIFICATION DIVISION.
       PROGRAM-ID. COLLATZ.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  N PICTURE 9999.
       77  DIV-BY-2 PICTURE 9999.
       77  MODULUS PICTURE 9.
       77  STEPS-TAKEN PICTURE 9999.

       LINKAGE SECTION.
       77  N-PARAM PICTURE 9999.
       77  STEPS-NEEDED-OUT PICTURE 999.

       PROCEDURE DIVISION USING N-PARAM, STEPS-NEEDED-OUT.
           MOVE N-PARAM TO N.
           SET STEPS-NEEDED-OUT TO 0.

           PERFORM UNTIL N <= 1
               DISPLAY N
               DIVIDE N BY 2 GIVING DIV-BY-2 REMAINDER MODULUS
               IF MODULUS = 0 THEN
                   MOVE DIV-BY-2 TO N
               ELSE
                   COMPUTE N = (N * 3) + 1
               END-IF
               ADD 1 TO STEPS-NEEDED-OUT
           END-PERFORM.

           DISPLAY N.
       END PROGRAM COLLATZ.

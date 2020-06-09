       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQRT.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  N PIC 9(10).
       77  GUESS PIC 9(5)V9(5).
       77  HIGH PIC 9(5)V9(5).
       77  LOW PIC 9(5)V9(5).
       77  HIGH-LOW-SWAP PIC 9(5)V9(5).
       77  EPSILON PIC 9(5)V9(5) VALUE 0.01.
       77  ERROR-AMT PIC 9(5)V9(5).
       77  ERROR-KIND PIC X.
           88 TOO-HIGH VALUE 'H'.
           88 TOO-LOW VALUE 'L'.

       LINKAGE SECTION.
       77  N-PARAM PICTURE 9(5).
       77  SQRT-OUT PICTURE 9(5)V9(5).

       PROCEDURE DIVISION USING N-PARAM, SQRT-OUT.
           MOVE N-PARAM TO N.
           GO TO BEGIN.

           COMPUTE-ERROR.
           IF (GUESS * GUESS) > N THEN
               COMPUTE ERROR-AMT = (GUESS * GUESS) - N
               SET TOO-HIGH TO TRUE
           ELSE
               COMPUTE ERROR-AMT = N - (GUESS * GUESS)
               SET TOO-LOW TO TRUE
           END-IF.

           SHOW-INFO.
               PERFORM
                   DISPLAY LOW, ' LOW'
                   DISPLAY HIGH, ' HIGH'
                   DISPLAY GUESS, ' GUESS'
                   DISPLAY N, ' N'
                   DISPLAY ERROR-AMT, ' ERROR-AMT'
                   DISPLAY EPSILON, 'EPSILON'
               END-PERFORM.

           BEGIN.
           SET HIGH TO N.
           SET LOW TO 0.
           COMPUTE GUESS = N / 2.

           PERFORM COMPUTE-ERROR.
           PERFORM SHOW-INFO.
           PERFORM UNTIL ERROR-AMT < EPSILON
               IF TOO-HIGH
                   SET HIGH-LOW-SWAP TO GUESS
                   COMPUTE GUESS = (LOW + GUESS) / 2
                   SET HIGH TO HIGH-LOW-SWAP
               ELSE IF TOO-LOW
                   SET HIGH-LOW-SWAP TO GUESS
                   COMPUTE GUESS = (HIGH + GUESS) / 2
                   SET LOW TO HIGH-LOW-SWAP
               END-IF
               PERFORM COMPUTE-ERROR
               PERFORM SHOW-INFO
           END-PERFORM.
           PERFORM COMPUTE-ERROR.
           PERFORM SHOW-INFO.

           MOVE GUESS TO SQRT-OUT.
       END PROGRAM SQRT.

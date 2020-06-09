       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOTTLES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  BOTTLES-ON-WALL PIC 99.

       PROCEDURE DIVISION.
           PERFORM BEGIN.
       BEGIN.
           PERFORM VARYING BOTTLES-ON-WALL
                   FROM 99 BY -1
                   UNTIL BOTTLES-ON-WALL IS ZERO
               DISPLAY BOTTLES-ON-WALL, ' Bottles of beer on the wall'
               DISPLAY BOTTLES-ON-WALL, ' Bottles of beer.'
               DISPLAY 'Take one down pass it around'
           END-PERFORM.
           DISPLAY 'No more bottles of beer on the wall'.
       END PROGRAM BOTTLES.

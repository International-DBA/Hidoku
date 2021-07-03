       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.    HIDOKU.
      *
      *    (KING'S TOUR PUZZLE.)
      *
       AUTHOR.        ANDREW S REID.
      *
       DATE-WRITTEN.  MAY 2021.
      *
      ******************************************************************
      * VERSION * DATE     * COMMENTS                                  *
      ******************************************************************
      *       1 * MAY 2021 * ORIGINAL VERSION CREATED FROM EARLIER C   *
      *                      PROGRAM.                                  *
      *                      THIS ONLY CREATES A SOLUTION.             *
      ******************************************************************
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SOURCE-COMPUTER. BUILT-AT-HOME.
      *
       OBJECT-COMPUTER. BUILT-AT-HOME.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
      /
       DATA DIVISION.
      *
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
      *
       01  INPUT-PARAMETERS.
           05 RANDOM-NUMBER-SEED PIC 9999.
           05 COMMA1             PIC X.
           05 SIZE1-INPUT        PIC 99.
           05 COMMA2             PIC X.
           05 START-COLUMN-INPUT PIC 99.
           05 COMMA3             PIC X.
           05 START-ROW-INPUT    PIC 99.     
           05 COMMA4             PIC X.
           05 END-COLUMN-INPUT   PIC 99.
           05 COMMA5             PIC X.
           05 END-ROW-INPUT      PIC 99.
      *
      *    THE RANDOM-MOVES ARRAY IS USED DURING SOLUTION CREATION.
      *    IT DETERMINES WHICH ORDER MOVES ARE SELECTED AT EACH LEVEL.
      *
       01  FILLER1.
           05 FILLER2 OCCURS 144.
               10 RANDOM-MOVES PIC 9(6) OCCURS 8.
      *
      *    THE BOARD ARRAY STORES THE KING'S PROGRESS AROUND THE BOARD.
      *
       01  FILLER3.
           05 FILLER4 OCCURS 12.
               10 BOARD PIC 9(3) OCCURS 12.
      *
      *    POSITION1 WAS CALLED POSITION IN ORIGINAL C PROGRAM BUT THIS
      *    IS A RESERVED WORD IN COBOL:
      *
       01  FILLER5.
           05 FILLER6 OCCURS 144.
               10 POSITION1 PIC 9(6) OCCURS 3.
      *
      *    ARRAY TO CHECK THAT THERE IS ONLY 1 CONTIGUOUS SET OF EMPTY
      *    SQUARES:
      *
       01  FILLER7.
           05 FILLER8 OCCURS 12.
               10 CONNECTIVITY-CHECK PIC 9(6) OCCURS 12.
      *
       77  A PIC 9(6).
       77  ACCESSIBILITY PIC 9(6).
       77  B PIC 9(6).
       77  CONTIGUOUS-SQUARES-FOUND PIC 9(6).
       77  DUMMY1 PIC X.
       77  END-COLUMN PIC 9(6).
       77  END-ROW PIC 9(6).
       77  FINISHED PIC 9(6).
       77  I PIC 9(6).
       77  INACCESSIBLE-SQUARES PIC 9(6).
       77  INVALID-PARAMETER-SWITCH PIC X.
       77  J PIC 9(6).
       77  K PIC 9(6).
       77  L PIC 9(6).
       77  LEVEL PIC 9(6).
       77  M PIC 9(6).
       77  MOVE-FOUND PIC 9(6).
       77  NON-CONTIGUOUS-SQUARES-FOUND PIC 9(6).
       77  P PIC 9(6).
       77  Q PIC 9(6).
       77  RANDOM-NUMBER1      PIC 9(6)V9(6) COMP.
       77  RANDOM-NUMBER2      PIC 9(6)V9(6) COMP.
       77  RANDOM-NUMBER3      PIC 9(6).
      *
      *    SIZE OF BOARD (3 TO 12) I.E. LENGTH OF 1 SIDE.
      *
      *    SIZE1 WAS CALLED SIZE IN ORIGINAL C PROGRAM BUT THIS IS A
      *    RESERVED WORD IN COBOL;
      * 
       77  SIZE1 PIC 9(6).
      *
      *    SIZE-X-SIZE HOLDS THE NUMBER OF SQUARES IN THE PUZZLE.
      *
       77  SIZE-X-SIZE PIC 9(6).
       77  START-COLUMN PIC 9(6).
       77  START-ROW PIC 9(6). 
       77  X PIC 9(6).
       77  Y PIC 9(6).
     /
       PROCEDURE DIVISION.
      *
       MAIN.
      *
           MOVE ZERO TO INVALID-PARAMETER-SWITCH.
           ACCEPT INPUT-PARAMETERS.
           DISPLAY "INPUT PARAMETERS: " INPUT-PARAMETERS.
      *
           IF RANDOM-NUMBER-SEED IS NUMERIC
               PERFORM GET-RANDOM-NUMBER RANDOM-NUMBER-SEED TIMES
           ELSE 
               DISPLAY "RANDOM NUMBER SEED NOT NUMERIC"
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
      *
           IF SIZE1-INPUT IS NOT NUMERIC
               DISPLAY "SIDE LENGTH MUST BE NUMERIC"
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
           IF SIZE1-INPUT < 3 OR SIZE1-INPUT > 12
               DISPLAY "SIDE LENGTH MUST BE BETWEEN 3 AND 12"
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
           MOVE SIZE1-INPUT TO SIZE1.
           MULTIPLY SIZE1 BY SIZE1 GIVING SIZE-X-SIZE.
      *
           IF START-COLUMN-INPUT IS NOT NUMERIC
               DISPLAY "START COLUMN MUST BE NUMERIC"
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
           IF START-COLUMN-INPUT < 1 OR START-COLUMN-INPUT > SIZE1
               DISPLAY "START COLUMN MUST BE BETWEEN 1 AND " SIZE1
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
           MOVE START-COLUMN-INPUT TO START-COLUMN.
      *
           IF START-ROW-INPUT IS NOT NUMERIC
               DISPLAY "START ROW MUST BE NUMERIC"
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
           IF START-ROW-INPUT < 1 OR START-ROW-INPUT > SIZE1
               DISPLAY "START ROW MUST BE BETWEEN 1 AND " SIZE1
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
           MOVE START-ROW-INPUT TO START-ROW.
      *
           IF END-COLUMN-INPUT IS NOT NUMERIC
               DISPLAY "END COLUMN MUST BE NUMERIC"
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
           IF END-COLUMN-INPUT < 1 OR END-COLUMN-INPUT > SIZE1
               DISPLAY "END COLUMN MUST BE BETWEEN 1 AND " SIZE1
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
           MOVE END-COLUMN-INPUT TO END-COLUMN.
      *
           IF END-ROW-INPUT IS NOT NUMERIC
               DISPLAY "END ROW MUST BE NUMERIC"
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
           IF END-ROW-INPUT < 1 OR END-ROW-INPUT > SIZE1
               DISPLAY "END ROW MUST BE BETWEEN 1 AND " SIZE1
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
           MOVE END-ROW-INPUT TO END-ROW.
      *
           IF START-COLUMN = END-COLUMN
               AND START-ROW = END-ROW
               DISPLAY "START SQUARE MUST NOT EQUAL END SQUARE"
               MOVE "1" TO INVALID-PARAMETER-SWITCH.
      *
           IF INVALID-PARAMETER-SWITCH = ZERO
               DISPLAY "PARAMETERS OK"
               DISPLAY "CALCULATING SOLUTION, PLEASE WAIT"
           ELSE
               DISPLAY "ABOUT TO FINISH"
               ACCEPT DUMMY1
               STOP RUN.
      *  
           MOVE ZEROES TO FILLER1, FILLER3, FILLER5.
           PERFORM DECIDE-MOVE-ORDER1
               VARYING A FROM 1 BY 1 UNTIL A > SIZE-X-SIZE.
           MOVE START-COLUMN TO POSITION1 (1,1).
           MOVE START-ROW TO POSITION1 (1,2).
           MOVE 1 TO BOARD (START-COLUMN,START-ROW).
           MOVE 1 TO LEVEL.
      *
      *    CONTINUE PROCESSING UNTIL EITHER:
      *    (1) ALL COMBINATIONS HAVE BEEN TESTED BUT NO SOLUTION HAS
      *        BEEN FOUND AND THE PROGRAM TRIES TO MOVE THE KING FROM
      *        ITS ORIGINAL POSITION.
      *    OR
      *    (2) A SOLUTION IS FOUND.
      *
           PERFORM FIND-NEXT-MOVE UNTIL
               LEVEL < 1 OR
               LEVEL NOT < SIZE-X-SIZE.
           IF LEVEL = ZERO DISPLAY "NO SOLUTION FOUND".
           IF LEVEL = SIZE-X-SIZE
               DISPLAY "HERE IS A SOLUTION"
               PERFORM DISPLAY-BOARD.
           DISPLAY "ABOUT TO FINISH".
           ACCEPT DUMMY1.           
           STOP RUN.
      *
       GET-RANDOM-NUMBER.
      *
           COMPUTE RANDOM-NUMBER1 = FUNCTION RANDOM().
      *
       DECIDE-MOVE-ORDER1.
      *
           PERFORM DECIDE-MOVE-ORDER2
               VARYING B FROM 1 BY 1 UNTIL B > 8.
      *
       DECIDE-MOVE-ORDER2.    
      *   
           MOVE 0 TO FINISHED.
           PERFORM DECIDE-MOVE-ORDER3 UNTIL FINISHED = 1.
      *
       DECIDE-MOVE-ORDER3.
           COMPUTE RANDOM-NUMBER1 = FUNCTION RANDOM().
           COMPUTE RANDOM-NUMBER2 = RANDOM-NUMBER1 * 8 + 1.
           MOVE RANDOM-NUMBER2 TO RANDOM-NUMBER3.
           IF RANDOM-MOVES (A,RANDOM-NUMBER3) = 0
               MOVE B TO RANDOM-MOVES (A,RANDOM-NUMBER3)
               MOVE 1 TO FINISHED.
      *
       FIND-NEXT-MOVE.
      *
      *    LOOK FOR NEXT AVAILABLE MOVE FROM CURRENT POSITION:
      *
      D    PERFORM DISPLAY-BOARD.
           MOVE ZERO TO MOVE-FOUND.
           PERFORM TRY-MOVES THROUGH TRY-MOVES-X UNTIL
               POSITION1 (LEVEL,3) NOT LESS THAN 8 OR
               MOVE-FOUND = 1.
           IF MOVE-FOUND = ZERO
               MOVE POSITION1 (LEVEL,1) TO X
               MOVE POSITION1 (LEVEL,2) TO Y
               MOVE ZERO TO BOARD (X,Y)
               MOVE ZERO TO POSITION1 (LEVEL,1),
                            POSITION1 (LEVEL,2),
                            POSITION1 (LEVEL,3)
               SUBTRACT 1 FROM LEVEL
           ELSE
               ADD 1 TO LEVEL
               MOVE X TO POSITION1 (LEVEL,1)
               MOVE Y TO POSITION1 (LEVEL,2)
               MOVE LEVEL TO BOARD (X,Y). 
      *         
       TRY-MOVES.
      *
           ADD 1 TO POSITION1 (LEVEL,3).
           MOVE POSITION1 (LEVEL,1) TO X.
           MOVE POSITION1 (LEVEL,2) TO Y.
           MOVE POSITION1 (LEVEL,3) TO P.
           MOVE RANDOM-MOVES (LEVEL,P) TO Q.
           IF Q = 1 ADD 1 TO Y.
           IF Q = 2 ADD 1 TO X ADD 1 TO Y.
           IF Q = 3 ADD 1 TO X.
           IF Q = 4 ADD 1 TO X SUBTRACT 1 FROM Y.
           IF Q = 5 SUBTRACT 1 FROM Y.
           IF Q = 6 SUBTRACT 1 FROM X SUBTRACT 1 FROM Y.
           IF Q = 7 SUBTRACT 1 FROM X.
           IF Q = 8 SUBTRACT 1 FROM X ADD 1 TO Y.
           IF X < 1 OR X > SIZE1 OR Y < 1 OR Y > SIZE1
               GO TO TRY-MOVES-X.
           IF BOARD (X,Y) NOT EQUAL ZERO
               GO TO TRY-MOVES-X.
      *
      *    IT IS ONLY POSSIBLE TO GO TO THE FINAL SQUARE ONCE ALL OTHER
      *    SQUARES HAVE BEEN VISITED:
      *
           IF X = END-COLUMN AND Y = END-ROW AND
               LEVEL NOT EQUAL SIZE-X-SIZE - 1
               GO TO TRY-MOVES-X.
      *
      *    CHECK THAT ALL THE UNOCCUPIED SQUARES ARE CONTIGUOUS.
      *    START BY ASSUMING THAT THEY ARE NOT:
      *
           MOVE ZEROES TO FILLER7.
      *
      *    FIND THE 1ST UNOCCUPIED SQUARE:
      *
           PERFORM FIND-UNOCCUPIED-SQUARE
               VARYING A FROM 1 BY 1 UNTIL A > SIZE1
                 AFTER B FROM 1 BY 1 UNTIL B > SIZE1.
      *
      *    NOW FIND ALL THE EMPTY SQUARES WHICH ARE CONTIGUOUS WITH THIS
      *    ONE:
      *
           MOVE 1 TO CONTIGUOUS-SQUARES-FOUND.
           PERFORM FIND-CONTIGUOUS-SQUARES1
               UNTIL CONTIGUOUS-SQUARES-FOUND = ZERO.
      *
      *    THE CONNECTIVITY-CHECK ARRAY SHOULD NOW HAVE A LIST OF ALL
      *    THE EMPTY SQUARES WHICH ARE CONTIGUOUS WITH THE FIRST.
      *
      *    NOW SCAN THE BOARD FOR EMPTY SQUARES WHICH ARE NOT CONTIGUOUS
      *    WITH THIS LIST:
      *
           MOVE ZERO TO NON-CONTIGUOUS-SQUARES-FOUND.
           PERFORM FIND-NON-CONTIGUOUS-UNUSED
               VARYING A FROM 1 BY 1 UNTIL A > SIZE1
                 AFTER B FROM 1 BY 1 UNTIL B > SIZE1.
           IF NON-CONTIGUOUS-SQUARES-FOUND = 1
               GO TO TRY-MOVES-X.
      *
      *    NOW SCAN THE BOARD AGAIN FOR INACCESSIBLE SQUARES I.E. THOSE
      *    WHICH DO NOT HAVE A WAY IN AND OUT:
      *
           MOVE 0 TO INACCESSIBLE-SQUARES.
           PERFORM FIND-INACCESSIBLE-SQUARES1
           THROUGH FIND-INACCESSIBLE-SQUARES1-X
               VARYING I FROM 1 BY 1 UNTIL I > SIZE1
                 AFTER J FROM 1 BY 1 UNTIL J > SIZE1.
           IF INACCESSIBLE-SQUARES = 1
               GO TO TRY-MOVES-X.
           MOVE 1 TO MOVE-FOUND.
      *
       TRY-MOVES-X.
      *
           EXIT.
      *
       FIND-UNOCCUPIED-SQUARE.
      *
           IF BOARD (A,B) = ZERO
               MOVE A TO I
               MOVE B TO J
      *    RECORD THIS SQUARE IN THE TABLE WHICH WILL CHECK IF ALL THE
      *    EMPTY SQUARES ARE CONTIGUOUS:
               MOVE 1 TO CONNECTIVITY-CHECK (I,J)
      *    JUMP OUT OF THE LOOP:
               MOVE 9 TO A,B.
      *
       FIND-CONTIGUOUS-SQUARES1.
      *
           MOVE ZERO TO CONTIGUOUS-SQUARES-FOUND.
           PERFORM FIND-CONTIGUOUS-SQUARES2
               VARYING A FROM 1 BY 1 UNTIL A > 8
                 AFTER B FROM 1 BY 1 UNTIL B > 8.
      *
       FIND-CONTIGUOUS-SQUARES2.
      *
           IF CONNECTIVITY-CHECK (A,B) = 1
               PERFORM FIND-CONTIGUOUS-SQUARES3
               VARYING K FROM 1 BY 1 UNTIL K > 8.
      *
       FIND-CONTIGUOUS-SQUARES3.
      *
           IF K = 1
              MOVE A TO I
              ADD 1 TO B GIVING J ELSE
           IF K = 2
               ADD 1 TO A GIVING I
               ADD 1 TO B GIVING J ELSE
           IF K = 3
               ADD 1 TO A GIVING I
               MOVE B TO J ELSE
           IF K = 4
               ADD 1 TO A GIVING I
               SUBTRACT 1 FROM B GIVING J ELSE
           IF K = 5
               MOVE A TO I
               SUBTRACT 1 FROM B GIVING J ELSE
           IF K = 6
               SUBTRACT 1 FROM A GIVING I
               SUBTRACT 1 FROM B GIVING J ELSE
           IF K = 7
               SUBTRACT 1 FROM A GIVING I
               MOVE B TO J ELSE
           IF K = 8
               SUBTRACT 1 FROM A GIVING I
               ADD 1 TO B GIVING J.
           IF I NOT LESS THAN 1
               AND I NOT > SIZE1
               AND J NOT LESS THAN 1
               AND J NOT > SIZE1
               AND BOARD (I,J) = ZERO
               AND CONNECTIVITY-CHECK (I,J) = ZERO
               MOVE 1 TO CONNECTIVITY-CHECK (I,J),
                         CONTIGUOUS-SQUARES-FOUND.
      *
       FIND-NON-CONTIGUOUS-UNUSED.
      *
           IF BOARD (A,B) = ZERO AND CONNECTIVITY-CHECK (A,B) NOT = 1
               MOVE 1 TO NON-CONTIGUOUS-SQUARES-FOUND.
      *
       FIND-INACCESSIBLE-SQUARES1.
      *
           IF BOARD (I,J) NOT = 0
               GO TO FIND-INACCESSIBLE-SQUARES1-X.
           IF I = X AND J = Y
               GO TO FIND-INACCESSIBLE-SQUARES1-X.
           IF I = END-COLUMN AND J = END-ROW
               GO TO FIND-INACCESSIBLE-SQUARES1-X.
           MOVE ZERO TO ACCESSIBILITY.
           PERFORM FIND-INACCESSIBLE-SQUARES2
           THROUGH FIND-INACCESSIBLE-SQUARES2-X
               VARYING K FROM 1 BY 1 UNTIL K > 8.
           IF ACCESSIBILITY < 2
               MOVE 1 TO INACCESSIBLE-SQUARES.
      *
       FIND-INACCESSIBLE-SQUARES1-X.
      *
       EXIT.
      *
       FIND-INACCESSIBLE-SQUARES2.
      *
           IF K = 1
               MOVE I TO L
               ADD 1 TO J GIVING M ELSE
           IF K = 2
               ADD 1 TO I GIVING L
               ADD 1 TO J GIVING M ELSE
           IF K = 3
               ADD 1 TO I GIVING L
               MOVE J TO M ELSE
           IF K = 4
               ADD 1 TO I GIVING L
               SUBTRACT 1 FROM J GIVING M ELSE
           IF K = 5
               MOVE I TO L
               SUBTRACT 1 FROM J GIVING M ELSE
           IF K = 6
               SUBTRACT 1 FROM I GIVING L
               SUBTRACT 1 FROM J GIVING M ELSE
           IF K = 7
               SUBTRACT 1 FROM I GIVING L
               MOVE J TO M ELSE
           IF K = 8
               SUBTRACT 1 FROM I GIVING L
               ADD 1 TO J GIVING M.
           IF L < 1 OR L > SIZE1 OR M < 1 OR M > SIZE1
               GO TO FIND-INACCESSIBLE-SQUARES2-X.
           IF BOARD (L,M) = 0
               ADD 1 TO ACCESSIBILITY.
      *
       FIND-INACCESSIBLE-SQUARES2-X.
      *
       EXIT.
      *  
       DISPLAY-BOARD.
      *
           DISPLAY BOARD (8,1) " " BOARD (8,2) " "
                   BOARD (8,3) " " BOARD (8,4) " "
                   BOARD (8,5) " " BOARD (8,6) " "
                   BOARD (8,7) " " BOARD (8,8).
           DISPLAY BOARD (7,1) " " BOARD (7,2) " "
                   BOARD (7,3) " " BOARD (7,4) " "
                   BOARD (7,5) " " BOARD (7,6) " "
                   BOARD (7,7) " " BOARD (7,8).
           DISPLAY BOARD (6,1) " " BOARD (6,2) " "
                   BOARD (6,3) " " BOARD (6,4) " "
                   BOARD (6,5) " " BOARD (6,6) " "
                   BOARD (6,7) " " BOARD (6,8).
           DISPLAY BOARD (5,1) " " BOARD (5,2) " "
                   BOARD (5,3) " " BOARD (5,4) " "
                   BOARD (5,5) " " BOARD (5,6) " "
                   BOARD (5,7) " " BOARD (5,8).
           DISPLAY BOARD (4,1) " " BOARD (4,2) " "
                   BOARD (4,3) " " BOARD (4,4) " "
                   BOARD (4,5) " " BOARD (4,6) " "
                   BOARD (4,7) " " BOARD (4,8).
           DISPLAY BOARD (3,1) " " BOARD (3,2) " "
                   BOARD (3,3) " " BOARD (3,4) " "
                   BOARD (3,5) " " BOARD (3,6) " "
                   BOARD (3,7) " " BOARD (3,8).
           DISPLAY BOARD (2,1) " " BOARD (2,2) " "
                   BOARD (2,3) " " BOARD (2,4) " "
                   BOARD (2,5) " " BOARD (2,6) " "
                   BOARD (2,7) " " BOARD (2,8).
           DISPLAY BOARD (1,1) " " BOARD (1,2) " "
                   BOARD (1,3) " " BOARD (1,4) " "
                   BOARD (1,5) " " BOARD (1,6) " "
                   BOARD (1,7) " " BOARD (1,8).
           DISPLAY " ".
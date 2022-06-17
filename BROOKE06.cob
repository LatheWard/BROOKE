      ******************************************************************
      * Author: LATHE WARD
      * Date: 07/20/21
      * Purpose: Register/Edit users for solid waste
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. BROOKE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT MYNAMES ASSIGN TO "SAMPLE.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT ADD-TO-DATA ASSIGN TO "TEMPFILE.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MYNAMES.
       01 THEUSERS.
            05 OWNER-NAME      PIC A(20).
            05 OWNER-ADDRESS   PIC X(35).

       FD ADD-TO-DATA.
       01 HOLDUSERS.
           05 N-OWNER-NAME     PIC A(20).
           05 N-OWNER-ADDRESS  PIC X(35).

       WORKING-STORAGE SECTION.
       01 USER-FIRST-CHOICE        PIC A.

       01 SWITCHES.
           05 EOF-SWITCH           PIC X VALUE "N".
           05 MOD-EOF-SWITCH       PIC X VALUE "N".

       01  HEADING-LINE.

        05 FILLER            PIC X(11)  VALUE "OWNER-NAME".
        05 FILLER            PIC X(9)   VALUE SPACES.
        05 FILLER            PIC X(16)  VALUE "OWNER-ADDRESS".
        05 FILLER            PIC X(16)  VALUE SPACES.

        01 MOD-NAME                  PIC A(20).


       PROCEDURE DIVISION.
           DISPLAY "==================================".
           DISPLAY "THREE RIVERS PLANNING FEE BILLING".
           DISPLAY "REMEMBER TO ACTIVATE CAPS LOCK".
           PERFORM 0100-MAIN-LOOP UNTIL USER-FIRST-CHOICE = "E".

       0100-MAIN-LOOP.
       DISPLAY "==================================".
       DISPLAY "ADDITIONS [A]"
       DISPLAY "MODIFICATION [B]"
       DISPLAY "SHOW NAMES AND ADDRESSES [C]"
       DISPLAY "DELETE [D]"
       DISPLAY "EXIT [E]"
       ACCEPT USER-FIRST-CHOICE

       IF USER-FIRST-CHOICE = "A" THEN
           OPEN EXTEND MYNAMES
           PERFORM 1400-ADD
           CLOSE MYNAMES
       END-IF.

       IF USER-FIRST-CHOICE = "B" THEN
           DISPLAY "ENTER NAME OF OWNER TO MODIFY"
           ACCEPT MOD-NAME
           PERFORM 1310-FIND-MODIFY
       END-IF

       IF USER-FIRST-CHOICE = "C" THEN
           DISPLAY HEADING-LINE
           OPEN INPUT MYNAMES
           PERFORM 1200-SHOW UNTIL EOF-SWITCH = "Y"
           MOVE "N" TO EOF-SWITCH
           CLOSE MYNAMES
       END-IF.

       IF USER-FIRST-CHOICE = "D" THEN
           DISPLAY "ENTER NAME TO DELETE"
           ACCEPT MOD-NAME
           PERFORM 1500-FILTER-FOR-DELETE
           PERFORM 1335-REWRITE-MYNAMES-FROM-ADD
          END-IF.

       IF USER-FIRST-CHOICE = "E"
          DISPLAY "GOODBYE"
          STOP RUN
       END-IF.

       1200-SHOW.
       READ MYNAMES
          AT END
               MOVE "Y" TO EOF-SWITCH
          NOT AT END
               DISPLAY OWNER-NAME, OWNER-ADDRESS
       END-READ.

       1300-PERFORM-MODIFY.
           OPEN INPUT MYNAMES
           READ MYNAMES
           AT END
               MOVE "Y" TO EOF-SWITCH
           NOT AT END
               IF MOD-NAME EQUALS OWNER-NAME
                   DISPLAY "MATCH FOUND"
                   PERFORM 1310-FIND-MODIFY
           END-IF
           END-READ.
           CLOSE MYNAMES.

       1310-FIND-MODIFY.
           PERFORM 1330-FILTER-TO-ADD-TO-DATA.
           PERFORM 1335-REWRITE-MYNAMES-FROM-ADD.

       1330-FILTER-TO-ADD-TO-DATA.
           DISPLAY "Filtering"
           OPEN OUTPUT ADD-TO-DATA.
           OPEN INPUT MYNAMES.
           MOVE "N" TO EOF-SWITCH
           PERFORM UNTIL EOF-SWITCH = "Y"
              READ MYNAMES
              AT END
                 MOVE "Y" TO EOF-SWITCH
              NOT AT END
                  IF OWNER-NAME NOT EQUALS MOD-NAME
                     MOVE OWNER-NAME TO N-OWNER-NAME
                     MOVE OWNER-ADDRESS TO N-OWNER-ADDRESS
                  ELSE
                      DISPLAY "NAME >>> "
                      ACCEPT N-OWNER-NAME
                      DISPLAY "ADDRESS >>> "
                      ACCEPT N-OWNER-ADDRESS
                  END-IF
                  WRITE HOLDUSERS
              END-READ
           END-PERFORM
           CLOSE ADD-TO-DATA.
           CLOSE MYNAMES.

       1335-REWRITE-MYNAMES-FROM-ADD.
           DISPLAY "Rewriting"
           OPEN INPUT ADD-TO-DATA.
           OPEN OUTPUT MYNAMES.
           MOVE "N" TO EOF-SWITCH
           PERFORM UNTIL EOF-SWITCH = "Y"
              READ ADD-TO-DATA
              AT END
                 MOVE "Y" TO EOF-SWITCH
              NOT AT END
                 IF NOT N-OWNER-NAME = "NONE"
                 WRITE THEUSERS FROM HOLDUSERS
                 END-IF
              END-READ
           END-PERFORM
           CLOSE ADD-TO-DATA.
           CLOSE MYNAMES.

       1400-ADD.
           DISPLAY "NAME >>> "
           ACCEPT OWNER-NAME
           DISPLAY "ADDRESS >>> "
           ACCEPT OWNER-ADDRESS
           WRITE THEUSERS.

       1500-FILTER-FOR-DELETE.
           DISPLAY "Filtering"
           OPEN OUTPUT ADD-TO-DATA.
           OPEN INPUT MYNAMES.
           MOVE "N" TO EOF-SWITCH
           PERFORM UNTIL EOF-SWITCH = "Y"
              READ MYNAMES
              AT END
                 MOVE "Y" TO EOF-SWITCH
              NOT AT END
                  IF OWNER-NAME NOT EQUALS MOD-NAME
                     MOVE OWNER-NAME TO N-OWNER-NAME
                     MOVE OWNER-ADDRESS TO N-OWNER-ADDRESS
                  ELSE
                     MOVE "NONE" TO N-OWNER-NAME
                     MOVE " " TO N-OWNER-ADDRESS
                  END-IF
                  WRITE HOLDUSERS
              END-READ
           END-PERFORM
           CLOSE ADD-TO-DATA.
           CLOSE MYNAMES.

       END PROGRAM BROOKE.

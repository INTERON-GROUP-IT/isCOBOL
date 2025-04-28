      *> Copyright (c) 2005 - 2024 Veryant. Users of isCOBOL
      *> may freely modify and redistribute this program.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. io-indexed.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE1 ASSIGN TO "FileIndexed"
                 ORGANIZATION IS INDEXED
                 ACCESS IS DYNAMIC
                 RECORD KEY IS KEY-FILE1
                 ALTERNATE RECORD KEY IS A01-4 WITH DUPLICATES
                 ALTERNATE RECORD KEY IS A01-7 WITH DUPLICATES
                 LOCK MODE IS MANUAL
                 FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE1.
       01  REC-FILE1.
           03  KEY-FILE1        PIC 9(09).
           03  D-A01.
               05  A01-1        PIC X(10).
               05  A01-2        PIC X(01).
               05  A01-3        PIC X(06).
               05  A01-4        PIC X(50).
               05  A01-5        PIC X(50).
               05  A01-6        PIC X(35).
               05  A01-7        PIC X(16).
               05  A01-8        PIC 9(11).
               05  A01-9.
                   10  A01-10   PIC X(35).
                   10  A01-11   PIC X(30).
                   10  A01-12   PIC X(02).
                   10  A01-13   PIC 9(05).
                   10  A01-14   PIC 9(03).
                   10  A01-15   PIC 9(05).
                   10  A01-16   PIC X(15).
               05  A01-17       PIC 9(04). 
               05  A01-18.
                   10  A01-19   PIC X(35).
                   10  A01-20   PIC X(30).
                   10  A01-21   PIC X(02).
                   10  A01-22   PIC 9(05).
                   10  A01-23   PIC 9(03).
                   10  A01-24   PIC 9(05).
                   10  A01-25   PIC X(15).
               05  A01-26       PIC X(01).
               05  A01-27       PIC 9(04). 
               05  A01-28.
                   10  A01-29   PIC X(35).
                   10  A01-30   PIC X(30).
                   10  A01-31   PIC X(02).
                   10  A01-32   PIC 9(05).
                   10  A01-33   PIC 9(03).
                   10  A01-34   PIC 9(05).
                   10  A01-35   PIC X(15).
               05  A01-36       PIC 9(08).
               05  A01-37       PIC X(30).
               05  A01-38       PIC X(02).
               05  A01-39       PIC 9(01).
               05  A01-40       PIC 9(02).
               05  A01-41       PIC 9(01).
               05  A01-42       PIC 9(01).
               05  A01-43       PIC 9(01).
               05  A01-44       PIC 9(03).
               05  A01-45       PIC 9(08).
               05  A01-46       PIC 9(08).
               05  A01-47       PIC X(05).
               05  A01-48       PIC X(30).
               05  A01-49.
                   10  A01-50   PIC X(12).
                   10  A01-51   PIC X(02).
               05  A01-52       PIC 9(05).
               05  A01-53       PIC X(01).
               05  A01-54       PIC X(01).
               05  A01-55       PIC 9(05).
               05  A01-56       PIC 9(06).
               05  FILLER       PIC X(04).
               05  A01-57       PIC X(01).
               05  A01-58       PIC 9(03).
               05  A01-59       PIC 9(03).
               05  A01-60       PIC 9(03).
               05  A01-61       PIC 9(03).
               05  FILLER       PIC X(07). 
               05  A01-62       PIC X(30).
               05  A01-63       PIC 9(04).
               05  A01-64       PIC 9(04).
               05  A01-65       PIC S9(05)V99 COMP-3 OCCURS 2.
               05  A01-66       PIC X(01).
               05  A01-67       PIC X(01).
               05  A01-68       PIC 9(08).
               05  A01-69       PIC X(02).
               05  A01-70       PIC X(10).
               05  A01-71       PIC 9(08).
               05  A01-72       PIC X(02).
               05  A01-73       PIC X(10).
               05  A01-74       PIC 9(08).
               05  A01-75       PIC X(02).
               05  A01-76       PIC X(10).
               05  A01-77       PIC 9(08).
               05  A01-78       PIC X(02).
               05  A01-79       PIC X(10).
               05  A01-80       PIC 9(08).
               05  A01-81       PIC X(02).
               05  A01-82       PIC X(10).
               05  A01-83       PIC 9(08).
               05  A01-84       PIC X(02).
               05  A01-85       PIC 9(01).
               05  A01-86       PIC X(01).
               05  A01-87       PIC X(01).
               05  A01-88       PIC X(01).
               05  A01-89       PIC S9(03)V99 COMP-3.
               05  A01-90       PIC X(15).
               05  A01-91       PIC S9(11)V99 COMP-3.
               05  A01-92       PIC X(05).
               05  A01-93       PIC 9(05).
               05  FILLER       PIC X(13).
               05  A01-94       PIC X(01).
               05  A01-95       PIC X(30).
               05  A01-96       PIC 9(04).
               05  A01-97       PIC 9(10).
               05  A01-98       PIC X(30).
               05  A01-99       PIC 9(05).
               05  A01-100      PIC X(20).
               05  A01-101      PIC X(02).
               05  A01-102      PIC S9(05) COMP-3.
               05  A01-103      PIC 9(03) OCCURS 5.
               05  A01-104      PIC 9(03).
               05  A01-105      PIC 9(03).
               05  A01-106      PIC 9(03).
               05  A01-107      PIC 9(02).
               05  A01-108      PIC 9(03).
               05  A01-109      PIC X(01).
               05  A01-110      PIC 9(01).
               05  A01-111      PIC X(01).
               05  A01-112      PIC 9(01).
               05  A01-113      PIC X(01).
               05  A01-114      PIC 9(02).
               05  A01-115      PIC 9(01).
               05  A01-116      PIC X(06).
               05  A01-117      PIC 9(01).
               05  A01-118      PIC X(01).
               05  A01-119      PIC 9(01).
               05  A01-120      PIC X(01).
               05  A01-121      PIC 9(02) OCCURS 3.
               05  A01-122      PIC S9(03)V99 COMP-3.
               05  A01-123      PIC S9(01)V99 COMP-3 OCCURS 7.
               05  A01-124      PIC 9(04) OCCURS 2.
               05  A01-125      OCCURS 5.
                   10  A01-126  PIC 9(03).
                   10  A01-127  PIC X(15).
               05  A01-128      PIC 9(03).
               05  A01-129      PIC 9(02).
               05  A01-130      PIC 9(02).
               05  A01-131      OCCURS 2. 
                   10  A01-132  PIC X(07).
                   10  A01-133  PIC 9(08).
                   10  A01-134  PIC 9(03).
               05  A01-135      PIC 9(01) OCCURS 10.
               05  A01-136      PIC 9(01).
               05  A01-137      PIC 9(05).
               05  A01-138      PIC X(15).
               05  A01-139      PIC 9(08).
               05  A01-140      PIC X(01).
               05  A01-141      PIC X(16).
               05  A01-142      OCCURS 4. 
                   10  A01-143  PIC 9(03).
                   10  A01-144  PIC 9(08).
               05  EX1-145      PIC X(01).
               05  EX1-146      PIC 9(04).
               05  EX1-147      PIC S9(11) COMP-3. 
               05  A01-148      PIC 9(08).
               05  A01-149      PIC X(02).
               05  A01-150      PIC 9(03).
               05  A01-151      PIC X(05).
               05  A01-152      PIC 9(05).
               05  A01-153      PIC 9(05).
               05  A01-154      PIC 9(02).
               05  A01-155      PIC 9(02).
               05  A01-156      PIC 9(13).
               05  A01-157      PIC 9(06).
               05  A01-158      PIC 9(05).
               05  A01-159      PIC 9(05).
               05  A01-160      PIC S9(09)V99 COMP-3.
               05  A01-161      PIC 9(02).
               05  A01-162      PIC 9(03).
               05  A01-163      PIC X(01).
               05  A01-164      PIC 9(03).
               05  A01-165      PIC 9(03).
               05  A01-166      PIC 9(06).
               05  A01-167      PIC 9(06).
               05  A01-168      PIC 9(02).
               05  A01-169      PIC X(01).
               05  A01-170      PIC 9(01).
               05  A01-171      PIC X(01).
               05  A01-172      PIC 9(09).
               05  A01-173      PIC 9(02).
               05  A01-174      PIC 9(04).
               05  A01-175      PIC X(01).
               05  A01-176      PIC X(01).
               05  A01-177      PIC 9(01).
               05  A01-178      PIC 9(02).
               05  A01-179      PIC 9(02).
               05  A01-180      PIC 9(02).
               05  A01-181      PIC 9(01).
               05  A01-182      PIC X(01).
               05  EX1-183      PIC 9(01).
               05  A01-184      PIC X(02).
               05  A01-185      PIC X(04).
               05  A01-186      PIC X(15).
               05  A01-187      PIC X(30).
               05  A01-188      PIC X(25). 
               05  A01-189      PIC 9(06) OCCURS 4.
               05  A01-190      PIC X(01).
               05  A01-191      PIC 9(01). 
               05  A01-192      PIC X(123). 

       WORKING-STORAGE SECTION.
       78  NUM-TIMES            VALUE 10000.

       77  FILE-STATUS          PIC X(2).
       77  SYNC-TIME-1          PIC 9(8).
       77  SYNC-TIME-2          PIC 9(8).
       77  TIME-DIFF            PIC 9(6)V99.
       77  BEGIN-SECS           PIC 9(6)V99.
       77  END-SECS             PIC 9(6)V99.
       77  BEGIN-TIME           PIC 9(8).
       01  BEGIN-TIME-FIELDS    REDEFINES BEGIN-TIME.
           03  BEGIN-HOURS      PIC 9(2).
           03  BEGIN-MINUTES    PIC 9(2).
           03  BEGIN-SECONDS    PIC 9(2).
           03  BEGIN-HUNDRETHS  PIC 9(2).
       77  END-TIME             PIC 9(8).
       01  END-TIME-FIELDS      REDEFINES END-TIME.
           03  END-HOURS        PIC 9(2).
           03  END-MINUTES      PIC 9(2).
           03  END-SECONDS      PIC 9(2).
           03  END-HUNDRETHS    PIC 9(2).
       77  TOTAL-TIME           PIC 9(7)V99 VALUE ZERO.
       77  TIME-DISP            PIC Z,ZZZ,ZZ9.99.
       77  IND                  PIC 9(9).
       77  FLAG-EOF             PIC 9.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY " "
           DISPLAY "INDEXED FILES"
           DISPLAY "NUM-TIMES:  " NUM-TIMES

           PERFORM LOAD-FILE1-TEST.
           PERFORM READ-FILE1-TEST.
           PERFORM UPDATE-FILE1-TEST.
           PERFORM DELETE-FILE1-TEST.

           MOVE TOTAL-TIME TO TIME-DISP
           DISPLAY "Total Time: " TIME-DISP

           GOBACK
           .

       LOAD-FILE1-TEST.
           INITIALIZE D-A01
           OPEN OUTPUT FILE1.
           PERFORM START-TIMER.
           PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > NUM-TIMES
              MOVE IND TO KEY-FILE1
              WRITE REC-FILE1
           END-PERFORM.
           PERFORM STOP-TIMER.
           CLOSE FILE1.
           ADD TIME-DIFF TO TOTAL-TIME.
           MOVE TIME-DIFF TO TIME-DISP
           DISPLAY "WRITE:      " TIME-DISP
           .

       READ-FILE1-TEST.
           OPEN INPUT FILE1.
           MOVE 0 TO FLAG-EOF
           MOVE 0 TO KEY-FILE1
           START FILE1 KEY >= KEY-FILE1
           PERFORM START-TIMER.
           PERFORM UNTIL FLAG-EOF = 1
              READ FILE1 NEXT RECORD
               AT END 
                 MOVE 1 TO FLAG-EOF
              END-READ
           END-PERFORM.
           PERFORM STOP-TIMER.
           CLOSE FILE1.
           ADD TIME-DIFF TO TOTAL-TIME.
           MOVE TIME-DIFF TO TIME-DISP
           DISPLAY "READ:       " TIME-DISP
           .

       UPDATE-FILE1-TEST.
           OPEN I-O FILE1.
           PERFORM START-TIMER.
           PERFORM VARYING IND FROM 1 BY 1 UNTIL IND IS > NUM-TIMES
              MOVE IND TO key-FILE1
              REWRITE REC-FILE1
           END-PERFORM.
           PERFORM STOP-TIMER.
           CLOSE FILE1.
           ADD TIME-DIFF TO TOTAL-TIME.
           MOVE TIME-DIFF TO TIME-DISP
           DISPLAY "REWRITE:    " TIME-DISP
           .

       DELETE-FILE1-TEST.
           OPEN I-O FILE1.
           PERFORM START-TIMER.
           PERFORM VARYING IND FROM 1 BY 1 UNTIL IND IS > NUM-TIMES
              MOVE IND TO KEY-FILE1
              DELETE FILE1 record
           END-PERFORM.
           PERFORM STOP-TIMER.
           CLOSE FILE1.
           ADD TIME-DIFF TO TOTAL-TIME.
           MOVE TIME-DIFF TO TIME-DISP
           DISPLAY "DELETE:     " TIME-DISP
           .

       START-TIMER.
           ACCEPT SYNC-TIME-1 FROM TIME.
           PERFORM WITH TEST AFTER UNTIL SYNC-TIME-1 NOT = SYNC-TIME-2
               ACCEPT SYNC-TIME-2 FROM TIME
           END-PERFORM.
           ACCEPT BEGIN-TIME FROM TIME
           .

       STOP-TIMER.
           ACCEPT END-TIME FROM TIME.
           COMPUTE BEGIN-SECS = (BEGIN-HOURS * 3600) +
               (BEGIN-MINUTES * 60) + (BEGIN-SECONDS) +
               (BEGIN-HUNDRETHS / 100).
           COMPUTE END-SECS = (END-HOURS * 3600) +
               (END-MINUTES * 60) + (END-SECONDS) +
               (END-HUNDRETHS / 100).
           IF (END-SECS < BEGIN-SECS) THEN
               ADD 86400 TO END-SECS
           END-IF.
           COMPUTE TIME-DIFF = END-SECS - BEGIN-SECS
           .
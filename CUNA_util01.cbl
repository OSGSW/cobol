       IDENTIFICATION DIVISION.                                         00000100
000002*                                                                 00000200
000004*                                                                 00000400
      *
000011 ENVIRONMENT DIVISION.                                            00001300
000012*                                                                 00001400
000013 CONFIGURATION SECTION.                                           00001500
000014*                                                                 00001600
000015 SOURCE-COMPUTER. IBM-370.                                        00001700
000016 OBJECT-COMPUTER. IBM-370.                                        00001800
000017*                                                                 00001900
000018 INPUT-OUTPUT SECTION.                                            00002000
000019*                                                                 00002100
000020 FILE-CONTROL.                                                    00002200
000021*                                                                 00002300
000022     SELECT IP-FILE1 ASSIGN TO WS-MCP-SYSIN1-PATH
           ORGANIZATION IS LINE SEQUENTIAL

           FILE STATUS IS WS-MCP-SYSIN1-STATUS.

      *
      *
000024     SELECT OP-FILE1 ASSIGN TO WS-MCP-SYSOUT1-PATH
           ORGANIZATION IS LINE SEQUENTIAL

               FILE STATUS IS WS-MCP-SYSOUT1-STATUS.

           SELECT CTL-FILE ASSIGN TO
           WS-CONTROL-FILE-PATH
           FILE STATUS IS WS-CONTROL-FILE-STATUS
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SYSLOG-FILE ASSIGN TO
           WS-SYSLOG-FILE-PATH
           FILE STATUS IS WS-SYSLOG-FILE-STATUS
           ORGANIZATION IS LINE SEQUENTIAL.



000025*                                                                 00002700
000026*
000027 DATA DIVISION.                                                   00002800
000028*                                                                 00002900
000029 FILE SECTION.                                                    00003000
000030*                                                                 00003100
000031 FD  IP-FILE1                                                     00003200
000036     DATA RECORD IS IP-REC1.
000037*                                                                 00003600
000038 01  IP-REC1 pic x(3000).
      *
000039*                                                                 00003100
000040 FD  OP-FILE1                                                     00003900
000045     DATA RECORD IS OP-REC1.                                      00004400
000046*                                                                 00004500
000047 01  OP-REC1 pic x(3000).

       FD  CTL-FILE.
       01  CTL-REC                        PIC X(300).

       FD  SYSLOG-FILE.
       01  SYSLOG-REC                     PIC X(130).

000048*                                                                 00004700
000049*                                                                 00004700
000050 WORKING-STORAGE SECTION.                                         00004800
000051*
       COPY FORMCP.
       01  IP-FILE1-STATUS          PIC X(02).


000052 01  FLAGS.
000053     05  WS-EOF                    PIC X VALUE 'N'.
000054         88  END-OF-FILE                 VALUE 'Y'.
           05  WS-PAPER-SW               PIC X VALUE 'N'.
000053     05  WS-EOF-PATH               PIC X VALUE 'N'.
000054         88  EOF-PATH                    VALUE 'Y'.
000055*
000056 01  WS-COUNTERS.
000057     05  WS-REC-CNTR-IN            PIC 9(9) VALUE ZEROS.
000058     05  WS-REC-CNTR-OUT           PIC 9(9) VALUE ZEROS.
           05  WS-1-COUNT                PIC 9(9) VALUE ZEROES.
           05  WS-2-COUNT                PIC 9(9) VALUE ZEROES.
           05  WS-3-COUNT                PIC 9(9) VALUE ZEROES.
           05  WS-4-COUNT                PIC 9(9) VALUE ZEROES.
           05  WS-8-COUNT                PIC 9(9) VALUE ZEROES.

       01  WS-MAX-PAGES                    PIC X(04).

      *                                                                 00071500


000138 PROCEDURE DIVISION.
000139*
000140****************
000141 000-MAIN-LOGIC.
000142****************

000143     PERFORM 100-INIT.

           IF RETURN-CODE NOT = +999
000144         PERFORM 200-PROCESS
000145             UNTIL END-OF-FILE
               DISPLAY 'PRESORTS        ' WS-1-COUNT
               DISPLAY 'RESIDUALS       ' WS-2-COUNT
               DISPLAY 'NO MAILS        ' WS-3-COUNT
               DISPLAY 'HANDSTUFFS      ' WS-4-COUNT
               DISPLAY 'PDF         ' WS-8-COUNT
               WRITE SYSLOG-REC FROM 'PRESORTS        '
               WRITE SYSLOG-REC FROM WS-1-COUNT
               WRITE SYSLOG-REC FROM 'RESIDUALS       '
               WRITE SYSLOG-REC FROM WS-2-COUNT
               WRITE SYSLOG-REC FROM 'NO MAILS        '
               WRITE SYSLOG-REC FROM WS-3-COUNT
               WRITE SYSLOG-REC FROM 'HANDSTUFFS      '
               WRITE SYSLOG-REC FROM WS-4-COUNT
               WRITE SYSLOG-REC FROM 'PDF      '
               WRITE SYSLOG-REC FROM WS-8-COUNT
           END-IF.



000146     CLOSE IP-FILE1, OP-FILE1.


000147     STOP RUN.
000148****************
000149 100-INIT.
000150****************

           ACCEPT WS-CONTROL-FILE-PATH FROM COMMAND-LINE.
      *    move 'debug\cobolexec.ctl' to WS-CONTROL-FILE-PATH.
           OPEN INPUT CTL-FILE.

           MOVE '0006' TO WS-MAX-PAGES.

           IF WS-CONTROL-FILE-STATUS = '00'
               READ CTL-FILE
                   AT END MOVE 'Y' TO WS-EOF-CTL
               END-READ
               PERFORM UNTIL EOF-CTL
                   move spaces to WS-MCP-PARM, WS-MCP-VALUE
                   UNSTRING CTL-REC
                       DELIMITED BY '=' OR '**' OR X'0D0A' OR '   '
                       INTO WS-MCP-PARM, WS-MCP-VALUE
                   END-UNSTRING
                   IF WS-MCP-PARM = 'SYSIN1'
                       MOVE WS-MCP-VALUE TO WS-MCP-SYSIN1-PATH
                       OPEN INPUT IP-FILE1
                       IF WS-MCP-SYSIN1-STATUS NOT = '00'
                           MOVE +999 TO RETURN-CODE
                       END-IF
                   END-IF
                   IF WS-MCP-PARM = 'SYSOUT1'
                       MOVE WS-MCP-VALUE TO WS-MCP-SYSOUT1-PATH
                       OPEN OUTPUT OP-FILE1
                       IF WS-MCP-SYSOUT1-STATUS NOT = '00'
                           MOVE +999 TO RETURN-CODE
                       END-IF
                   END-IF
                   IF WS-MCP-PARM = 'SYSLOG'
                       MOVE WS-MCP-VALUE TO WS-SYSLOG-FILE-PATH
                       OPEN OUTPUT SYSLOG-FILE
                       IF WS-SYSLOG-FILE-STATUS NOT = '00'
                           MOVE +999 TO RETURN-CODE
                       END-IF
                   END-IF
                   IF WS-MCP-PARM = 'PARM1'
                       MOVE WS-MCP-VALUE TO WS-MAX-PAGES
                   END-IF

                   READ CTL-FILE
                       AT END MOVE 'Y' TO WS-EOF-CTL
                   END-READ
               END-PERFORM
           ELSE
               MOVE +999 TO RETURN-CODE
           END-IF.

           CLOSE CTL-FILE.

           IF RETURN-CODE = +999
               IF  WS-CONTROL-FILE-STATUS NOT = '00'
                   STRING 'BAD OPEN CTL. FILE STATUS '
                       DELIMITED BY SIZE
                       WS-CONTROL-FILE-STATUS
                       INTO SYSLOG-REC
                   DISPLAY SYSLOG-REC
               END-IF
               IF WS-SYSLOG-FILE-STATUS = '00'
                   IF  WS-MCP-SYSIN1-STATUS NOT = '00'
                       STRING 'BAD OPEN SYSIN1. FILE STATUS '
                           DELIMITED BY SIZE
                           WS-MCP-SYSIN1-STATUS
                           INTO SYSLOG-REC
                       WRITE SYSLOG-REC
                   END-IF
                   IF  WS-MCP-SYSOUT1-STATUS NOT = '00'
                       STRING 'BAD OPEN SYSOUT1. FILE STATUS '
                           DELIMITED BY SIZE
                           WS-MCP-SYSOUT1-STATUS
                           INTO SYSLOG-REC
                       WRITE SYSLOG-REC
                   END-IF
               ELSE
                   STRING 'BAD OPEN SYSLOG. FILE STATUS '
                       DELIMITED BY SIZE
                       WS-SYSLOG-FILE-STATUS
                       INTO SYSLOG-REC
                   DISPLAY SYSLOG-REC
               END-IF.



           IF RETURN-CODE NOT = +999
000160         PERFORM 700-READ.

000161*
000162****************
000163 200-PROCESS.
000164****************

           move IP-REC1 to op-rec1.
		   display op-rec1 (1:10)

           if op-rec1 (1:10) = '%FILENAME%'
               WRITE   OP-REC1
000177         ADD 1 TO WS-REC-CNTR-OUT
		   end-if.


000178     PERFORM 700-READ.
000179**
000180****************
000181 700-READ.
000182****************
000183     READ IP-FILE1
000184     AT END MOVE 'Y' TO WS-EOF.

000185     IF NOT END-OF-FILE
000186        ADD 1 TO WS-REC-CNTR-IN
000187     END-IF.


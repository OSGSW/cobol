       01  WS-MCP-SYSIN1-PATH              PIC X(300).
       01  WS-MCP-SYSIN2-PATH              PIC X(300).
       01  WS-MCP-SYSIN3-PATH              PIC X(300).
       01  WS-MCP-SYSIN4-PATH              PIC X(300).
       01  WS-MCP-SYSIN5-PATH              PIC X(300).
       01  WS-MCP-SYSIN6-PATH              PIC X(300).
       01  WS-MCP-SYSIN7-PATH              PIC X(300).
       01  WS-MCP-SYSIN8-PATH              PIC X(300).
       01  WS-MCP-SYSIN9-PATH              PIC X(300).
       01  WS-MCP-SYSIN1-STATUS.
           05  WS-MCP-SYSIN1-STATUS-01     PIC X(01) VALUE '9'.
           05  WS-MCP-SYSIN1-STATUS-02     PIC X(01) VALUE '9'.
       01  WS-MCP-SYSIN2-STATUS            PIC X(02) VALUE '99'.
       01  WS-MCP-SYSIN3-STATUS            PIC X(02) VALUE '99'.
       01  WS-MCP-SYSIN4-STATUS            PIC X(02) VALUE '99'.
       01  WS-MCP-SYSIN5-STATUS            PIC X(02) VALUE '99'.
       01  WS-MCP-SYSIN6-STATUS            PIC X(02) VALUE '99'.
       01  WS-MCP-SYSIN7-STATUS            PIC X(02) VALUE '99'.
       01  WS-MCP-SYSIN8-STATUS            PIC X(02) VALUE '99'.
       01  WS-MCP-SYSIN9-STATUS            PIC X(02) VALUE '99'.
       01  WS-MCP-SYSOUT1-PATH             PIC X(300).
       01  WS-MCP-SYSOUT2-PATH             PIC X(300).
       01  WS-MCP-SYSOUT3-PATH             PIC X(300).
       01  WS-MCP-SYSOUT4-PATH             PIC X(300).
       01  WS-MCP-SYSOUT5-PATH             PIC X(300).
       01  WS-MCP-SYSOUT6-PATH             PIC X(300).
       01  WS-MCP-SYSOUT7-PATH             PIC X(300).
       01  WS-MCP-SYSOUT8-PATH             PIC X(300).
       01  WS-MCP-SYSOUT9-PATH             PIC X(300).
       01  WS-MCP-SYSOUT1-STATUS           PIC X(02) VALUE '99'.
       01  WS-MCP-SYSOUT2-STATUS           PIC X(02) VALUE '99'.
       01  WS-MCP-SYSOUT3-STATUS           PIC X(02) VALUE '99'.
       01  WS-MCP-SYSOUT4-STATUS           PIC X(02) VALUE '99'.
       01  WS-MCP-SYSOUT5-STATUS           PIC X(02) VALUE '99'.
       01  WS-MCP-SYSOUT6-STATUS           PIC X(02) VALUE '99'.
       01  WS-MCP-SYSOUT7-STATUS           PIC X(02) VALUE '99'.
       01  WS-MCP-SYSOUT8-STATUS           PIC X(02) VALUE '99'.
       01  WS-MCP-SYSOUT9-STATUS           PIC X(02) VALUE '99'.
       01  WS-CONTROL-FILE-PATH        PIC X(300).
       01  WS-CONTROL-FILE-STATUS      PIC X(02) VALUE '99'.
       01  WS-SYSLOG-FILE-PATH         PIC X(300).
       01  WS-SYSLOG-FILE-STATUS       PIC X(02) VALUE '99'.
       01  WS-EOF-CTL                  PIC X VALUE 'N'.
               88  EOF-CTL                     VALUE 'Y'.
       01  WS-MCP-PARM                     PIC X(300).
       01  WS-MCP-VALUE                    PIC X(300).
       01  WS-MCP-VALUE-RD REDEFINES WS-MCP-VALUE.
           05  WS-MCP-VALUE-WT-01             PIC 9.
           05  FILLER                         PIC X.
           05  WS-MCP-VALUE-WT-02             PIC 9999.
           05  FILLER                  PIC X(294).
       01  WS-MCP-PAGE-WT                  PIC 9V9999.
       01  WS-MCP-ENV-WT                   PIC 9V9999.
       01  WS-MCP-INSERT01-WT              PIC 9V9999.
       01  WS-MCP-INSERT02-WT              PIC 9V9999.
       01  WS-MCP-INSERT03-WT              PIC 9V9999.
       01  WS-MCP-INSERT04-WT              PIC 9V9999.
       01  WS-MCP-INSERT05-WT              PIC 9V9999.
       01  WS-MCP-INSERT06-WT              PIC 9V9999.
       01  WS-MCP-INSERT01-TAG             PIC X(04).
       01  WS-MCP-INSERT02-TAG             PIC X(04).
       01  WS-MCP-INSERT03-TAG             PIC X(04).
       01  WS-MCP-INSERT04-TAG             PIC X(04).
       01  WS-MCP-INSERT05-TAG             PIC X(04).
       01  WS-MCP-INSERT06-TAG             PIC X(04).
       01  WS-MCP-INSERT01-FLAG            PIC X(01).
       01  WS-MCP-INSERT02-FLAG            PIC X(01).
       01  WS-MCP-INSERT03-FLAG            PIC X(01).
       01  WS-MCP-INSERT04-FLAG            PIC X(01).
       01  WS-MCP-INSERT05-FLAG            PIC X(01).
       01  WS-MCP-INSERT06-FLAG            PIC X(01).
       01  WS-MCP-TOTAL-WT                 PIC 99V9999.
       01  WS-MCP-WORKORDER                PIC X(12).

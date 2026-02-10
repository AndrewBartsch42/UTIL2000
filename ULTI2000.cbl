       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. UTIL2000.                                            00020001
      ***************************************************************** 00030000
      *  Programmer.:ANDREW BARTSCH                                     00040001
      *  Date.......:2/10/2026                                          00050001
      *  GitHub URL.:                                                   00060000
      *  Description:                                                   00070000
      ***************************************************************** 00080000
       DATA DIVISION.                                                   00090000
       WORKING-STORAGE SECTION.                                         00100000
                                                                        00110000
      ***************************************************************** 00120000
      * CONSTANTS                                                       00130000
      ***************************************************************** 00140000
       01  WS-RATE-TIER1            PIC V99      VALUE .12.             00150000
       01  WS-RATE-TIER2            PIC V99      VALUE .15.             00160000
       01  WS-RATE-TIER3            PIC V99      VALUE .18.             00170000
       01  WS-TIER1-LIMIT           PIC 9(4)     VALUE 500.             00180000
       01  WS-TIER2-LIMIT           PIC 9(4)     VALUE 500.             00190004
                                                                        00200000
      ***************************************************************** 00210000
      * 3 PREDEFINED CUSTOMERS (NO TABLES)                              00220000
      ***************************************************************** 00230000
       01  WS-CUST1.                                                    00240000
           05  WS-C1-NAME           PIC X(12)   VALUE 'CUST-ALPHA  '.   00250000
           05  WS-C1-KWH            PIC 9(5)    VALUE 350.              00260000
           05  WS-C1-FEE            PIC 9(3)V99 VALUE 14.95.            00270000
                                                                        00280000
       01  WS-CUST2.                                                    00290001
           05  WS-C2-NAME           PIC X(12)   VALUE 'CUST-BETA   '.   00300001
           05  WS-C2-KWH            PIC 9(5)    VALUE 925.              00310001
           05  WS-C2-FEE            PIC 9(3)V99 VALUE 14.95.            00320001
                                                                        00330001
       01  WS-CUST3.                                                    00340001
           05  WS-C3-NAME           PIC X(12)   VALUE 'CUST-CHARLIE'.   00350001
           05  WS-C3-KWH            PIC 9(5)    VALUE 1350.             00360001
           05  WS-C3-FEE            PIC 9(3)V99 VALUE 14.95.            00370001
                                                                        00380001
      ***************************************************************** 00390000
      * CURRENT "INPUT" FIELDS (LOADED PER CUSTOMER)                    00400000
      ***************************************************************** 00410000
       01  WS-CUST-NAME             PIC X(12)   VALUE SPACES.           00420000
       01  WS-KWH-USED              PIC 9(5)    VALUE 0.                00430000
       01  WS-SERVICE-FEE           PIC 9(3)V99 VALUE 0.                00440000
                                                                        00450000
      ***************************************************************** 00460000
      * WORK AREAS                                                      00470000
      ***************************************************************** 00480000
       01  WS-TIER1-KWH             PIC 9(5)     VALUE 0.               00490000
       01  WS-TIER2-KWH             PIC 9(5)     VALUE 0.               00500000
       01  WS-TIER3-KWH             PIC 9(5)     VALUE 0.               00510000
                                                                        00520000
       01  WS-TIER1-CHARGE          PIC 9(5)V99  VALUE 0.               00530000
       01  WS-TIER2-CHARGE          PIC 9(5)V99  VALUE 0.               00540000
       01  WS-TIER3-CHARGE          PIC 9(5)V99  VALUE 0.               00550000
                                                                        00560000
       01  WS-SUBTOTAL              PIC 9(6)V99  VALUE 0.               00570000
       01  WS-TOTAL-BILL            PIC 9(6)V99  VALUE 0.               00580000
                                                                        00590000
      ***************************************************************** 00600000
      * EDITED FIELDS FOR DISPLAY                                       00610000
      ***************************************************************** 00620000
       01  WS-KWH-USED-ED           PIC Z,ZZZ,ZZZ,ZZ9.                  00630000
       01  WS-MONEY-ED              PIC $$,$$$,$$9.99.                  00640000
       01  WS-MONEY-ED2             PIC $$,$$$,$$9.99.                  00650000
                                                                        00660000
      ***************************************************************** 00670000
      * IT'S GO TIME!                                                   00680000
      ***************************************************************** 00690000
       PROCEDURE DIVISION.                                              00700000
                                                                        00710000
      ***************************************************************** 00720000
      * MAINLINE - DISPLAY HEADING, LOAD CUSTOMER, RUN BILL, STOP       00730000
      ***************************************************************** 00740000
       000-MAIN.                                                        00750000
           DISPLAY '*************************************'.             00760001
           DISPLAY '*** UTIL2000 - ALL CUSTOMER BILLS ***'.             00770001
           DISPLAY '*************************************'.             00780001
           DISPLAY ' '.                                                 00790000
                                                                        00800000
           PERFORM 500-LOAD-CUST1.                                      00810001
           PERFORM 600-RUN-BILL.                                        00820000
                                                                        00830000
           PERFORM 510-LOAD-CUST2.                                      00840001
           PERFORM 600-RUN-BILL.                                        00850001
                                                                        00860001
           PERFORM 520-LOAD-CUST3.                                      00870001
           PERFORM 600-RUN-BILL.                                        00880002
           STOP RUN.                                                    00890000
                                                                        00900000
      ***************************************************************** 00910000
      * MOVE name/kwh/fee from CUST into current fields.                00920000
      ***************************************************************** 00930000
       500-LOAD-CUST1.                                                  00940001
           MOVE WS-C1-NAME TO WS-CUST-NAME.                             00950000
           MOVE WS-C1-KWH  TO WS-KWH-USED.                              00960000
           MOVE WS-C1-FEE  TO WS-SERVICE-FEE.                           00970000
                                                                        00980000
       510-LOAD-CUST2.                                                  00990001
           MOVE WS-C2-NAME TO WS-CUST-NAME.                             01000001
           MOVE WS-C2-KWH  TO WS-KWH-USED.                              01010001
           MOVE WS-C2-FEE  TO WS-SERVICE-FEE.                           01020001
                                                                        01030001
       520-LOAD-CUST3.                                                  01040001
           MOVE WS-C3-NAME TO WS-CUST-NAME.                             01050001
           MOVE WS-C3-KWH  TO WS-KWH-USED.                              01060001
           MOVE WS-C3-FEE  TO WS-SERVICE-FEE.                           01070001
                                                                        01080001
      ***************************************************************** 01090000
      * BILL ROUTINE                                                    01100000
      ***************************************************************** 01110000
       600-RUN-BILL.                                                    01120000
           PERFORM 100-INITIALIZE.                                      01130000
           PERFORM 200-CALC-TIERS.                                      01140000
           PERFORM 300-CALC-CHARGES.                                    01150000
           PERFORM 400-DISPLAY-RESULTS.                                 01160000
           DISPLAY ' '.                                                 01170000
                                                                        01180000
      ***************************************************************** 01190000
      * Zero tier kWh, charges, subtotal, total                         01200000
      ***************************************************************** 01210000
       100-INITIALIZE.                                                  01220000
           MOVE 0 TO WS-TIER1-KWH                                       01230000
                    WS-TIER2-KWH                                        01240000
                    WS-TIER3-KWH                                        01250000
                    WS-TIER1-CHARGE                                     01260000
                    WS-TIER2-CHARGE                                     01270000
                    WS-TIER3-CHARGE                                     01280000
                    WS-SUBTOTAL                                         01290000
                    WS-TOTAL-BILL.                                      01300000
                                                                        01310000
      ***************************************************************** 01320000
      * Determine WS-TIER1-KWH, WS-TIER2-KWH, WS-TIER3-KWH              01330000
      * based on WS-KWH-USED                                            01340000
      *                                                                 01350000
      * These are the per-kWh rates:                                    01360000
      * - Tier 1: first 500 kWh at $0.12/kWh                            01370000
      * - Tier 2: next 500 kWh (kWh 501 1000) at $0.15/kWh              01380000
      * - Tier 3: any kWh above 1000 at $0.18/kWh                       01390000
      ***************************************************************** 01400000
       200-CALC-TIERS.                                                  01410000
           *> If amount used is less than 500 kWh, all goes in tier 1   01420000
           IF WS-KWH-USED <= WS-TIER1-LIMIT                             01430000
               MOVE WS-KWH-USED TO WS-TIER1-KWH                         01440000
               MOVE 0 TO WS-TIER2-KWH WS-TIER3-KWH                      01450000
           ELSE                                                         01460000
               MOVE WS-TIER1-LIMIT TO WS-TIER1-KWH                      01470000
                                                                        01480000
               *> If amount used is between 501 and 1000 kWh,           01490000
               *> tier 1 is full, remainder goes in tier 2              01500000
               IF WS-KWH-USED <= (WS-TIER1-LIMIT + WS-TIER2-LIMIT)      01510000
                   COMPUTE WS-TIER2-KWH =                               01520000
                       WS-KWH-USED - WS-TIER1-LIMIT                     01530000
                   MOVE 0 TO WS-TIER3-KWH                               01540000
                                                                        01550000
               *> If amount used is between 1001 and above,             01560000
               *> tier 1 and tier 2 are full, remainder goes in tier 3  01570000
               ELSE                                                     01580000
                   MOVE WS-TIER2-LIMIT TO WS-TIER2-KWH                  01590000
                   COMPUTE WS-TIER3-KWH =                               01600000
                       WS-KWH-USED - WS-TIER1-LIMIT - WS-TIER2-LIMIT    01610000
               END-IF                                                   01620000
           END-IF.                                                      01630000
                                                                        01640000
      ***************************************************************** 01650000
      * COMPUTE charges using ROUNDED and compute totals.               01660000
      ***************************************************************** 01670000
       300-CALC-CHARGES.                                                01680000
           COMPUTE WS-TIER1-CHARGE ROUNDED =                            01690000
               WS-TIER1-KWH * WS-RATE-TIER1.                            01700000
                                                                        01710000
           COMPUTE WS-SUBTOTAL = WS-TIER1-CHARGE.                       01720000
                                                                        01730000
           COMPUTE WS-TIER2-CHARGE ROUNDED =                            01731004
               WS-TIER2-KWH * WS-RATE-TIER2.                            01732004
                                                                        01733004
           COMPUTE WS-SUBTOTAL = WS-TIER2-CHARGE + WS-SUBTOTAL.         01734004
                                                                        01735004
           COMPUTE WS-TIER3-CHARGE ROUNDED =                            01736004
               WS-TIER3-KWH * WS-RATE-TIER3.                            01737004
                                                                        01738004
           COMPUTE WS-SUBTOTAL = WS-TIER3-CHARGE + WS-SUBTOTAL.         01739004
                                                                        01739104
           COMPUTE WS-TOTAL-BILL =                                      01740000
               WS-SUBTOTAL + WS-SERVICE-FEE.                            01750000
                                                                        01760000
      ***************************************************************** 01770000
      * Display report including customer name.                         01780000
      ***************************************************************** 01790000
       400-DISPLAY-RESULTS.                                             01800000
           MOVE WS-KWH-USED TO WS-KWH-USED-ED.                          01810000
                                                                        01820000
           DISPLAY '--------------------------------'.                  01830000
           DISPLAY 'CUSTOMER: ' WS-CUST-NAME.                           01840000
           DISPLAY '--------------------------------'.                  01850000
           DISPLAY 'KWH USED       : ' WS-KWH-USED-ED.                  01860000
                                                                        01870000
           MOVE WS-SERVICE-FEE TO WS-MONEY-ED.                          01880000
           DISPLAY 'SERVICE FEE    : ' WS-MONEY-ED.                     01890000
                                                                        01900000
           MOVE WS-TIER1-CHARGE TO WS-MONEY-ED.                         01910000
           DISPLAY 'TIER 1 CHARGE  : ' WS-MONEY-ED.                     01920000
                                                                        01930000
           MOVE WS-TIER2-CHARGE TO WS-MONEY-ED.                         01940000
           DISPLAY 'TIER 2 CHARGE  : ' WS-MONEY-ED.                     01950000
                                                                        01960000
           MOVE WS-TIER3-CHARGE TO WS-MONEY-ED.                         01970000
           DISPLAY 'TIER 3 CHARGE  : ' WS-MONEY-ED.                     01980000
                                                                        01990000
           MOVE WS-TOTAL-BILL TO WS-MONEY-ED2.                          02000000
           DISPLAY '--------------------------------'.                  02010000
           DISPLAY 'TOTAL BILL     : ' WS-MONEY-ED2.                    02020000
           DISPLAY '--------------------------------'.                  02030000

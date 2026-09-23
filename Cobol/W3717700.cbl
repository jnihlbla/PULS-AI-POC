000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3717700.                                                
000300 AUTHOR.         GAVIN SMITH.                                             
000400 DATE-WRITTEN.   99/12/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        THIS PROGRAM TAKES DISTRICT/ACCOUNT INPUT SUMMARY FILE           
001000*        PLUS WEEKS TRANSACTION FILE AND CREATES REPORTS FOR EACH         
001100*        CONSOLIDATED CUSTOMER.                                           
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800*    CHANGE LOG:                                                          
001900*                                                                         
002000*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
002100*      ----------------------------------------------------------         
002200*      15/07/02 - REDDY RAHUL     - E'TRACKER 10251645.                   
002300*                                   ADD ADDITIONAL INFORMATION AND        
002400*                                   IMPROVE THE REPORT SENT TO            
002500*                                   IMPORTERS.                            
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500*          --- CUSTOMER/ACCOUNT PARAMETER SUMMARY FILE                    
003600     SELECT W37131                     ASSIGN TO W37177D1.                
003700     SKIP2                                                                
003800*          --- WEEKS SUT SUP RET FAK KRE ADJ TRANSACTIONS                 
003900     SELECT W37141                     ASSIGN TO W37177D2.                
004000     SKIP2                                                                
004100*          --- REPORT FILE                                                
004200     SELECT W37177                     ASSIGN TO W37177D3.                
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500     SKIP3                                                                
004600 FILE SECTION.                                                            
004700     SKIP3                                                                
004800 FD  W37131                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  -COPY W37107      -L.                                                
005300     SKIP3                                                                
005400 FD  W37141                                                               
005500     RECORDING       F                                                    
005600     BLOCK CONTAINS  0.                                                   
005700                                                                          
005800*01  -COPY W37109      -L.                                                
005900     SKIP3                                                                
006000 FD  W37177                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01  POST -COPY W37177 -PRE  REP-  -L.                                    
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700                                                                          
006800                                                                          
006900*    -- CHECKED BY WY2000                                                 
007000 77  IDPGM                       PIC X(8)    VALUE 'W3717700'.            
007100 77  JA                          PIC X       VALUE 'J'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
007300                                                                          
007400 77  W37131-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W37131                       VALUE 'J'.                   
007600                                                                          
007700 77  W37141-EOF-SW               PIC X       VALUE 'N'.                   
007800     88  END-OF-W37141                       VALUE 'J'.                   
007900     EJECT                                                                
008000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008100 01  FILLER REDEFINES DAGENS-DATUM.                                       
008200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008500     EJECT                                                                
008600 01  DYNAMISKA-SUBPROGRAM.                                                
008700*                                                                         
008800       03  ABEND                   PIC X(8)    VALUE 'ABEND'.             
008900       03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.           
009000       03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.            
009100       03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.           
009200     SKIP2                                                                
009300*    --- PARAMETRAR TILL ABEND                                            
009400                                                                          
009500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009800     SKIP2                                                                
009900 01  FELTEXT.                                                             
010000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL POSTSUM                                          
010400*                                                                         
010500*01  -COPY W0005   -PRE  POSTSUM-                                         
010600     EJECT                                                                
010700 01  IN01-AREA-START             PIC X(24)   VALUE                        
010800                                 'IN01-AREA-START  '.                     
010900     SKIP2                                                                
011000                                                                          
011100*01  AREA -COPY W37107     -PRE IN01-                                     
011200     EJECT                                                                
011300 01  IN02-AREA-START            PIC X(24)   VALUE                         
011400                                 'IN02-AREA-START  '.                     
011500     SKIP2                                                                
011600                                                                          
011700*01  AREA -COPY W37109     -PRE IN02-                                     
011800     EJECT                                                                
011900 01  REP-AREA-START              PIC X(24)   VALUE                        
012000                                 'REP-AREA-START  '.                      
012100     SKIP2                                                                
012200                                                                          
012300*01  AREA -COPY W37177     -PRE REP-                                      
012400     EJECT                                                                
012500 01  W-SUMMERINGS-FALT.                                                   
012600     03   WTOT-SUPOINT-ING  PIC S9(9) VALUE ZERO.                         
012700     03   WTOT-SUPOINT-RET  PIC S9(9) VALUE ZERO.                         
012800     03   WTOT-SUPOINT-FAK  PIC S9(9) VALUE ZERO.                         
012900     03   WTOT-SUPOINT-ADJ  PIC S9(9) VALUE ZERO.                         
013000     03   WTOT-SUPOINT-UTG  PIC S9(9) VALUE ZERO.                         
013100     03   WTOT-SUPOINT-RIT  PIC S9(9) VALUE ZERO.                         
013200     03   WTOT-SUPOINT-PP   PIC S9(9) VALUE ZERO.                         
013300     03   WTOT-SUPOINT      PIC S9(9) VALUE ZERO.                         
013400 01  W-STRING-KEYS.                                                       
013500     03 W-STRINGS .                                                       
013600       05 WS-IDDISTR-BET   PIC  9(5) VALUE ZERO.                          
013700       05 WS-KDEXCHA       PIC  9(3) VALUE ZERO.                          
013800 01  W-REPORT-KEYS.                                                       
013900     03 W-SAVE-KEY .                                                      
014000       05 W-KDEXCHA       PIC S9(3) COMP-3.                               
014100       05 W-IDDISTR-BET   PIC S9(5) COMP-3.                               
014200     03 W-IDPTYP        PIC X(3).                                         
014300     03 W-FLEXCREP      PIC X(1).                                         
014400     03 W-FLEXCDET      PIC X(1).                                         
014500     03 W-CURRENT-KEY.                                                    
014600       05 WL-KDEXCHA       PIC S9(3) COMP-3.                              
014700       05 WL-IDDISTR-BET   PIC S9(5) COMP-3.                              
014800     03 BREAKFLAG       PIC X.                                            
014900     03 W-WEEK          PIC 99.                                           
015000 01  W-REPORT-LAYOUT.                                                     
015100     03 T-RUB01.                                                          
015200       05 FILLER           PIC X(6) VALUE 'WEEK:'.                        
015300       05 T-DAAAVV         PIC 9(6).                                      
015400       05 FILLER           PIC X(12)  VALUE '  DISTRICT: '.               
015500       05 T-IDDISTR-BET    PIC Z(3)9.                                     
015600       05 FILLER           PIC X(11)  VALUE '  ACCOUNT: '.                
015700       05 T-KDEXCHA        PIC Z(2)9.                                     
015800       05 FILLER   PIC X(38) VALUE '        BALANCE SHEET  '.             
015900     03 T-RUB02.                                                          
016000       05 FILLER           PIC X(80) VALUE SPACE.                         
016100     03 TOMRAD.                                                           
016200       05 FILLER           PIC X(80) VALUE SPACE.                         
016300     03 T-RUB03A.                                                         
016400       05 FILLER           PIC X(40) VALUE                                
016500               'DIST  OPENING    WEEKS    WEEKS    WEEKS'.                
016600       05 FILLER           PIC X(40) VALUE                                
016700               '  CLOSING   RETURNS INVOICES            '.                
016800     03 T-RUB03B.                                                         
016900       05 FILLER           PIC X(40) VALUE                                
017000               'RICT  BALANCE  RETURNS  DELIVER   ADJUST'.                
017100       05 FILLER           PIC X(40) VALUE                                
017200               '  BALANCE   TRANSIT  PENDING            '.                
017300     03 T-RAD01.                                                          
017400       05 T-IDDISTR        PIC Z(3)9.                                     
017500*      05 FILLER           PIC X     VALUE SPACE.                         
017600       05 T-SUPOINT-ING    PIC -(8)9.                                     
017700**     05 FILLER           PIC XX    VALUE SPACE.                         
017800       05 T-SUPOINT-RET    PIC -(8)9.                                     
017900**     05 FILLER           PIC XX    VALUE SPACE.                         
018000       05 T-SUPOINT-FAK    PIC -(8)9.                                     
018100*      05 FILLER           PIC X     VALUE SPACE.                         
018200       05 T-SUPOINT-ADJ    PIC -(8)9.                                     
018300*      05 FILLER           PIC X     VALUE SPACE.                         
018400       05 T-SUPOINT-UTG    PIC -(8)9.                                     
018500       05 FILLER           PIC X     VALUE SPACE.                         
018600       05 T-SUPOINT-RIT    PIC -(8)9.                                     
018700**     05 FILLER           PIC XX    VALUE SPACE.                         
018800       05 T-SUPOINT-PP     PIC -(8)9.                                     
018900**     05 FILLER           PIC XX    VALUE SPACE.                         
019000     03 T-TOT01.                                                          
019100       05 FILLER           PIC X(4) VALUE 'TOT.'.                         
019200*      05 FILLER           PIC X     VALUE SPACE.                         
019300       05 TTOT-SUPOINT-ING PIC -(8)9.                                     
019400*      05 FILLER           PIC XX    VALUE SPACE.                         
019500       05 TTOT-SUPOINT-RET PIC -(8)9.                                     
019600*      05 FILLER           PIC XX    VALUE SPACE.                         
019700       05 TTOT-SUPOINT-FAK PIC -(8)9.                                     
019800*      05 FILLER           PIC X     VALUE SPACE.                         
019900       05 TTOT-SUPOINT-ADJ PIC -(8)9.                                     
020000*      05 FILLER           PIC X     VALUE SPACE.                         
020100       05 TTOT-SUPOINT-UTG PIC -(8)9.                                     
020200       05 FILLER           PIC X     VALUE SPACE.                         
020300       05 TTOT-SUPOINT-RIT PIC -(8)9.                                     
020400*      05 FILLER           PIC XX    VALUE SPACE.                         
020500       05 TTOT-SUPOINT-PP  PIC -(8)9.                                     
020600*      05 FILLER           PIC XX    VALUE SPACE.                         
020700     03 D-RUB01.                                                          
020800       05 FILLER           PIC X(80) VALUE 'DELIVERIES:'.                 
020900     03 D-RUB02.                                                          
021000       05 FILLER           PIC X(40) VALUE                                
021100       'DIST ORDERNO   DATE TYP    PARTNO DESCRI'.                        
021200       05 FILLER           PIC X(40) VALUE                                
021300       'PTION             QTY POINTS     TOTAL  '.                        
021400     03 D-RAD01.                                                          
021500       05 D-IDDISTR       PIC Z(3)9.                                      
021600       05 FILLER          PIC X    VALUE SPACE.                           
021700       05 D-IDORDER       PIC Z(6)9.                                      
021800       05 FILLER          PIC X    VALUE SPACE.                           
021900       05 D-TIDAT         PIC Z(6).                                       
022000       05 FILLER          PIC X    VALUE SPACE.                           
022100       05 D-IDPTYP        PIC X(3).                                       
022200       05 FILLER          PIC X    VALUE SPACE.                           
022300       05 D-IDARTNR       PIC Z(9).                                       
022400       05 FILLER          PIC X    VALUE SPACE.                           
022500       05 D-BEART-ENG     PIC X(18).                                      
022600       05 FILLER          PIC X    VALUE SPACE.                           
022700       05 D-KVANTAL       PIC Z(7).                                       
022800       05 FILLER          PIC X    VALUE SPACE.                           
022900       05 D-KVPOINT       PIC -(8).                                       
023000*      05 FILLER          PIC X    VALUE SPACE.                           
023100       05 D-SUPOINT       PIC -(9).                                       
023200     03 D-TOT01.                                                          
023300       05 FILLER          PIC X(69)  VALUE 'TOTAL'.                       
023400       05 DTOT-SUPOINT    PIC -(9).                                       
023500     03 R-RUB01.                                                          
023600       05 FILLER           PIC X(80) VALUE 'RETURNS:'.                    
023700     03 R-RUB02A.                                                         
023800       05 FILLER           PIC X(45) VALUE                                
023900       'DIST           RETURN                        '.                   
024000       05 FILLER           PIC X(50) VALUE                                
024100       '      QTY.   QTY.                DC               '.              
024200     03 R-RUB02B.                                                         
024300       05 FILLER           PIC X(45) VALUE                                
024400       'RICT CUSTOMER      ID    PARTNO DESCRIPTION  '.                   
024500       05 FILLER           PIC X(50) VALUE                                
024600       '      RETURN APPROV   POINTS STA CODE     TOTAL   '.              
024700     03 R-RAD01.                                                          
024800       05 R1-IDDISTR       PIC  Z(4).                                     
024900       05 FILLER           PIC XX   VALUE SPACE.                          
025000       05 R1-IDKUNDNR      PIC  Z(6)9(1).                                 
025100       05 FILLER           PIC X    VALUE SPACE.                          
025200       05 R1-IDBYTRAP      PIC  Z(7).                                     
025300       05 FILLER           PIC X    VALUE SPACE.                          
025400       05 R1-IDARTNR       PIC  Z(9).                                     
025500       05 FILLER           PIC X    VALUE SPACE.                          
025600       05 R1-BEART-ENG     PIC  X(18).                                    
025700       05 FILLER           PIC X    VALUE SPACE.                          
025800       05 R1-KVANTAL       PIC  Z(6).                                     
025900       05 FILLER           PIC X    VALUE SPACE.                          
026000       05 R1-KVRETUR-GODK  PIC  Z(6).                                     
026100       05 FILLER           PIC X    VALUE SPACE.                          
026200       05 R1-KVPOINT       PIC  -(8).                                     
026300       05 FILLER           PIC X    VALUE SPACE.                          
026400       05 R1-KDBYTSTA-OBJ  PIC     X.                                     
026500       05 FILLER           PIC XXX  VALUE SPACE.                          
026600       05 R1-KDBYTREF      PIC  X(3).                                     
026700       05 FILLER           PIC XX   VALUE SPACE.                          
026800       05 R1-SUPOINT       PIC  -(9).                                     
026900     03 R-TOT01.                                                          
027000       05 FILLER           PIC X(83)  VALUE 'TOTAL'.                      
027100       05 RTOT-SUPOINT     PIC -(9).                                      
027200     03 A-RUB01.                                                          
027300       05 FILLER           PIC X(80) VALUE 'ADJUSTMENTS'.                 
027400     03 A-RUB02.                                                          
027500       05 FILLER           PIC X(40) VALUE                                
027600       'DIST   DATE COMMENT                     '.                        
027700       05 FILLER           PIC X(40) VALUE                                
027800       '                                 TOTAL  '.                        
027900     03 A-RAD01.                                                          
028000       05 A-IDDISTR         PIC Z(4).                                     
028100       05 FILLER            PIC X VALUE SPACE.                            
028200       05 A-TIDAT           PIC Z(6).                                     
028300       05 FILLER            PIC X VALUE SPACE.                            
028400       05 A-TENOTE          PIC X(40).                                    
028500       05 FILLER            PIC X(17) VALUE SPACE.                        
028600       05 A-SUPOINT         PIC ---------.                                
028700       05 FILLER            PIC X VALUE SPACE.                            
028800     03 A-TOT01.                                                          
028900       05 FILLER            PIC X(69)  VALUE 'TOTAL'.                     
029000       05 ATOT-SUPOINT      PIC ---------.                                
029100     03 F-FOT01.                                                          
029200       05 FILLER            PIC X(20) VALUE 'NEXT INVOICE WEEK'.          
029300       05 F-DAAVV           PIC 9(6)  VALUE 999999.                       
029400       05 FILLER            PIC X(54) VALUE SPACE.                        
029500     SKIP2                                                                
029600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
029700     EJECT                                                                
029800 01 FILLER                      PIC X(16)   VALUE 'IMS-WS'.               
029900     SKIP3                                                                
030000 01 NYCKLAR-TILL-DLI.                                                     
030100     03  W-WDGXKEY-3155-X.                                                
030200       05  W-IDHTYP-3155       PIC X(4)    VALUE '3155'.                  
030300       05  FILLER              PIC X(26)   VALUE LOW-VALUE.               
030400     03  W-WDGXKEY-3156-X.                                                
030500       05  W-KDSEGKEY-3156     PIC X(1)    VALUE '1'.                     
030600*---STATUS-KOD FRÅN IMS------                                             
030700 01 STATUS-WS    PIC XX.                                                  
030800     88 SEGMENT-FINNS      VALUE ' '.                                     
030900     SKIP2                                                                
031000 01 GODK-STATUSKODER.                                                     
031100     03 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
031200     SKIP3                                                                
031300 01 SSA1   PIC X(64).                                                     
031400 01 SSA2   PIC X(64).                                                     
031500     EJECT                                                                
031600* ---IMS FUNKTIONSKODER                                                   
031700*01 -COPY W0003                                                           
031800     EJECT                                                                
031900* DLI-INPUT-OUTPUT AREA                                                   
032000 01 FILLER   PIC X(16) VALUE 'DLI-IO-WDGX3156'.                           
032100 01 DLI-IO-WDGX3156.                                                      
032200*    03 -COPY WDGX3156                                                    
032300     EJECT                                                                
032400 LINKAGE SECTION.                                                         
032500*01 -COPY W0008 -PRE 3156-                                                
032600     05 FILLER PIC X.                                                     
032700 PROCEDURE DIVISION USING 3156-PCB.                                       
032800 MAIN SECTION.                                                            
032900     ENTRY 'DLITCBL' USING 3156-PCB.                                      
033000     SKIP2                                                                
033100                                                                          
033200     PERFORM A-INIT                                                       
033300     PERFORM S01-LAES-W37131                                              
033400     PERFORM S02-LAES-W37141                                              
033500     PERFORM IMS-GU-WDGX3156 .                                            
033600*CALCULATE NEXT INVOICE DATE                                              
033700     MOVE 3156-TIVECKNR-BYTDEB(1) TO W-WEEK                               
033800     IF IN01-DAAAVV (5:2) <= W-WEEK  AND                                  
033900        3156-TIVECKNR-BYTDEB(1) > 0                                       
034000      MOVE IN01-DAAAVV TO F-DAAVV                                         
034100      MOVE W-WEEK      TO F-DAAVV(5:2)                                    
034200     ELSE                                                                 
034300      MOVE 3156-TIVECKNR-BYTDEB(2) TO W-WEEK                              
034400      IF IN01-DAAAVV (5:2) <= W-WEEK    AND                               
034500        3156-TIVECKNR-BYTDEB(2) > 0                                       
034600       MOVE IN01-DAAAVV TO F-DAAVV                                        
034700       MOVE W-WEEK      TO F-DAAVV(5:2)                                   
034800      ELSE                                                                
034900       MOVE 3156-TIVECKNR-BYTDEB(3) TO W-WEEK                             
035000       IF IN01-DAAAVV (5:2) <= W-WEEK   AND                               
035100        3156-TIVECKNR-BYTDEB(3) > 0                                       
035200        MOVE IN01-DAAAVV TO F-DAAVV                                       
035300        MOVE W-WEEK      TO F-DAAVV(5:2)                                  
035400       ELSE                                                               
035500       MOVE 3156-TIVECKNR-BYTDEB(4) TO W-WEEK                             
035600        IF IN01-DAAAVV (5:2) <= W-WEEK   AND                              
035700        3156-TIVECKNR-BYTDEB(4) > 0                                       
035800         MOVE IN01-DAAAVV TO F-DAAVV                                      
035900         MOVE W-WEEK TO F-DAAVV(5:2)                                      
036000        ELSE                                                              
036100         MOVE 3156-TIVECKNR-BYTDEB(1) TO W-WEEK                           
036200         MOVE IN01-DAAAVV TO F-DAAVV                                      
036300         ADD 100 TO F-DAAVV                                               
036400         MOVE W-WEEK TO F-DAAVV(5:2)                                      
036500        END-IF                                                            
036600       END-IF                                                             
036700      END-IF                                                              
036800     END-IF                                                               
036900****END OF CALC-INVOICEDATE                                               
037000     PERFORM UNTIL END-OF-W37131                                          
037100       IF IN01-FLEXCREP = 'Y'                                             
037200*UPDATE KEYS                                                              
037300         MOVE IN01-IDDISTR-BET TO W-IDDISTR-BET                           
037400                                  WS-IDDISTR-BET                          
037500         MOVE IN01-KDEXCHA     TO W-KDEXCHA                               
037600                                  WS-KDEXCHA                              
037700         MOVE IN01-FLEXCDET    TO W-FLEXCDET                              
037800         MOVE IN01-FLEXCREP    TO W-FLEXCREP                              
037900         MOVE 'N'              TO BREAKFLAG                               
038000*UPDATE MEMO                                                              
038100         MOVE ')SEND'          TO REP-AREA                                
038200         PERFORM S11-SKRIV-W37177                                         
038300         STRING 'TITLE ' WS-IDDISTR-BET '-'                               
038400             WS-KDEXCHA '-' IN01-DAAAVV(3:4)                              
038500             DELIMITED BY SIZE INTO REP-AREA                              
038600         PERFORM S11-SKRIV-W37177                                         
038700         MOVE 'OPTION FORCE'   TO REP-AREA                                
038800         PERFORM S11-SKRIV-W37177                                         
038900         MOVE 'LINESIZE 95'    TO REP-AREA                                
039000         PERFORM S11-SKRIV-W37177                                         
039100         STRING 'DEST ' 3156-IDMAIL                                       
039200             DELIMITED BY SIZE INTO REP-AREA                              
039300         PERFORM S11-SKRIV-W37177                                         
039400         IF IN01-IDMAIL NOT = SPACE                                       
039500           STRING 'DEST ' IN01-IDMAIL                                     
039600               DELIMITED BY SIZE INTO REP-AREA                            
039700           PERFORM S11-SKRIV-W37177                                       
039800         END-IF                                                           
039900         MOVE 'MEMO' TO REP-AREA                                          
040000         PERFORM S11-SKRIV-W37177                                         
040100*SKAPA REPORT                                                             
040200         PERFORM B-SKAPA-REPORT                                           
040300*CLOSE MEMO                                                               
040400         MOVE ')END' TO REP-AREA                                          
040500         PERFORM S11-SKRIV-W37177                                         
040600       ELSE                                                               
040700         PERFORM S01-LAES-W37131                                          
040800       END-IF                                                             
040900     END-PERFORM                                                          
041000                                                                          
041100                                                                          
041200     PERFORM Z-FINIT                                                      
041300                                                                          
041400     MOVE ZERO TO RETURN-CODE                                             
041500     GOBACK                                                               
041600     .                                                                    
041700     EJECT                                                                
041800 A-INIT SECTION.                                                          
041900                                                                          
042000     OPEN INPUT  W37131                                                   
042100                 W37141                                                   
042200                                                                          
042300     OPEN OUTPUT W37177                                                   
042400     SKIP2                                                                
042500     ACCEPT DAGENS-DATUM  FROM DATE                                       
042600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
042700     .                                                                    
042800     EJECT                                                                
042900 B-SKAPA-REPORT SECTION.                                                  
043000*** SKRIV RUBRIK                                                          
043100     MOVE TOMRAD TO           REP-AREA                                    
043200     PERFORM S11-SKRIV-W37177                                             
043300     MOVE IN01-IDDISTR-BET TO T-IDDISTR-BET                               
043400     MOVE IN01-KDEXCHA     TO T-KDEXCHA                                   
043500     MOVE IN01-DAAAVV      TO T-DAAAVV                                    
043600     MOVE T-RUB01          TO  REP-AREA                                   
043700     PERFORM S11-SKRIV-W37177                                             
043800*** SKRIV SUMMERINGS SEKTION                                              
043900***    SKRIV SUMRUBRIK1                                                   
044000     MOVE T-RUB02          TO  REP-AREA                                   
044100     PERFORM S11-SKRIV-W37177                                             
044200***    SKRIV SUMRUBRIK2                                                   
044300     MOVE T-RUB03A         TO  REP-AREA                                   
044400     PERFORM S11-SKRIV-W37177                                             
044500     MOVE T-RUB03B         TO  REP-AREA                                   
044600     PERFORM S11-SKRIV-W37177                                             
044700***    PERFORM SUMLINES                                                   
044800*    PERFORM UNTIL BREAKFLAG = 'Y'                                        
044900     PERFORM UNTIL END-OF-W37131                        OR                
045000                   IN01-IDDISTR-BET NOT = W-IDDISTR-BET OR                
045100                   IN01-KDEXCHA     NOT = W-KDEXCHA                       
045200***       SKRIV SUMLINE DETALJER                                          
045300       MOVE IN01-IDDISTR     TO T-IDDISTR                                 
045400       MOVE IN01-SUPOINT-ING TO T-SUPOINT-ING                             
045500       MOVE IN01-SUPOINT-RET TO T-SUPOINT-RET                             
045600       MOVE IN01-SUPOINT-FAK TO T-SUPOINT-FAK                             
045700       MOVE IN01-SUPOINT-ADJ TO T-SUPOINT-ADJ                             
045800       MOVE IN01-SUPOINT-UTG TO T-SUPOINT-UTG                             
045900       MOVE IN01-SUPOINT-RIT TO T-SUPOINT-RIT                             
046000       MOVE IN01-SUPOINT-PP  TO T-SUPOINT-PP                              
046100       MOVE T-RAD01          TO  REP-AREA                                 
046200       PERFORM S11-SKRIV-W37177                                           
046300***       BERÄKNA TOTALEN                                                 
046400       ADD  IN01-SUPOINT-ING TO WTOT-SUPOINT-ING                          
046500       ADD  IN01-SUPOINT-RET TO WTOT-SUPOINT-RET                          
046600       ADD  IN01-SUPOINT-FAK TO WTOT-SUPOINT-FAK                          
046700       ADD  IN01-SUPOINT-ADJ TO WTOT-SUPOINT-ADJ                          
046800       ADD  IN01-SUPOINT-UTG TO WTOT-SUPOINT-UTG                          
046900       ADD  IN01-SUPOINT-RIT TO WTOT-SUPOINT-RIT                          
047000       ADD  IN01-SUPOINT-PP  TO WTOT-SUPOINT-PP                           
047100***       LÄS NÄSTA SUMLINE DETALJ                                        
047200       PERFORM S01-LAES-W37131                                            
047300     END-PERFORM                                                          
047400***    SKRIV SUM TOTALRAD                                                 
047500     MOVE WTOT-SUPOINT-ING TO TTOT-SUPOINT-ING                            
047600     MOVE WTOT-SUPOINT-RET TO TTOT-SUPOINT-RET                            
047700     MOVE WTOT-SUPOINT-FAK TO TTOT-SUPOINT-FAK                            
047800     MOVE WTOT-SUPOINT-ADJ TO TTOT-SUPOINT-ADJ                            
047900     MOVE WTOT-SUPOINT-UTG TO TTOT-SUPOINT-UTG                            
048000     MOVE WTOT-SUPOINT-RIT TO TTOT-SUPOINT-RIT                            
048100     MOVE WTOT-SUPOINT-PP  TO TTOT-SUPOINT-PP                             
048200     MOVE T-TOT01          TO  REP-AREA                                   
048300     PERFORM S11-SKRIV-W37177                                             
048400***    NOLLSTÄLL TOTALFÄLT                                                
048500     MOVE ZERO             TO TTOT-SUPOINT-ING                            
048600     MOVE ZERO             TO TTOT-SUPOINT-RET                            
048700     MOVE ZERO             TO TTOT-SUPOINT-FAK                            
048800     MOVE ZERO             TO TTOT-SUPOINT-ADJ                            
048900     MOVE ZERO             TO TTOT-SUPOINT-UTG                            
049000     MOVE ZERO             TO TTOT-SUPOINT-RIT                            
049100     MOVE ZERO             TO TTOT-SUPOINT-PP                             
049200     MOVE ZERO             TO WTOT-SUPOINT-ING                            
049300     MOVE ZERO             TO WTOT-SUPOINT-RET                            
049400     MOVE ZERO             TO WTOT-SUPOINT-FAK                            
049500     MOVE ZERO             TO WTOT-SUPOINT-ADJ                            
049600     MOVE ZERO             TO WTOT-SUPOINT-UTG                            
049700     MOVE ZERO             TO WTOT-SUPOINT-RIT                            
049800     MOVE ZERO             TO WTOT-SUPOINT-PP                             
049900     MOVE TOMRAD TO           REP-AREA                                    
050000     PERFORM S11-SKRIV-W37177                                             
050100*** SKRIV DETALJ SEKTION                                                  
050200***    KOLL ATT DETALJER SKA SKRIVAS                                      
050300     IF W-FLEXCDET = 'Y'                                                  
050400       PERFORM UNTIL (END-OF-W37141 OR                                    
050500        (W-CURRENT-KEY >= W-SAVE-KEY))                                    
050600         PERFORM S02-LAES-W37141                                          
050700       END-PERFORM                                                        
050800       IF NOT END-OF-W37141                                               
050900         IF  (W-CURRENT-KEY = W-SAVE-KEY)                                 
051000         PERFORM  BA-DETAIL-REPORT                                        
051100         END-IF                                                           
051200       END-IF                                                             
051300     END-IF                                                               
051400*** SKRIV FOTNOT                                                          
051500     MOVE F-FOT01          TO  REP-AREA                                   
051600     PERFORM S11-SKRIV-W37177                                             
051700     .                                                                    
051800     EJECT                                                                
051900 BA-DETAIL-REPORT SECTION.                                                
052100     PERFORM BAD-RUBBISH-TRANS                                            
052200     PERFORM BAA-RETURN-TRANS                                             
052300     PERFORM BAB-FAKKRE-TRANS                                             
052400     PERFORM BAC-ADJUST-TRANS                                             
052500     .                                                                    
052600     EJECT                                                                
052700 BAD-RUBBISH-TRANS  SECTION.                                              
052800     IF IN02-IDPTYP > 'SAA'                                               
052900*** BEHANDLA DETALJRADER                                                  
053000       PERFORM UNTIL END-OF-W37141                      OR                
053100                     (W-CURRENT-KEY NOT = W-SAVE-KEY   OR                 
053200                      (IN02-IDPTYP   NOT > 'SAA'     )    )               
053300***   LÄS NÄSTA POST                                                      
053400         PERFORM S02-LAES-W37141                                          
053500       END-PERFORM                                                        
053600     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900 BAA-RETURN-TRANS  SECTION.                                               
054000     IF NOT END-OF-W37141                                                 
054100       IF             (W-CURRENT-KEY  = W-SAVE-KEY)                       
054200         IF IN02-IDPTYP = 'RET'                                           
054300*** SKRIV RUBRIKER                                                        
054400           MOVE R-RUB01          TO  REP-AREA                             
054500           PERFORM S11-SKRIV-W37177                                       
054600           MOVE R-RUB02A         TO  REP-AREA                             
054700           PERFORM S11-SKRIV-W37177                                       
054800           MOVE R-RUB02B         TO  REP-AREA                             
054900           PERFORM S11-SKRIV-W37177                                       
055000*** BEHANDLA DETALJRADER                                                  
055100           PERFORM UNTIL END-OF-W37141                 OR                 
055200                     (W-CURRENT-KEY NOT = W-SAVE-KEY   OR                 
055300                     (IN02-IDPTYP   NOT = 'RET'     )    )                
055400***   SKRIV DETALJRAD                                                     
055500             MOVE IN02-IDDISTR      TO  R1-IDDISTR                        
055600             MOVE IN02-IDKUNDNR     TO  R1-IDKUNDNR                       
055700             MOVE IN02-IDBYTRAP     TO  R1-IDBYTRAP                       
055800             MOVE IN02-IDARTNR      TO  R1-IDARTNR                        
055900             MOVE IN02-BEART-ENG    TO  R1-BEART-ENG                      
056000             MOVE IN02-KVANTAL      TO  R1-KVANTAL                        
056100             MOVE IN02-KVRETUR-GODK TO  R1-KVRETUR-GODK                   
056200             MOVE IN02-KVPOINT      TO  R1-KVPOINT                        
056300             MOVE IN02-KDBYTSTA-OBJ TO  R1-KDBYTSTA-OBJ                   
056400             MOVE IN02-KDBYTREF     TO  R1-KDBYTREF                       
056500             MOVE IN02-SUPOINT      TO  R1-SUPOINT                        
056600             MOVE R-RAD01           TO  REP-AREA                          
056700             PERFORM S11-SKRIV-W37177                                     
056800***   LÄGG TILL TOTALEN                                                   
056900             ADD IN02-SUPOINT  TO WTOT-SUPOINT                            
057000***   LÄS NÄSTA POST                                                      
057100             PERFORM S02-LAES-W37141                                      
057200           END-PERFORM                                                    
057300*** SKRIV SUMMARAD                                                        
057400           MOVE WTOT-SUPOINT   TO RTOT-SUPOINT                            
057500           MOVE R-TOT01        TO REP-AREA                                
057600           PERFORM S11-SKRIV-W37177                                       
057700           MOVE TOMRAD TO           REP-AREA                              
057800           PERFORM S11-SKRIV-W37177                                       
057900*** NOLLSTÄLL SUMMAN                                                      
058000           MOVE ZERO           TO WTOT-SUPOINT                            
058100         END-IF                                                           
058200       END-IF                                                             
058300     END-IF                                                               
058400     .                                                                    
058500     EJECT                                                                
058600 BAB-FAKKRE-TRANS  SECTION.                                               
058700     IF NOT END-OF-W37141                                                 
058800       IF             (W-CURRENT-KEY  = W-SAVE-KEY)                       
058900         IF  (IN02-IDPTYP = 'FAK' OR 'KRE'     )                          
059000***  SKRIV RUBRIK                                                         
059100          MOVE D-RUB01        TO REP-AREA                                 
059200          PERFORM S11-SKRIV-W37177                                        
059300          MOVE D-RUB02        TO REP-AREA                                 
059400          PERFORM S11-SKRIV-W37177                                        
059500***  BEARBETA RADER                                                       
059600          PERFORM UNTIL ( END-OF-W37141             OR                    
059700              (W-CURRENT-KEY NOT = W-SAVE-KEY)      OR                    
059800              (IN02-IDPTYP   NOT = 'FAK' AND 'KRE')  )                    
059900***     SKRIV DETALJRAD                                                   
060000            MOVE IN02-IDDISTR       TO  D-IDDISTR                         
060100            MOVE IN02-IDORDER       TO  D-IDORDER                         
060200            MOVE IN02-DADATUM (3:6) TO  D-TIDAT                           
060300            IF IN02-IDPTYP = 'FAK'                                        
060400            MOVE 'INV'              TO  D-IDPTYP                          
060500            END-IF                                                        
060600            IF IN02-IDPTYP = 'KRE'                                        
060700            MOVE 'C/N'              TO  D-IDPTYP                          
060800            END-IF                                                        
060900            MOVE IN02-IDARTNR       TO  D-IDARTNR                         
061000            MOVE IN02-BEART-ENG     TO  D-BEART-ENG                       
061100            MOVE IN02-KVANTAL       TO  D-KVANTAL                         
061200            MOVE IN02-KVPOINT       TO  D-KVPOINT                         
061300            MOVE IN02-SUPOINT       TO  D-SUPOINT                         
061400            MOVE D-RAD01 TO REP-AREA                                      
061500            PERFORM S11-SKRIV-W37177                                      
061600***     LÄGG TILL TOTALSUMMA                                              
061700           ADD IN02-SUPOINT TO WTOT-SUPOINT                               
061800***     LÄS IN FILEN                                                      
061900           PERFORM S02-LAES-W37141                                        
062000          END-PERFORM                                                     
062100***  SKRIV TOTALRADEN                                                     
062200          MOVE WTOT-SUPOINT TO DTOT-SUPOINT                               
062300          MOVE D-TOT01 TO     REP-AREA                                    
062400          PERFORM S11-SKRIV-W37177                                        
062500          MOVE TOMRAD TO           REP-AREA                               
062600          PERFORM S11-SKRIV-W37177                                        
062700***  NOLLSTÄLL TOTALRADEN                                                 
062800          MOVE ZERO         TO WTOT-SUPOINT                               
062900         END-IF                                                           
063000       END-IF                                                             
063100     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400 BAC-ADJUST-TRANS  SECTION.                                               
063500     IF NOT END-OF-W37141                                                 
063600       IF             (W-CURRENT-KEY  = W-SAVE-KEY)                       
063700         IF  (IN02-IDPTYP = 'ADJ'   )                                     
063800***  SKRIV RUBRIK                                                         
063900          MOVE A-RUB01        TO REP-AREA                                 
064000          PERFORM S11-SKRIV-W37177                                        
064100          MOVE A-RUB02        TO REP-AREA                                 
064200          PERFORM S11-SKRIV-W37177                                        
064300***  BEARBETA RADER                                                       
064400          PERFORM UNTIL ( END-OF-W37141             OR                    
064500              (W-CURRENT-KEY NOT = W-SAVE-KEY)      OR                    
064600              (IN02-IDPTYP   NOT = 'ADJ')  )                              
064700***     SKRIV DETALJRAD                                                   
064800            MOVE IN02-IDDISTR       TO  A-IDDISTR                         
064900            MOVE IN02-DADATUM (3:6) TO  A-TIDAT                           
065000            MOVE IN02-TENOTE        TO  A-TENOTE                          
065100            MOVE IN02-SUPOINT       TO  A-SUPOINT                         
065200            MOVE A-RAD01 TO REP-AREA                                      
065300            PERFORM S11-SKRIV-W37177                                      
065400***     LÄGG TILL TOTALSUMMA                                              
065500           ADD IN02-SUPOINT TO WTOT-SUPOINT                               
065600***     LÄS IN FILEN                                                      
065700           PERFORM S02-LAES-W37141                                        
065800          END-PERFORM                                                     
065900***  SKRIV TOTALRADEN                                                     
066000          MOVE WTOT-SUPOINT TO ATOT-SUPOINT                               
066100          MOVE A-TOT01 TO     REP-AREA                                    
066200          PERFORM S11-SKRIV-W37177                                        
066300          MOVE TOMRAD TO           REP-AREA                               
066400          PERFORM S11-SKRIV-W37177                                        
066500***  NOLLSTÄLL TOTALRADEN                                                 
066600          MOVE ZERO         TO WTOT-SUPOINT                               
066700         END-IF                                                           
066800       END-IF                                                             
066900     END-IF                                                               
067000     .                                                                    
067100     EJECT                                                                
067200 Z-FINIT SECTION.                                                         
067300     CLOSE W37131                                                         
067400           W37141                                                         
067500           W37177                                                         
067600     SKIP2                                                                
067700     MOVE 'S' TO POSTSUM-OPKOD                                            
067800     CALL POSTSUM USING POSTSUM-PARM                                      
067900     .                                                                    
068000     EJECT                                                                
068100 S01-LAES-W37131  SECTION.                                                
068200     READ W37131 INTO IN01-AREA                                           
068300     AT END                                                               
068400        MOVE HIGH-VALUE TO IN01-AREA                                      
068500        SET END-OF-W37131 TO TRUE                                         
068600                                                                          
068700     NOT AT END                                                           
068800        MOVE 'W37131' TO POSTSUM-FDNAMN                                   
068900        MOVE 'W37177D1' TO POSTSUM-DDNAMN2                                
069000*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
069100*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
069200        MOVE SPACE       TO POSTSUM-TRANSTYP                              
069300        CALL POSTSUM USING POSTSUM-PARM                                   
069400     END-READ                                                             
069500     .                                                                    
069600     EJECT                                                                
069700 S02-LAES-W37141  SECTION.                                                
069800     READ W37141 INTO IN02-AREA                                           
069900     AT END                                                               
070000        MOVE HIGH-VALUE TO IN02-AREA                                      
070100        SET END-OF-W37141 TO TRUE                                         
070200                                                                          
070300     NOT AT END                                                           
070500        MOVE IN02-IDDISTR-BET TO WL-IDDISTR-BET                           
070600        MOVE IN02-KDEXCHA     TO WL-KDEXCHA                               
070700        MOVE 'W37141' TO POSTSUM-FDNAMN                                   
070800        MOVE 'W37177D2' TO POSTSUM-DDNAMN2                                
070900*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
071000*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
071100        MOVE IN02-IDPTYP TO POSTSUM-TRANSTYP                              
071200        CALL POSTSUM USING POSTSUM-PARM                                   
071300     END-READ                                                             
071400     .                                                                    
071500     EJECT                                                                
071600 S11-SKRIV-W37177 SECTION.                                                
071700                                                                          
071800     WRITE REP-POST FROM REP-AREA                                         
072000     MOVE 'REP'      TO POSTSUM-TRANSTYP                                  
072100     MOVE 'W37177' TO POSTSUM-FDNAMN                                      
072200     MOVE 'W37177D3' TO POSTSUM-DDNAMN2                                   
072300     CALL POSTSUM USING POSTSUM-PARM                                      
072400     MOVE SPACE       TO REP-AREA                                         
072500     .                                                                    
072600     EJECT                                                                
072700*S99-ABEND SECTION.                                                       
072800*                                                                         
072900*    SKIP2                                                                
073000*    MOVE 'S' TO POSTSUM-OPKOD                                            
073100*    CALL POSTSUM USING POSTSUM-PARM                                      
073200*    CALL ABEND USING RKOD-ABEND                                          
073300*    .                                                                    
073400 IMS-GU-WDGX3156 SECTION.                                                 
073500     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-3155-X ')'                    
073600      DELIMITED BY SIZE INTO SSA1                                         
073700     STRING 'WDGX3156(KDSEGKEY =' W-WDGXKEY-3156-X ')'                    
073800      DELIMITED BY SIZE INTO SSA2                                         
073900     MOVE '  '             TO GODK-STATUSKODER                            
074000     CALL CBLTDLI USING GU  3156-PCB DLI-IO-WDGX3156 SSA1 SSA2            
074100     MOVE 3156-STATUS-CODE TO STATUS-WS                                   
074200     PERFORM IMS-STATUSKONTROLL                                           
074300     .                                                                    
074400     EJECT                                                                
074500 IMS-STATUSKONTROLL SECTION.                                              
074600     SET STATUS-IX TO 1                                                   
074700     SEARCH GODK-STATUS                                                   
074800     AT END                                                               
074900      STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                   
075000       DELIMITED BY SIZE INTO FELTEXT                                     
075100      DISPLAY FELTEXT                                                     
075200      CALL FELLOG                                                         
075300      WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                            
075400      CONTINUE                                                            
075500     END-SEARCH                                                           
075600     .                                                                    

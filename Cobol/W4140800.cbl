000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4140800.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   10/08/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        TO GENERATE REPORT FOR PARTS DELIVERED FROM SDC FOR              
001000*        DIRECT DELIVERIES AND TO SEND THEM FOR D&P                       
001100*                                                                         
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- FILE FROM W41401                                           
002600     SELECT W41412                     ASSIGN TO W41408D1.                
002700     SKIP2                                                                
002800*          --- SUMMARY REPORT FOR SERVICE PARTS FROM SDC                  
002900     SELECT W41413                     ASSIGN TO W41408D2.                
003000     SKIP2                                                                
003100*          --- DETAILED REPORT FOR SERVICE PARTS FROM DC                  
003200     SELECT W41414                     ASSIGN TO W41408D3.                
003300*          --- COPY INPUT RECORDS TO CREATE WEEKLY REPORT LATER           
003400     SELECT W41415                     ASSIGN TO W41408D4.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W41412                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W4141201      -L.                                              
004500     SKIP3                                                                
004600 FD  W41413                                                               
004700     RECORDING       V                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000 01  WS-SUM-RECORD              PIC X(133).                               
005100     SKIP3                                                                
005200 FD  W41414                                                               
005300     RECORDING       V                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600 01  WS-DET-RECORD              PIC X(133).                               
005700     EJECT                                                                
005800 FD  W41415                                                               
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100                                                                          
006200*01  RECORD -COPY W4141201 -PRE  UT-  -L.                                 
006300     SKIP3                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500                                                                          
006600 77  IDPGM                       PIC X(8)    VALUE 'W4140800'.            
006700 77  YES                         PIC X       VALUE 'J'.                   
006800 77  NOO                         PIC X       VALUE 'N'.                   
006900                                                                          
007000 77  W41412-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W41412                       VALUE 'J'.                   
007200     EJECT                                                                
007300 01  TODAYS-DATE.                                                         
007400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007600     03  TODAYS-DATE-DAY         PIC 9(2).                                
007700     EJECT                                                                
007800 01  GENERAL-SUBPROGRAMS.                                                 
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008200     SKIP2                                                                
008300*    --- PARAMETERS TO ABEND                                              
008400                                                                          
008500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008800*                                                                         
008900 01  ERROR-TEXT.                                                          
009000     03  FILLER                  PIC X(8)    VALUE 'ERR-TXT'.             
009100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009200     EJECT                                                                
009300*    --- PARAMETERS TO DATKORT                                            
009400*                                                                         
009500 01  PROGRAM-NAME                PIC X(6)    VALUE 'W41408'.              
009600 01  DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.              
009700*                                                                         
009800*01  -COPY WDATKORT                                                       
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL POSTSUM                                          
010100*                                                                         
010200*01  -COPY W0005   -PRE  POSTSUM-                                         
010300     EJECT                                                                
010400 01  W41412-AREA-START           PIC X(24)   VALUE                        
010500                                 'W41412-AREA-START  '.                   
010600*01  AREA -COPY W4141201     -PRE IN-                                     
010700                                                                          
010800 01  W41415-AREA-START           PIC X(24)   VALUE                        
010900                                 'W41415-AREA-START  '.                   
011000*01  AREA -COPY W4141201     -PRE UT-                                     
011100                                                                          
011200*                                                                         
011300 01  WS-TABLE.                                                            
011400     03 WS-TABLE-AREA OCCURS 10.                                          
011500       05 WS-TAB-IDLEVNR         PIC X(5).                                
011600       05 WS-TAB-DC-CNT          PIC 9(5) COMP-3.                         
011700       05 WS-TAB-LEVNR-CNT       PIC 9(5) COMP-3.                         
011800*                                                                         
011900 01  WS-MISC.                                                             
012000     03 WS-SAVE-IDDC             PIC X(2)      VALUE SPACES.              
012100     03 WS-SAVE-IDLEVNR          PIC X(5)      VALUE SPACES.              
012200     03 IX                       PIC 9(2)      VALUE ZERO.                
012300     03 WS-TOT-LINES             PIC 9(9)      VALUE ZERO.                
012400     03 WS-TOT-CNT-DC            PIC 9(9)      VALUE ZERO.                
012500     03 WS-SERV-DC               PIC 9(3)V9(3) VALUE ZERO.                
012600     03 WS-LEVNR-CNT             PIC 9(9)      VALUE ZERO.                
012700     03 WS-SERV-DC-DET           PIC 9(3)V9(3) VALUE ZERO.                
012800     03 WS-DET-IDKUNDRF.                                                  
012900        05 WS-DET-IDORDNR7       PIC 9(7)      VALUE ZERO.                
013000        05 FILLER                PIC X(3)      VALUE SPACES.              
013100*                                                                         
013200 01  WS-DAP-SUM1.                                                         
013300     03  FILLER                  PIC X(133)  VALUE                        
013400                                 ' ¤DAPW41408-001'.                       
013500     EJECT                                                                
013600 01  WS-DAP-SUM2.                                                         
013700     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
013800     03  WS-DAP-SUM-IDDC         PIC X(2)    VALUE SPACE.                 
013900     03  FILLER                  PIC X(126)  VALUE SPACE.                 
014000     EJECT                                                                
014100*                                                                         
014200 01  WS-SUM-HDR-REC.                                                      
014300     03 FILLER                   PIC X(40) VALUE                          
014400     'W41408-001           Service Parts   DC '.                          
014500     03 WS-SUM-IDDC              PIC X(02).                               
014600     03 FILLER                   PIC X(28) VALUE ' Day'.                  
014700     03 WS-SUM-DATE              PIC X(06) VALUE SPACES.                  
014800*                                                                         
014900 01  WS-SUMMARY-REC.                                                      
015000     03 FILLER                   PIC X(19) VALUE                          
015100                                 'Incoming lines for'.                    
015200     03 WS-SUM-IDLEVNR           PIC X(05) VALUE SPACES.                  
015300     03 FILLER                   PIC X(02) VALUE ': '.                    
015400     03 WS-SUM-CNT               PIC ZZZZ9 VALUE ZERO.                    
015500*                                                                         
015600 01  WS-TOTAL-REC.                                                        
015700     03 FILLER                   PIC X(22) VALUE                          
015800                                 'Total incoming lines: '.                
015900     03 WS-TOT-LINES-ED          PIC ZZZZ9 VALUE ZERO.                    
016000*                                                                         
016100 01  WS-TOTAL-DC-REC.                                                     
016200     03 FILLER                   PIC X(30) VALUE                          
016300                        'Total lines delivered from DC '.                 
016400     03 WS-TOT-IDDC              PIC X(02).                               
016500     03 FILLER                   PIC X(02) VALUE ': '.                    
016600     03 WS-TOT-CNT-DC-ED         PIC ZZZZ9 VALUE ZERO.                    
016700*                                                                         
016800 01  WS-SERV-DC-REC.                                                      
016900     03 FILLER                   PIC X(11) VALUE                          
017000                                 'Service DC '.                           
017100     03 WS-SERV-IDDC             PIC X(02).                               
017200     03 FILLER                   PIC X(02) VALUE ': '.                    
017300     03 WS-SERV-DC-ED            PIC ZZ9.9 VALUE ZERO.                    
017400     03 FILLER                   PIC X(01) VALUE '%'.                     
017500*                                                                         
017600 01  WS-LEVNR-REC.                                                        
017700     03 FILLER                   PIC X(24) VALUE                          
017800                        'Lines delivered from DC '.                       
017900     03 WS-LEVNR-IDDC            PIC X(02).                               
018000     03 FILLER                   PIC X(05) VALUE ' for '.                 
018100     03 WS-LEVNR-IDLEVNR         PIC X(05) VALUE SPACES.                  
018200     03 FILLER                   PIC X(02) VALUE ': '.                    
018300     03 WS-LEVNR-CNT-ED          PIC ZZZZ9 VALUE ZERO.                    
018400*                                                                         
018500 01  WS-SERV-DC-DET-REC.                                                  
018600     03 FILLER                   PIC X(11) VALUE                          
018700                                 'Service DC '.                           
018800     03 WS-SERV-IDDC-DET         PIC X(02).                               
018900     03 FILLER                   PIC X(05) VALUE ' for '.                 
019000     03 WS-SERV-IDLEVNR          PIC X(05) VALUE SPACES.                  
019100     03 FILLER                   PIC X(02) VALUE ': '.                    
019200     03 WS-SERV-DC-DET-ED        PIC ZZ9.9 VALUE ZERO.                    
019300     03 FILLER                   PIC X(01) VALUE '%'.                     
019400                                                                          
019500 01  WS-BLANK-LINE               PIC X(80) VALUE SPACES.                  
019600                                                                          
019700 01  WS-DAP-DET1.                                                         
019800     03  FILLER                  PIC X(133)  VALUE                        
019900                                 ' ¤DAPW41408-002'.                       
020000                                                                          
020100 01  WS-DAP-DET2.                                                         
020200     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
020300     03  WS-DAP-DET-IDDC         PIC X(2)    VALUE SPACE.                 
020400     03  FILLER                  PIC X(126)  VALUE SPACE.                 
020500                                                                          
020600 01  WS-DET-HDR-REC1.                                                     
020700     03 FILLER                   PIC X(42) VALUE                          
020800     'W41408-002             Service Parts   DC '.                        
020900     03 WS-DET-IDDC              PIC X(02).                               
021000     03 FILLER                   PIC X(06) VALUE ' Day.'.                 
021100     03 FILLER                   PIC X(40) VALUE                          
021200     'Lines from Supplier'.                                               
021300      03 WS-DET-DATE             PIC X(06) VALUE SPACES.                  
021400*                                                                         
021500 01  WS-DET-HDR-REC2.                                                     
021600     03 FILLER                   PIC X(08) VALUE 'Supplier'.              
021700     03 FILLER                   PIC X(02) VALUE SPACES.                  
021800     03 FILLER                   PIC X(06) VALUE 'Regdat'.                
021900     03 FILLER                   PIC X(03) VALUE SPACES.                  
022000     03 FILLER                   PIC X(06) VALUE 'PartNo'.                
022100     03 FILLER                   PIC X(05) VALUE SPACES.                  
022200     03 FILLER                   PIC X(08) VALUE 'Quantity'.              
022300     03 FILLER                   PIC X(02) VALUE SPACES.                  
022400     03 FILLER                   PIC X(05) VALUE 'Distr'.                 
022500     03 FILLER                   PIC X(02) VALUE SPACES.                  
022600     03 FILLER                   PIC X(07) VALUE 'Dealer'.                
022700     03 FILLER                   PIC X(02) VALUE SPACES.                  
022800     03 FILLER                   PIC X(07) VALUE 'OrderNo'.               
022900*                                                                         
023000 01  WS-DET-DET-REC.                                                      
023100     03 WS-DET-IDLEVNR           PIC X(05) VALUE SPACES.                  
023200     03 FILLER                   PIC X(05) VALUE SPACES.                  
023300     03 WS-DET-REGDAT            PIC Z9(6) VALUE ZEROS.                   
023400     03 FILLER                   PIC X(02) VALUE SPACES.                  
023500     03 WS-DET-IDARTNR           PIC Z(8)9 VALUE ZEROS.                   
023600     03 FILLER                   PIC X(02) VALUE SPACES.                  
023700     03 WS-DET-KVBEART           PIC Z(6)9 VALUE ZEROS.                   
023800     03 FILLER                   PIC X(03) VALUE SPACES.                  
023900     03 WS-DET-IDDISTR           PIC Z(4)9 VALUE ZEROS.                   
024000     03 FILLER                   PIC X(02) VALUE SPACES.                  
024100     03 WS-DET-IDKUNDNR          PIC Z(6)9 VALUE ZEROS.                   
024200     03 FILLER                   PIC X(02) VALUE SPACES.                  
024300     03 WS-DET-IDKUNDRF-ED       PIC Z(6)9 VALUE ZEROS.                   
024400*                                                                         
024500 PROCEDURE DIVISION.                                                      
024600 MAIN SECTION.                                                            
024700                                                                          
024800     PERFORM A-INIT                                                       
024900     PERFORM S01-READ-W41412                                              
025000                                                                          
025010     PERFORM UNTIL IN-BUMU-IDDC-STEER NOT = SPACE                         
025020       PERFORM S01-READ-W41412                                            
025030     END-PERFORM                                                          
025040                                                                          
025100     IF NOT END-OF-W41412                                                 
025200       PERFORM D-INIT-FIELDS                                              
025300     END-IF                                                               
025400                                                                          
025500     PERFORM UNTIL END-OF-W41412                                          
025600       IF IN-BUMU-IDDC-STEER = WS-SAVE-IDDC                               
025700         IF IN-BUMU-IDLEVNR = WS-SAVE-IDLEVNR                             
025800           PERFORM B-PROCESS-INPUT                                        
025900         ELSE                                                             
026000           COMPUTE IX = IX + 1                                            
026100           MOVE IN-BUMU-IDLEVNR  TO WS-SAVE-IDLEVNR                       
026200           PERFORM B-PROCESS-INPUT                                        
026300         END-IF                                                           
026400       ELSE                                                               
026500         PERFORM C-CREATE-W41413                                          
026600         PERFORM D-INIT-FIELDS                                            
026700         PERFORM B-PROCESS-INPUT                                          
026800       END-IF                                                             
026900       PERFORM S01-READ-W41412                                            
027000     END-PERFORM                                                          
027100                                                                          
027200     IF WS-TOT-LINES > ZERO                                               
027300       PERFORM C-CREATE-W41413                                            
027400     END-IF                                                               
027500                                                                          
027600     PERFORM Z-FINIT                                                      
027700                                                                          
027800     MOVE ZERO TO RETURN-CODE                                             
027900     GOBACK                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 A-INIT SECTION.                                                          
028300     OPEN INPUT  W41412                                                   
028400                                                                          
028500     OPEN OUTPUT W41413                                                   
028600                 W41414                                                   
028700                 W41415                                                   
028800*                                                                         
028900     CALL DATKORT USING PROGRAM-NAME DATECARD-ID DATUMKORT                
029000     MOVE D-AAR     TO  TODAYS-DATE-YEAR                                  
029100     MOVE D-MAANAD  TO  TODAYS-DATE-MONTH                                 
029200     MOVE D-DAG     TO  TODAYS-DATE-DAY                                   
029300     MOVE IDPGM     TO POSTSUM-PROGNAMN                                   
029400*                                                                         
029500     .                                                                    
029600     EJECT                                                                
029700                                                                          
029800 B-PROCESS-INPUT SECTION.                                                 
029900                                                                          
030000     IF IX <= 10                                                          
030100       MOVE IN-BUMU-IDLEVNR      TO WS-TAB-IDLEVNR(IX)                    
030200       IF IN-BUMU-IDDC-STEER = IN-BUMU-IDDC                               
030300         COMPUTE WS-TAB-DC-CNT (IX)  = WS-TAB-DC-CNT (IX) + 1             
030400       ELSE                                                               
030500         PERFORM BA-CREATE-W41414                                         
030600       END-IF                                                             
030700       PERFORM BB-CREATE-W41415                                           
030800       COMPUTE WS-TAB-LEVNR-CNT (IX) = WS-TAB-LEVNR-CNT (IX) + 1          
030900       COMPUTE WS-TOT-LINES          = WS-TOT-LINES + 1                   
031000     END-IF                                                               
031100                                                                          
031200     .                                                                    
031300     EJECT                                                                
031400                                                                          
031500 BA-CREATE-W41414 SECTION.                                                
031600     MOVE IN-BUMU-IDLEVNR        TO WS-DET-IDLEVNR                        
031700     MOVE IN-BUMU-TIREGDAT       TO WS-DET-REGDAT                         
031800     MOVE IN-BUMU-IDARTNR        TO WS-DET-IDARTNR                        
031900     MOVE IN-BUMU-KVBEART        TO WS-DET-KVBEART                        
032000     MOVE IN-BUMU-IDDISTR        TO WS-DET-IDDISTR                        
032100     MOVE IN-BUMU-IDKUNDNR       TO WS-DET-IDKUNDNR                       
032200     MOVE IN-BUMU-IDKUNDRF       TO WS-DET-IDKUNDRF                       
032300     MOVE WS-DET-IDORDNR7        TO WS-DET-IDKUNDRF-ED                    
032400     MOVE WS-DET-DET-REC         TO WS-DET-RECORD                         
032500     PERFORM S12-WRITE-W41414                                             
032600     .                                                                    
032700 BB-CREATE-W41415  SECTION.                                               
032800     MOVE IN-BUMU-IDLEVNR        TO UT-BUMU-IDLEVNR                       
032900     MOVE IN-BUMU-TIREGDAT       TO UT-BUMU-TIREGDAT                      
033000     MOVE IN-BUMU-IDARTNR        TO UT-BUMU-IDARTNR                       
033100     MOVE IN-BUMU-KVBEART        TO UT-BUMU-KVBEART                       
033200     MOVE IN-BUMU-IDDISTR        TO UT-BUMU-IDDISTR                       
033300     MOVE IN-BUMU-IDKUNDNR       TO UT-BUMU-IDKUNDNR                      
033400     MOVE IN-BUMU-IDKUNDRF       TO UT-BUMU-IDKUNDRF                      
033500     MOVE IN-BUMU-IDDC           TO UT-BUMU-IDDC                          
033600     MOVE IN-BUMU-IDDC-STEER     TO UT-BUMU-IDDC-STEER                    
033700     PERFORM S13-WRITE-W41415                                             
033800     .                                                                    
033900 C-CREATE-W41413 SECTION.                                                 
034000                                                                          
034100     MOVE WS-SUM-HDR-REC         TO WS-SUM-RECORD                         
034200     PERFORM S11-WRITE-W41413                                             
034300                                                                          
034400     MOVE WS-BLANK-LINE          TO WS-SUM-RECORD                         
034500     PERFORM S11-WRITE-W41413                                             
034600                                                                          
034700     MOVE +1 TO IX                                                        
034800     PERFORM UNTIL IX > 10 OR WS-TAB-IDLEVNR(IX) = SPACE                  
034900       MOVE WS-TAB-LEVNR-CNT(IX) TO  WS-SUM-CNT                           
035000       MOVE WS-TAB-IDLEVNR(IX)   TO  WS-SUM-IDLEVNR                       
035100       COMPUTE WS-TOT-CNT-DC = WS-TOT-CNT-DC +                            
035200                                 WS-TAB-DC-CNT(IX)                        
035300       MOVE WS-SUMMARY-REC       TO WS-SUM-RECORD                         
035400       PERFORM S11-WRITE-W41413                                           
035500       COMPUTE IX = IX + 1                                                
035600     END-PERFORM                                                          
035700                                                                          
035800     MOVE WS-TOT-LINES           TO WS-TOT-LINES-ED                       
035900*                                                                         
036000     MOVE WS-BLANK-LINE          TO WS-SUM-RECORD                         
036100     PERFORM S11-WRITE-W41413                                             
036200*                                                                         
036300     MOVE WS-TOTAL-REC           TO WS-SUM-RECORD                         
036400     PERFORM S11-WRITE-W41413                                             
036500*                                                                         
036600     MOVE WS-BLANK-LINE          TO WS-SUM-RECORD                         
036700     PERFORM S11-WRITE-W41413                                             
036800*                                                                         
036900     MOVE WS-TOT-CNT-DC          TO WS-TOT-CNT-DC-ED                      
037000     IF WS-TOT-LINES > 0                                                  
037100       COMPUTE WS-SERV-DC =                                               
037200               (WS-TOT-CNT-DC / WS-TOT-LINES) * 100                       
037300     END-IF                                                               
037400     MOVE WS-SERV-DC             TO WS-SERV-DC-ED                         
037500*                                                                         
037600     MOVE WS-TOTAL-DC-REC        TO WS-SUM-RECORD                         
037700     PERFORM S11-WRITE-W41413                                             
037800*                                                                         
037900     MOVE WS-SERV-DC-REC         TO WS-SUM-RECORD                         
038000     PERFORM S11-WRITE-W41413                                             
038100*                                                                         
038200     MOVE WS-BLANK-LINE          TO WS-SUM-RECORD                         
038300     PERFORM S11-WRITE-W41413                                             
038400*                                                                         
038500     MOVE +1 TO IX                                                        
038600     PERFORM UNTIL IX > 10 OR WS-TAB-IDLEVNR(IX) = SPACE                  
038700       MOVE  WS-TAB-DC-CNT(IX)   TO WS-LEVNR-CNT                          
038800       MOVE  WS-TAB-IDLEVNR(IX)  TO WS-LEVNR-IDLEVNR                      
038900                                    WS-SERV-IDLEVNR                       
039000       MOVE WS-LEVNR-CNT         TO WS-LEVNR-CNT-ED                       
039100       IF WS-TAB-LEVNR-CNT(IX) > 0                                        
039200         COMPUTE WS-SERV-DC-DET =                                         
039300               (WS-TAB-DC-CNT(IX) / WS-TAB-LEVNR-CNT(IX)) * 100           
039400       END-IF                                                             
039500       MOVE WS-SERV-DC-DET       TO WS-SERV-DC-DET-ED                     
039600*                                                                         
039700       MOVE WS-LEVNR-REC         TO WS-SUM-RECORD                         
039800       PERFORM S11-WRITE-W41413                                           
039900*                                                                         
040000       MOVE WS-SERV-DC-DET-REC   TO WS-SUM-RECORD                         
040100       PERFORM S11-WRITE-W41413                                           
040200*                                                                         
040300       MOVE WS-BLANK-LINE        TO WS-SUM-RECORD                         
040400       PERFORM S11-WRITE-W41413                                           
040500*                                                                         
040600       COMPUTE IX = IX + 1                                                
040700     END-PERFORM                                                          
040800     .                                                                    
040900     EJECT                                                                
041000                                                                          
041100 D-INIT-FIELDS SECTION.                                                   
041200                                                                          
041300     MOVE TODAYS-DATE            TO WS-SUM-DATE                           
041400                                    WS-DET-DATE                           
041500     MOVE +1                     TO IX                                    
041600     MOVE IN-BUMU-IDDC-STEER     TO WS-SAVE-IDDC                          
041700     MOVE IN-BUMU-IDLEVNR        TO WS-SAVE-IDLEVNR                       
041800     MOVE ZEROES                 TO WS-TOT-LINES                          
041900                                    WS-TOT-CNT-DC                         
042000                                    WS-SERV-DC                            
042100                                    WS-LEVNR-CNT                          
042200                                    WS-SERV-DC-DET                        
042300                                                                          
042400     MOVE IN-BUMU-IDDC-STEER     TO WS-DAP-SUM-IDDC                       
042500                                    WS-SUM-IDDC                           
042600                                    WS-TOT-IDDC                           
042700                                    WS-SERV-IDDC                          
042800                                    WS-LEVNR-IDDC                         
042900                                    WS-SERV-IDDC-DET                      
043000                                    WS-DAP-DET-IDDC                       
043100                                    WS-DET-IDDC                           
043200                                                                          
043300     INITIALIZE WS-TABLE                                                  
043400                                                                          
043500     MOVE WS-DAP-SUM1            TO WS-SUM-RECORD                         
043600     PERFORM S11-WRITE-W41413                                             
043700                                                                          
043800     MOVE WS-DAP-SUM2            TO WS-SUM-RECORD                         
043900     PERFORM S11-WRITE-W41413                                             
044000                                                                          
044100     MOVE WS-DAP-DET1            TO WS-DET-RECORD                         
044200     PERFORM S12-WRITE-W41414                                             
044300                                                                          
044400     MOVE WS-DAP-DET2            TO WS-DET-RECORD                         
044500     PERFORM S12-WRITE-W41414                                             
044600                                                                          
044700     MOVE WS-DET-HDR-REC1        TO WS-DET-RECORD                         
044800     PERFORM S12-WRITE-W41414                                             
044900                                                                          
045000     MOVE WS-BLANK-LINE          TO WS-DET-RECORD                         
045100     PERFORM S12-WRITE-W41414                                             
045200                                                                          
045300     MOVE WS-DET-HDR-REC2        TO WS-DET-RECORD                         
045400     PERFORM S12-WRITE-W41414                                             
045500                                                                          
045600     MOVE WS-BLANK-LINE          TO WS-DET-RECORD                         
045700     PERFORM S12-WRITE-W41414                                             
045800     .                                                                    
045900     EJECT                                                                
046000                                                                          
046100 Z-FINIT SECTION.                                                         
046200     CLOSE W41412                                                         
046300           W41413                                                         
046400           W41414                                                         
046500           W41415                                                         
046600                                                                          
046700     MOVE 'S' TO POSTSUM-OPKOD                                            
046800     CALL POSTSUM USING POSTSUM-PARM                                      
046900     .                                                                    
047000     EJECT                                                                
047100 S01-READ-W41412  SECTION.                                                
047200     READ W41412 INTO IN-AREA                                             
047300     AT END                                                               
047400        SET END-OF-W41412  TO TRUE                                        
047500                                                                          
047600     NOT AT END                                                           
047700        MOVE 'W41412'      TO POSTSUM-FDNAMN                              
047800        MOVE 'W41408D1'    TO POSTSUM-DDNAMN2                             
047900        MOVE SPACE         TO POSTSUM-TRANSTYP                            
048000        CALL POSTSUM USING POSTSUM-PARM                                   
048100     END-READ                                                             
048200     .                                                                    
048300 S11-WRITE-W41413 SECTION.                                                
048400     WRITE WS-SUM-RECORD                                                  
048500                                                                          
048600     MOVE SPACE         TO POSTSUM-TRANSTYP                               
048700     MOVE 'W41413'      TO POSTSUM-FDNAMN                                 
048800     MOVE 'W41408D2'    TO POSTSUM-DDNAMN2                                
048900     CALL POSTSUM USING POSTSUM-PARM                                      
049000     .                                                                    
049100 S12-WRITE-W41414 SECTION.                                                
049200     WRITE WS-DET-RECORD                                                  
049300                                                                          
049400     MOVE SPACE         TO POSTSUM-TRANSTYP                               
049500     MOVE 'W41414'      TO POSTSUM-FDNAMN                                 
049600     MOVE 'W41408D3'    TO POSTSUM-DDNAMN2                                
049700     CALL POSTSUM USING POSTSUM-PARM                                      
049800     .                                                                    
049900 S13-WRITE-W41415 SECTION.                                                
050000     WRITE UT-RECORD FROM UT-AREA                                         
050100                                                                          
050200     MOVE SPACE         TO POSTSUM-TRANSTYP                               
050300     MOVE 'W41415'      TO POSTSUM-FDNAMN                                 
050400     MOVE 'W41408D4'    TO POSTSUM-DDNAMN2                                
050500     CALL POSTSUM USING POSTSUM-PARM                                      
050600     .                                                                    
050700 S99-ABEND SECTION.                                                       
050800     MOVE 'S' TO POSTSUM-OPKOD                                            
050900     CALL POSTSUM USING POSTSUM-PARM                                      
051000     CALL ABEND USING RKOD-ABEND                                          
051100     .                                                                    

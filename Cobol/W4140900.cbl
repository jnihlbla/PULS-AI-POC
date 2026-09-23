000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4140900.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   10/09/08.                                                
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
002600     SELECT W41412                     ASSIGN TO W41409D1.                
002700     SKIP2                                                                
002800*          --- SUMMARY REPORT FOR SERVICE PARTS FROM SDC                  
002900     SELECT W41413                     ASSIGN TO W41409D2.                
003000     SKIP2                                                                
003100*          --- DETAILED REPORT FOR SERVICE PARTS FROM DC                  
003200     SELECT W41414                     ASSIGN TO W41409D3.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W41412                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W4141201      -L.                                              
004300     SKIP3                                                                
004400 FD  W41413                                                               
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800 01  WS-SUM-RECORD              PIC X(133).                               
004900     SKIP3                                                                
005000 FD  W41414                                                               
005100     RECORDING       V                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400 01  WS-DET-RECORD              PIC X(133).                               
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800 77  IDPGM                       PIC X(8)    VALUE 'W4140900'.            
005900 77  YES                         PIC X       VALUE 'J'.                   
006000 77  NOO                         PIC X       VALUE 'N'.                   
006100                                                                          
006200 77  W41412-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-W41412                       VALUE 'J'.                   
006400     EJECT                                                                
006500 01  TODAYS-DATE.                                                         
006600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006800     03  TODAYS-DATE-DAY         PIC 9(2).                                
006900     EJECT                                                                
007000 01  GENERAL-SUBPROGRAMS.                                                 
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007400     SKIP2                                                                
007500*    --- PARAMETERS TO ABEND                                              
007600                                                                          
007700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008000*                                                                         
008100 01  ERROR-TEXT.                                                          
008200     03  FILLER                  PIC X(8)    VALUE 'ERR-TXT'.             
008300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008400     EJECT                                                                
008500*    --- PARAMETERS TO DATKORT                                            
008600*                                                                         
008700 01  PROGRAM-NAME                PIC X(6)    VALUE 'W41409'.              
008800 01  DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.              
008900*                                                                         
009000*01  -COPY WDATKORT                                                       
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL POSTSUM                                          
009300*                                                                         
009400*01  -COPY W0005   -PRE  POSTSUM-                                         
009500     EJECT                                                                
009600 01  W41412-AREA-START           PIC X(24)   VALUE                        
009700                                 'W41412-AREA-START  '.                   
009800*01  AREA -COPY W4141201     -PRE IN-                                     
009900                                                                          
010000*                                                                         
010100 01  WS-TABLE.                                                            
010200     03 WS-TABLE-AREA OCCURS 10.                                          
010300       05 WS-TAB-IDLEVNR         PIC X(5).                                
010400       05 WS-TAB-DC-CNT          PIC 9(5) COMP-3.                         
010500       05 WS-TAB-LEVNR-CNT       PIC 9(5) COMP-3.                         
010600*                                                                         
010700 01  WS-MISC.                                                             
010800     03 WS-SAVE-IDDC             PIC X(2)      VALUE SPACES.              
010900     03 WS-SAVE-IDLEVNR          PIC X(5)      VALUE SPACES.              
011000     03 IX                       PIC 9(2)      VALUE ZERO.                
011100     03 WS-TOT-LINES             PIC 9(9)      VALUE ZERO.                
011200     03 WS-TOT-CNT-DC            PIC 9(9)      VALUE ZERO.                
011300     03 WS-SERV-DC               PIC 9(3)V9(3) VALUE ZERO.                
011400     03 WS-LEVNR-CNT             PIC 9(9)      VALUE ZERO.                
011500     03 WS-SERV-DC-DET           PIC 9(3)V9(3) VALUE ZERO.                
011600     03 WS-DET-IDKUNDRF.                                                  
011700        05 WS-DET-IDORDNR7       PIC 9(7)      VALUE ZERO.                
011800        05 FILLER                PIC X(3)      VALUE SPACES.              
011900*                                                                         
012000 01  WS-DAP-SUM1.                                                         
012100     03  FILLER                  PIC X(133)  VALUE                        
012200                                 ' ¤DAPW41409-001'.                       
012300     EJECT                                                                
012400 01  WS-DAP-SUM2.                                                         
012500     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
012600     03  WS-DAP-SUM-IDDC         PIC X(2)    VALUE SPACE.                 
012700     03  FILLER                  PIC X(126)  VALUE SPACE.                 
012800     EJECT                                                                
012900*                                                                         
013000 01  WS-SUM-HDR-REC.                                                      
013100     03 FILLER                   PIC X(40) VALUE                          
013200     'W41409-001           Service Parts   DC '.                          
013300     03 WS-SUM-IDDC              PIC X(02).                               
013400     03 FILLER                   PIC X(28) VALUE ' Week'.                 
013500     03 WS-SUM-DATE              PIC X(06) VALUE SPACES.                  
013600*                                                                         
013700 01  WS-SUMMARY-REC.                                                      
013800     03 FILLER                   PIC X(19) VALUE                          
013900                                 'Incoming lines for'.                    
014000     03 WS-SUM-IDLEVNR           PIC X(05) VALUE SPACES.                  
014100     03 FILLER                   PIC X(02) VALUE ': '.                    
014200     03 WS-SUM-CNT               PIC ZZZZ9 VALUE ZERO.                    
014300*                                                                         
014400 01  WS-TOTAL-REC.                                                        
014500     03 FILLER                   PIC X(22) VALUE                          
014600                                 'Total incoming lines: '.                
014700     03 WS-TOT-LINES-ED          PIC ZZZZ9 VALUE ZERO.                    
014800*                                                                         
014900 01  WS-TOTAL-DC-REC.                                                     
015000     03 FILLER                   PIC X(30) VALUE                          
015100                        'Total lines delivered from DC '.                 
015200     03 WS-TOT-IDDC              PIC X(02).                               
015300     03 FILLER                   PIC X(02) VALUE ': '.                    
015400     03 WS-TOT-CNT-DC-ED         PIC ZZZZ9 VALUE ZERO.                    
015500*                                                                         
015600 01  WS-SERV-DC-REC.                                                      
015700     03 FILLER                   PIC X(11) VALUE                          
015800                                 'Service DC '.                           
015900     03 WS-SERV-IDDC             PIC X(02).                               
016000     03 FILLER                   PIC X(02) VALUE ': '.                    
016100     03 WS-SERV-DC-ED            PIC ZZ9.9 VALUE ZERO.                    
016200     03 FILLER                   PIC X(01) VALUE '%'.                     
016300*                                                                         
016400 01  WS-LEVNR-REC.                                                        
016500     03 FILLER                   PIC X(24) VALUE                          
016600                        'Lines delivered from DC '.                       
016700     03 WS-LEVNR-IDDC            PIC X(02).                               
016800     03 FILLER                   PIC X(05) VALUE ' for '.                 
016900     03 WS-LEVNR-IDLEVNR         PIC X(05) VALUE SPACES.                  
017000     03 FILLER                   PIC X(02) VALUE ': '.                    
017100     03 WS-LEVNR-CNT-ED          PIC ZZZZ9 VALUE ZERO.                    
017200*                                                                         
017300 01  WS-SERV-DC-DET-REC.                                                  
017400     03 FILLER                   PIC X(11) VALUE                          
017500                                 'Service DC '.                           
017600     03 WS-SERV-IDDC-DET         PIC X(02).                               
017700     03 FILLER                   PIC X(05) VALUE ' for '.                 
017800     03 WS-SERV-IDLEVNR          PIC X(05) VALUE SPACES.                  
017900     03 FILLER                   PIC X(02) VALUE ': '.                    
018000     03 WS-SERV-DC-DET-ED        PIC ZZ9.9 VALUE ZERO.                    
018100     03 FILLER                   PIC X(01) VALUE '%'.                     
018200                                                                          
018300 01  WS-BLANK-LINE               PIC X(80) VALUE SPACES.                  
018400                                                                          
018500 01  WS-DAP-DET1.                                                         
018600     03  FILLER                  PIC X(133)  VALUE                        
018700                                 ' ¤DAPW41409-002'.                       
018800                                                                          
018900 01  WS-DAP-DET2.                                                         
019000     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
019100     03  WS-DAP-DET-IDDC         PIC X(2)    VALUE SPACE.                 
019200     03  FILLER                  PIC X(126)  VALUE SPACE.                 
019300                                                                          
019400 01  WS-DET-HDR-REC1.                                                     
019500     03 FILLER                   PIC X(42) VALUE                          
019600     'W41409-002             Service Parts   DC '.                        
019700     03 WS-DET-IDDC              PIC X(02).                               
019800     03 FILLER                   PIC X(06) VALUE ' Week.'.                
019900     03 FILLER                   PIC X(40) VALUE                          
020000     'Lines from Supplier'.                                               
020100      03 WS-DET-DATE             PIC X(06) VALUE SPACES.                  
020200*                                                                         
020300 01  WS-DET-HDR-REC2.                                                     
020400     03 FILLER                   PIC X(08) VALUE 'Supplier'.              
020500     03 FILLER                   PIC X(02) VALUE SPACES.                  
020600     03 FILLER                   PIC X(06) VALUE 'Regdat'.                
020700     03 FILLER                   PIC X(03) VALUE SPACES.                  
020800     03 FILLER                   PIC X(06) VALUE 'PartNo'.                
020900     03 FILLER                   PIC X(05) VALUE SPACES.                  
021000     03 FILLER                   PIC X(08) VALUE 'Quantity'.              
021100     03 FILLER                   PIC X(02) VALUE SPACES.                  
021200     03 FILLER                   PIC X(05) VALUE 'Distr'.                 
021300     03 FILLER                   PIC X(02) VALUE SPACES.                  
021400     03 FILLER                   PIC X(07) VALUE 'Dealer'.                
021500     03 FILLER                   PIC X(02) VALUE SPACES.                  
021600     03 FILLER                   PIC X(07) VALUE 'OrderNo'.               
021700*                                                                         
021800 01  WS-DET-DET-REC.                                                      
021900     03 WS-DET-IDLEVNR           PIC X(05) VALUE SPACES.                  
022000     03 FILLER                   PIC X(05) VALUE SPACES.                  
022100     03 WS-DET-REGDAT            PIC Z9(6) VALUE ZEROS.                   
022200     03 FILLER                   PIC X(02) VALUE SPACES.                  
022300     03 WS-DET-IDARTNR           PIC Z(8)9 VALUE ZEROS.                   
022400     03 FILLER                   PIC X(02) VALUE SPACES.                  
022500     03 WS-DET-KVBEART           PIC Z(6)9 VALUE ZEROS.                   
022600     03 FILLER                   PIC X(03) VALUE SPACES.                  
022700     03 WS-DET-IDDISTR           PIC Z(4)9 VALUE ZEROS.                   
022800     03 FILLER                   PIC X(02) VALUE SPACES.                  
022900     03 WS-DET-IDKUNDNR          PIC Z(6)9 VALUE ZEROS.                   
023000     03 FILLER                   PIC X(02) VALUE SPACES.                  
023100     03 WS-DET-IDKUNDRF-ED       PIC Z(6)9 VALUE ZEROS.                   
023200*                                                                         
023300 PROCEDURE DIVISION.                                                      
023400 MAIN SECTION.                                                            
023500                                                                          
023600     PERFORM A-INIT                                                       
023700     PERFORM S01-READ-W41412                                              
023800*                                                                         
023900     IF NOT END-OF-W41412                                                 
024000       PERFORM D-INIT-FIELDS                                              
024100     END-IF                                                               
024200                                                                          
024300     PERFORM UNTIL END-OF-W41412                                          
024400       IF IN-BUMU-IDDC-STEER = WS-SAVE-IDDC                               
024500         IF IN-BUMU-IDLEVNR = WS-SAVE-IDLEVNR                             
024600           PERFORM B-PROCESS-INPUT                                        
024700         ELSE                                                             
024800           COMPUTE IX = IX + 1                                            
024900           MOVE IN-BUMU-IDLEVNR  TO WS-SAVE-IDLEVNR                       
025000           PERFORM B-PROCESS-INPUT                                        
025100         END-IF                                                           
025200       ELSE                                                               
025300         PERFORM C-CREATE-W41413                                          
025400         PERFORM D-INIT-FIELDS                                            
025500         PERFORM B-PROCESS-INPUT                                          
025600       END-IF                                                             
025700       PERFORM S01-READ-W41412                                            
025800     END-PERFORM                                                          
025900                                                                          
026000     IF WS-TOT-LINES > ZERO                                               
026100       PERFORM C-CREATE-W41413                                            
026200     END-IF                                                               
026300                                                                          
026400     PERFORM Z-FINIT                                                      
026500                                                                          
026600     MOVE ZERO TO RETURN-CODE                                             
026700     GOBACK                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 A-INIT SECTION.                                                          
027100     OPEN INPUT  W41412                                                   
027200                                                                          
027300     OPEN OUTPUT W41413                                                   
027400                 W41414                                                   
027500*                                                                         
027600     CALL DATKORT USING PROGRAM-NAME DATECARD-ID DATUMKORT                
027700     MOVE D-AAR     TO  TODAYS-DATE-YEAR                                  
027800     MOVE D-MAANAD  TO  TODAYS-DATE-MONTH                                 
027900     MOVE D-DAG     TO  TODAYS-DATE-DAY                                   
028000     MOVE IDPGM     TO POSTSUM-PROGNAMN                                   
028100*                                                                         
028200     .                                                                    
028300     EJECT                                                                
028400                                                                          
028500 B-PROCESS-INPUT SECTION.                                                 
028600                                                                          
028700     IF IX <= 10                                                          
028800       MOVE IN-BUMU-IDLEVNR      TO WS-TAB-IDLEVNR(IX)                    
028900       IF IN-BUMU-IDDC-STEER = IN-BUMU-IDDC                               
029000         COMPUTE WS-TAB-DC-CNT (IX)  = WS-TAB-DC-CNT (IX) + 1             
029100       ELSE                                                               
029200         PERFORM BA-CREATE-W41414                                         
029300       END-IF                                                             
029400       COMPUTE WS-TAB-LEVNR-CNT (IX) = WS-TAB-LEVNR-CNT (IX) + 1          
029500       COMPUTE WS-TOT-LINES          = WS-TOT-LINES + 1                   
029600     END-IF                                                               
029700                                                                          
029800     .                                                                    
029900     EJECT                                                                
030000                                                                          
030100 BA-CREATE-W41414 SECTION.                                                
030200     MOVE IN-BUMU-IDLEVNR        TO WS-DET-IDLEVNR                        
030300     MOVE IN-BUMU-TIREGDAT       TO WS-DET-REGDAT                         
030400     MOVE IN-BUMU-IDARTNR        TO WS-DET-IDARTNR                        
030500     MOVE IN-BUMU-KVBEART        TO WS-DET-KVBEART                        
030600     MOVE IN-BUMU-IDDISTR        TO WS-DET-IDDISTR                        
030700     MOVE IN-BUMU-IDKUNDNR       TO WS-DET-IDKUNDNR                       
030800     MOVE IN-BUMU-IDKUNDRF       TO WS-DET-IDKUNDRF                       
030900     MOVE WS-DET-IDORDNR7        TO WS-DET-IDKUNDRF-ED                    
031000     MOVE WS-DET-DET-REC         TO WS-DET-RECORD                         
031100     PERFORM S12-WRITE-W41414                                             
031200     .                                                                    
031300 C-CREATE-W41413 SECTION.                                                 
031400                                                                          
031500     MOVE WS-SUM-HDR-REC         TO WS-SUM-RECORD                         
031600     PERFORM S11-WRITE-W41413                                             
031700                                                                          
031800     MOVE WS-BLANK-LINE          TO WS-SUM-RECORD                         
031900     PERFORM S11-WRITE-W41413                                             
032000                                                                          
032100     MOVE +1 TO IX                                                        
032200     PERFORM UNTIL IX > 10 OR WS-TAB-IDLEVNR(IX) = SPACE                  
032300       MOVE WS-TAB-LEVNR-CNT(IX) TO  WS-SUM-CNT                           
032400       MOVE WS-TAB-IDLEVNR(IX)   TO  WS-SUM-IDLEVNR                       
032500       COMPUTE WS-TOT-CNT-DC = WS-TOT-CNT-DC +                            
032600                                 WS-TAB-DC-CNT(IX)                        
032700       MOVE WS-SUMMARY-REC       TO WS-SUM-RECORD                         
032800       PERFORM S11-WRITE-W41413                                           
032900       COMPUTE IX = IX + 1                                                
033000     END-PERFORM                                                          
033100                                                                          
033200     MOVE WS-TOT-LINES           TO WS-TOT-LINES-ED                       
033300*                                                                         
033400     MOVE WS-BLANK-LINE          TO WS-SUM-RECORD                         
033500     PERFORM S11-WRITE-W41413                                             
033600*                                                                         
033700     MOVE WS-TOTAL-REC           TO WS-SUM-RECORD                         
033800     PERFORM S11-WRITE-W41413                                             
033900*                                                                         
034000     MOVE WS-BLANK-LINE          TO WS-SUM-RECORD                         
034100     PERFORM S11-WRITE-W41413                                             
034200*                                                                         
034300     MOVE WS-TOT-CNT-DC          TO WS-TOT-CNT-DC-ED                      
034400     IF WS-TOT-LINES > 0                                                  
034500       COMPUTE WS-SERV-DC =                                               
034600               (WS-TOT-CNT-DC / WS-TOT-LINES) * 100                       
034700     END-IF                                                               
034800     MOVE WS-SERV-DC             TO WS-SERV-DC-ED                         
034900*                                                                         
035000     MOVE WS-TOTAL-DC-REC        TO WS-SUM-RECORD                         
035100     PERFORM S11-WRITE-W41413                                             
035200*                                                                         
035300     MOVE WS-SERV-DC-REC         TO WS-SUM-RECORD                         
035400     PERFORM S11-WRITE-W41413                                             
035500*                                                                         
035600     MOVE WS-BLANK-LINE          TO WS-SUM-RECORD                         
035700     PERFORM S11-WRITE-W41413                                             
035800*                                                                         
035900     MOVE +1 TO IX                                                        
036000     PERFORM UNTIL IX > 10 OR WS-TAB-IDLEVNR(IX) = SPACE                  
036100       MOVE  WS-TAB-DC-CNT(IX)   TO WS-LEVNR-CNT                          
036200       MOVE  WS-TAB-IDLEVNR(IX)  TO WS-LEVNR-IDLEVNR                      
036300                                    WS-SERV-IDLEVNR                       
036400       MOVE WS-LEVNR-CNT         TO WS-LEVNR-CNT-ED                       
036500       IF WS-TAB-LEVNR-CNT(IX) > 0                                        
036600         COMPUTE WS-SERV-DC-DET =                                         
036700               (WS-TAB-DC-CNT(IX) / WS-TAB-LEVNR-CNT(IX)) * 100           
036800       END-IF                                                             
036900       MOVE WS-SERV-DC-DET       TO WS-SERV-DC-DET-ED                     
037000*                                                                         
037100       MOVE WS-LEVNR-REC         TO WS-SUM-RECORD                         
037200       PERFORM S11-WRITE-W41413                                           
037300*                                                                         
037400       MOVE WS-SERV-DC-DET-REC   TO WS-SUM-RECORD                         
037500       PERFORM S11-WRITE-W41413                                           
037600*                                                                         
037700       MOVE WS-BLANK-LINE        TO WS-SUM-RECORD                         
037800       PERFORM S11-WRITE-W41413                                           
037900*                                                                         
038000       COMPUTE IX = IX + 1                                                
038100     END-PERFORM                                                          
038200     .                                                                    
038300     EJECT                                                                
038400                                                                          
038500 D-INIT-FIELDS SECTION.                                                   
038600                                                                          
038700     MOVE TODAYS-DATE            TO WS-SUM-DATE                           
038800                                    WS-DET-DATE                           
038900     MOVE +1                     TO IX                                    
039000     MOVE IN-BUMU-IDDC-STEER     TO WS-SAVE-IDDC                          
039100     MOVE IN-BUMU-IDLEVNR        TO WS-SAVE-IDLEVNR                       
039200     MOVE ZEROES                 TO WS-TOT-LINES                          
039300                                    WS-TOT-CNT-DC                         
039400                                    WS-SERV-DC                            
039500                                    WS-LEVNR-CNT                          
039600                                    WS-SERV-DC-DET                        
039700                                                                          
039800     MOVE IN-BUMU-IDDC-STEER     TO WS-DAP-SUM-IDDC                       
039900                                    WS-SUM-IDDC                           
040000                                    WS-TOT-IDDC                           
040100                                    WS-SERV-IDDC                          
040200                                    WS-LEVNR-IDDC                         
040300                                    WS-SERV-IDDC-DET                      
040400                                    WS-DAP-DET-IDDC                       
040500                                    WS-DET-IDDC                           
040600                                                                          
040700     INITIALIZE WS-TABLE                                                  
040800                                                                          
040900     MOVE WS-DAP-SUM1            TO WS-SUM-RECORD                         
041000     PERFORM S11-WRITE-W41413                                             
041100                                                                          
041200     MOVE WS-DAP-SUM2            TO WS-SUM-RECORD                         
041300     PERFORM S11-WRITE-W41413                                             
041400                                                                          
041500     MOVE WS-DAP-DET1            TO WS-DET-RECORD                         
041600     PERFORM S12-WRITE-W41414                                             
041700                                                                          
041800     MOVE WS-DAP-DET2            TO WS-DET-RECORD                         
041900     PERFORM S12-WRITE-W41414                                             
042000                                                                          
042100     MOVE WS-DET-HDR-REC1        TO WS-DET-RECORD                         
042200     PERFORM S12-WRITE-W41414                                             
042300                                                                          
042400     MOVE WS-BLANK-LINE          TO WS-DET-RECORD                         
042500     PERFORM S12-WRITE-W41414                                             
042600                                                                          
042700     MOVE WS-DET-HDR-REC2        TO WS-DET-RECORD                         
042800     PERFORM S12-WRITE-W41414                                             
042900                                                                          
043000     MOVE WS-BLANK-LINE          TO WS-DET-RECORD                         
043100     PERFORM S12-WRITE-W41414                                             
043200     .                                                                    
043300     EJECT                                                                
043400                                                                          
043500 Z-FINIT SECTION.                                                         
043600     CLOSE W41412                                                         
043700           W41413                                                         
043800           W41414                                                         
043900                                                                          
044000     MOVE 'S' TO POSTSUM-OPKOD                                            
044100     CALL POSTSUM USING POSTSUM-PARM                                      
044200     .                                                                    
044300     EJECT                                                                
044400 S01-READ-W41412  SECTION.                                                
044500     READ W41412 INTO IN-AREA                                             
044600     AT END                                                               
044700        SET END-OF-W41412  TO TRUE                                        
044800                                                                          
044900     NOT AT END                                                           
045000        MOVE 'W41412'      TO POSTSUM-FDNAMN                              
045100        MOVE 'W41409D1'    TO POSTSUM-DDNAMN2                             
045200        MOVE SPACE         TO POSTSUM-TRANSTYP                            
045300        CALL POSTSUM USING POSTSUM-PARM                                   
045400     END-READ                                                             
045500     .                                                                    
045600 S11-WRITE-W41413 SECTION.                                                
045700     WRITE WS-SUM-RECORD                                                  
045800                                                                          
045900     MOVE SPACE         TO POSTSUM-TRANSTYP                               
046000     MOVE 'W41413'      TO POSTSUM-FDNAMN                                 
046100     MOVE 'W41409D2'    TO POSTSUM-DDNAMN2                                
046200     CALL POSTSUM USING POSTSUM-PARM                                      
046300     .                                                                    
046400 S12-WRITE-W41414 SECTION.                                                
046500     WRITE WS-DET-RECORD                                                  
046600                                                                          
046700     MOVE SPACE         TO POSTSUM-TRANSTYP                               
046800     MOVE 'W41414'      TO POSTSUM-FDNAMN                                 
046900     MOVE 'W41409D3'    TO POSTSUM-DDNAMN2                                
047000     CALL POSTSUM USING POSTSUM-PARM                                      
047100     .                                                                    
047200 S99-ABEND SECTION.                                                       
047300     MOVE 'S' TO POSTSUM-OPKOD                                            
047400     CALL POSTSUM USING POSTSUM-PARM                                      
047500     CALL ABEND USING RKOD-ABEND                                          
047600     .                                                                    

000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9011800.                                                
000300 AUTHOR.         GÖRAN KJELLSON                                           
000400 DATE-WRITTEN.   SPRING 2020                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        STOCK AVAILIBILITY - GRIP.                                       
000900*                                                                         
001000*                                                                         
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W90118T                                             
001400*        MID:         W90118I1                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W90118O1                                            
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000 DATA DIVISION.                                                           
002100                                                                          
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400 77    IDPGM                     PIC X(8)    VALUE 'W9011800'.            
002500 77    CURRENT-SECTION           PIC X(16)   VALUE SPACE.                 
002600 77    CURRENT-IMS-SECTION       PIC X(16)   VALUE SPACE.                 
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77    ERROR-TEXT                PIC X(80) VALUE SPACE.                   
002900 77    FELTEXT                   PIC X(80) VALUE SPACE.                   
003000 77    KDRC-DISPLAY              PIC Z(5).                                
003100 77    RKOD-ABEND-WITH-DUMP      PIC S9(4)   COMP VALUE +1000.            
003200                                                                          
003300 77    YES                       PIC X       VALUE 'Y'.                   
003400 77    JA                        PIC X       VALUE 'J'.                   
003500 77    NOO                       PIC X       VALUE 'N'.                   
003600                                                                          
003700 77    WS-KVAKS                  PIC S9(8)   VALUE ZERO.                  
003800 77    WS-RETURNS                PIC S9(8)   VALUE ZERO.                  
003900 77    W-IDARTNR-TILLK           PIC 9(8)    VALUE ZERO.                  
003910 77    WS-KVLS                   PIC S9(7)   VALUE ZERO.                  
004000                                                                          
004100 77  WS-KDAKDISP                 PIC 9       VALUE 0.                     
004200     88  NO-AKS                              VALUE 0.                     
004300     88  FULL-AKS                            VALUE 1.                     
004400     88  LIMITED-AKS                         VALUE 2.                     
004500 01  WS-ETA-DATUM-NUM            PIC 9(6).                                
004600 01  WS-ETA-DATUM REDEFINES WS-ETA-DATUM-NUM.                             
004700     03  WS-ETA-DATUM-AAR        PIC 9(2).                                
004800     03  WS-ETA-DATUM-MAANAD     PIC 9(2).                                
004900     03  WS-ETA-DATUM-DAG        PIC 9(2).                                
005000                                                                          
005100 77    ALL-OK-SW                 PIC X       VALUE 'Y'.                   
005200   88  ALL-OK                                VALUE 'Y'.                   
005300   88  ALL-NOT-OK                            VALUE 'N'.                   
005400                                                                          
005500*      --- VALID IDDC CODES                                               
005600*                                                                         
005700*01   -COPY WWDC99                                                        
005800                                                                          
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006200   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
006300   03  WZ01SUB                   PIC X(8)    VALUE 'WZ01SUB '.            
006400   03  W411AREG                  PIC X(8)    VALUE 'W411AREG'.            
006500   03  W411SPAR                  PIC X(8)    VALUE 'W411SPAR'.            
006600   03  W218ETA                   PIC X(8)    VALUE 'W218ETA '.            
006700*                                                                         
006800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006900                                                                          
007000*01  -COPY WZ01SUB                                                        
007100                                                                          
007200 01   FILLER             PIC X(5)  VALUE 'AREG '.                         
007300*   -COPY W411AREG                                                        
007400                                                                          
007500 01   FILLER             PIC X(5)  VALUE 'SPAR '.                         
007600*   -COPY W411SPAR                                                        
007700                                                                          
007800 01   FILLER             PIC X(5)  VALUE 'LETA '.                         
007900*   -COPY W218LETA -PRE ETA-.                                             
008000******************************************************************        
008100*                                                                         
008200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008500                                                                          
008600 01  REQU-AREA.                                                           
008700*    03  -COPY WZ01REQU                                                   
008800*    03  -COPY W90118I1                                                   
008900                                                                          
009000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009100                                                                          
009200 01  RESP-AREA.                                                           
009300*    03  -COPY WZ01RESP                                                   
009400*    03  -COPY W90118O1                                                   
009500                                                                          
009600 01  MESSAGE-CODES.                                                       
009700     03  SYS-ERROR               PIC X(3)    VALUE '099'.                 
009800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009900                                                                          
010000                                                                          
010100*    --- WORK-AREA FOR IMS-SECTIONS                                       
010200*                                                                         
010300                                                                          
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500 01  KEYS-FOR-DLI.                                                        
010600     03  W-IDARTNR-X.                                                     
010700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010800     03  w-IDDC-X.                                                        
010900         05  W-IDDC                  PIC X(2).                            
011000     03  W-WDGX01KEY-X.                                                   
011100         05  W-IDHTYP            PIC  X(4)   VALUE '4521'.                
011200         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
011300     03  W-IDSKYLT-X.                                                     
011400         05  W-IDSKYLT           PIC  X(3)   VALUE 'GB '.                 
011500     03  W-WDGX11KEY-X.                                                   
011600         05  W-KDORDBEK          PIC  9(2)   VALUE ZERO.                  
011700         05  W-IDSKYLT-WDGX      PIC  X(3)   VALUE 'GB '.                 
011800     03  W-IDPTYP-X.                                                      
011900         05  W-IDPTYP            PIC X(3)    VALUE '310'.                 
012000     03  W-KDRT-07-X.                                                     
012100         05  W-KDRT-07           PIC S9(3)   VALUE 7    COMP-3.           
012200     03  W-KDRT-77-X.                                                     
012300         05  W-KDRT-77           PIC S9(3)   VALUE 77   COMP-3.           
012400     03  W-FLTEXT-X.                                                      
012500         05  W-FLTEXT            PIC  X(1)   VALUE 'N'.                   
012600                                                                          
012700                                                                          
012800*    --- IMS FUNKTIONSKODER                                               
012900*01  -COPY W0003                                                          
013000                                                                          
013100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
013200 01  DLI-IO-WDK611.                                                       
013300*    03  -COPY WDK611                                                     
013400                                                                          
013500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
013600 01  DLI-IO-WDK711.                                                       
013700*    03  -COPY WDK711                                                     
013800                                                                          
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
014000 01  DLI-IO-WDB601.                                                       
014100*    03  -COPY WDB601                                                     
014200                                                                          
014300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL601'.         
014400 01  DLI-IO-WDL601.                                                       
014500*    03  -COPY WDL601                                                     
014600                                                                          
014700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL611'.         
014800 01  DLI-IO-WDL611.                                                       
014900*    03  -COPY WDL611                                                     
015000                                                                          
015100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
015200 01  DLI-IO-WDD311.                                                       
015300*    03  -COPY WDD311                                                     
015400                                                                          
015500 01  FILLER         PIC X(16) VALUE 'DLI-IO-XXKJ11'.                      
015600 01  DLI-IO-XXKJ11.                                                       
015700*    03  -COPY WDGX4522                                                   
015800                                                                          
015900 01  FILLER                      PIC X(16)   VALUE 'WDD701-AREA'.         
016000 01  DLI-IO-WDD701.                                                       
016100*    03  -COPY WDD701                                                     
016200                                                                          
016300 01  FILLER                      PIC X(16)   VALUE 'WDD702-AREA'.         
016400 01  DLI-IO-WDD702.                                                       
016500*    03  -COPY WDD702                                                     
016600                                                                          
016700*    --- STATUS-KOD FRÅN IMS                                              
016800 01  STATUS-WS                   PIC XX.                                  
016900     88  SEGMENT-FOUND                       VALUE '  '.                  
017000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017100                                                                          
017200 01  GODK-STATUSKODER.                                                    
017300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017400                                                                          
017500 01  ALL-SSA.                                                             
017600    03 SSA1                      PIC X(160).                              
017700    03 SSA2                      PIC X(96).                               
017800                                                                          
017900                                                                          
018000                                                                          
018100                                                                          
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400*01  -COPY W0009     -PRE MSG-                                            
018500                                                                          
018600*01  -COPY W0008     -PRE WDK6-                                           
018700     05  FILLER                  PIC X.                                   
018800*01  -COPY W0008     -PRE WDK7-                                           
018900     05  FILLER                  PIC X.                                   
019000*01  -COPY W0008     -PRE WDB6-                                           
019100     05  FILLER                  PIC X.                                   
019200*01  -COPY W0008     -PRE WDL6-                                           
019300     05  FILLER                  PIC X.                                   
019400*01  -COPY W0008     -PRE XXKJ-                                           
019500     05  FILLER                  PIC X.                                   
019600*01  -COPY W0008     -PRE BENA-                                           
019700     05  FILLER                  PIC X.                                   
019800*01  -COPY W0008     -PRE WDD7-                                           
019900     05  FILLER                  PIC X.                                   
020000                                                                          
020100 01  AREG-WDK6-PCB          PIC X.                                        
020200 01  AREG-WDK7-PCB          PIC X.                                        
020300                                                                          
020400 01  SPAR-WDF8-PCB          PIC X.                                        
020500 01  SPAR-WDF8A-PCB         PIC X.                                        
020600 01  SPAR-WDK6-PCB          PIC X.                                        
020700                                                                          
020800 01  ETA-ARTC-PCB                PIC X.                                   
020900 01  ETA-WDK7-PCB                PIC X.                                   
021000 01  ETA-INLC-PCB                PIC X.                                   
021100 01  ETA-LEVA-PCB                PIC X.                                   
021200 01  ETA-WDB6-PCB                PIC X.                                   
021300 01  ETA-WDD9-PCB                PIC X.                                   
021400                                                                          
021500                                                                          
021600 PROCEDURE DIVISION  USING MSG-PCB                                        
021700                                                                          
021800                     WDK6-PCB  WDK7-PCB  WDB6-PCB                         
021900                     WDL6-PCB  XXKJ-PCB  BENA-PCB WDD7-PCB                
022000                                                                          
022100                     AREG-WDK6-PCB AREG-WDK7-PCB                          
022200                                                                          
022300                     SPAR-WDF8-PCB SPAR-WDF8A-PCB SPAR-WDK6-PCB           
022400                                                                          
022500                     ETA-ARTC-PCB  ETA-WDK7-PCB   ETA-INLC-PCB            
022600                     ETA-LEVA-PCB  ETA-WDB6-PCB   ETA-WDD9-PCB.           
022700                                                                          
022800 MAIN SECTION.                                                            
022900     ENTRY 'DLITCBL' USING MSG-PCB                                        
023000                                                                          
023100                     WDK6-PCB  WDK7-PCB  WDB6-PCB                         
023200                     WDL6-PCB  XXKJ-PCB  BENA-PCB WDD7-PCB                
023300                                                                          
023400                     AREG-WDK6-PCB AREG-WDK7-PCB                          
023500                                                                          
023600                     SPAR-WDF8-PCB SPAR-WDF8A-PCB SPAR-WDK6-PCB           
023700                                                                          
023800                     ETA-ARTC-PCB  ETA-WDK7-PCB   ETA-INLC-PCB            
023900                     ETA-LEVA-PCB  ETA-WDB6-PCB   ETA-WDD9-PCB.           
024000                                                                          
024100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
024200     IF SUB-KDRC = 0                                                      
024300       IF REQU-KDPGMACT = 'S'                                             
024400                                                                          
024500          PERFORM A-INIT-SPARA-INPUT                                      
024600          IF REQU-IDARTNR NUMERIC                                         
024700            PERFORM B-CHECK-PART-AND-DC                                   
024800          ELSE                                                            
024900            MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                        
025000            MOVE NOO           TO ALL-OK-SW                               
025100          END-IF                                                          
025200          IF ALL-OK                                                       
025300* INGET D/K  PERFORM C-CHECK-PART-BLOCKS                                  
025400             IF ALL-OK                                                    
025500                IF DCS-CDC                                                
025600                   PERFORM D-CHECK-CDC-STOCK                              
025700                ELSE                                                      
025800                   PERFORM E-CHECK-SDC-NDC-STOCK                          
025900                END-IF                                                    
026000                IF RESP-KVLS = ZERO                                       
026100                   PERFORM F-CHECK-DISP-DATE                              
026200                END-IF                                                    
026300             END-IF                                                       
026400          END-IF                                                          
026410          IF  RESP-KVLS    > ZERO                                         
026420          AND (RESP-KDORDBEK = 41 OR 61 OR 54)                            
026430             MOVE SPACE         TO RESP-KDORDBEK                          
026431             INSPECT RESP-IDARTNR-TILLK REPLACING CHARACTERS              
026432                                        BY SPACE                          
026440          END-IF                                                          
026500          IF (RESP-KDORDBEK = SPACE OR '22')                              
026600             CONTINUE                                                     
026700          ELSE                                                            
026800             PERFORM G-GET-CONF-TEXT                                      
026900          END-IF                                                          
027000       ELSE                                                               
027100         MOVE SYS-ERROR    TO RESP-IDMSG-ERROR                            
027200       END-IF                                                             
027300                                                                          
027400       PERFORM S02-RETURN-RESPONSE                                        
027500     END-IF                                                               
027600                                                                          
027700     MOVE ZERO                         TO RETURN-CODE                     
027800                                                                          
027900     GOBACK                                                               
028000     .                                                                    
028100                                                                          
028200                                                                          
028300 A-INIT-SPARA-INPUT SECTION.                                              
028400     MOVE 'A-INIT-SPARA-INPUT' TO CURRENT-SECTION                         
028500                                                                          
028600     MOVE 001                    TO RESP-IDMSGVER                         
028700     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
028800                                    RESP-IDMSG-INFO                       
028900                                    RESP-IDELMT-ERROR                     
029000     MOVE SPACE                  TO RESP-W90118O1-CTX                     
029100     MOVE REQU-IDARTNR           TO RESP-IDARTNR                          
029200     MOVE REQU-IDDC              TO RESP-IDDC                             
029300                                                                          
029400     .                                                                    
029500                                                                          
029600                                                                          
029700 B-CHECK-PART-AND-DC SECTION.                                             
029800     MOVE 'B-CHECK-PART-DC' TO CURRENT-SECTION                            
029900                                                                          
030000     MOVE REQU-IDARTNR       TO AREG-IDARTNR                              
030100                                W-IDARTNR                                 
030200     MOVE REQU-IDDC          TO AREG-IDDC                                 
030300                                W-IDDC                                    
030400                                WS-IDDC                                   
030500     MOVE YES                TO ALL-OK-SW                                 
030600                                                                          
030700     PERFORM IMS-GU-WDB601                                                
030800     IF SEGMENT-FOUND                                                     
030900        CALL W411AREG USING AREG-W411AREG                                 
031000                            AREG-WDK6-PCB                                 
031100                            AREG-WDK7-PCB                                 
031200                                                                          
031300        IF AREG-KDORDBEK > 0 OR AREG-KDERS-UTG > 0                        
031400           IF AREG-KDORDBEK > 0                                           
031500              MOVE AREG-KDORDBEK TO RESP-KDORDBEK                         
031600           ELSE                                                           
031700              MOVE 41            TO RESP-KDORDBEK                         
031800           END-IF                                                         
031801           IF RESP-KDORDBEK      = 41                                     
031802              CONTINUE                                                    
031810           ELSE                                                           
031900              MOVE NOO           TO ALL-OK-SW                             
031910           END-IF                                                         
032000        ELSE                                                              
032100           EVALUATE TRUE                                                  
032200             WHEN AREG-KDERS = 19 OR 29                                   
032300               MOVE 54 TO RESP-KDORDBEK                                   
032400*cc*           MOVE NOO TO ALL-OK-SW                                      
032500                                                                          
032600             WHEN AREG-KDERS = 52                                         
032700               MOVE 52 TO RESP-KDORDBEK                                   
032800               MOVE NOO TO ALL-OK-SW                                      
032900                                                                          
033000             WHEN (AREG-KDERS > 10 AND < 14) OR                           
033100                  (AREG-KDERS > 20 AND < 24) OR                           
033200                  (AREG-KDERS = 17 OR 27)                                 
033300               MOVE 41 TO RESP-KDORDBEK                                   
033310               IF AREG-KDERS = 13 or 23                                   
033400                  MOVE NOO TO ALL-OK-SW                                   
033410*cc*           MOVE NOO TO ALL-OK-SW                                      
033420               END-IF                                                     
033500                                                                          
033600             WHEN (AREG-KDERS > 13 AND < 17) OR                           
033700                  (AREG-KDERS > 23 AND < 27) OR                           
033800                  (AREG-KDERS = 18 OR 28)                                 
033900               MOVE 61 TO RESP-KDORDBEK                                   
033910               IF AREG-KDERS = 16 or 26                                   
033920                  MOVE NOO TO ALL-OK-SW                                   
034000*cc*           MOVE NOO TO ALL-OK-SW                                      
034010               END-IF                                                     
034100                                                                          
034200           END-EVALUATE                                                   
034300           IF AREG-KDORDBEK NOT = 58                                      
034400              PERFORM BA-ADD-PART-INFO                                    
034500           END-IF                                                         
034600        END-IF                                                            
034700        IF RESP-KDORDBEK = 41 OR 61                                       
034800           PERFORM IMS-GU-WDD701                                          
034900           IF SEGMENT-FOUND                                               
035000              PERFORM IMS-GNP-WDD702                                      
035100              IF SEGMENT-FOUND                                            
035200                 MOVE IDARTNR-TILLK      TO W-IDARTNR-TILLK               
035300                 PERFORM IMS-GNP-WDD702                                   
035400                 IF SEGMENT-MISSING                                       
035500                    MOVE W-IDARTNR-TILLK TO RESP-IDARTNR-TILLK            
035600                 END-IF                                                   
035700              END-IF                                                      
035800           END-IF                                                         
035900        END-IF                                                            
036000     ELSE                                                                 
036100        MOVE '22'           TO RESP-KDORDBEK                              
036200        MOVE 'DC NOT VALID' TO RESP-TEORDBEK                              
036300        MOVE NOO            TO ALL-OK-SW                                  
036400     END-IF                                                               
036500     .                                                                    
036600                                                                          
036700                                                                          
036800 BA-ADD-PART-INFO SECTION.                                                
036900     MOVE 'BA-ADD-PART-INFO' TO CURRENT-SECTION                           
037000                                                                          
037100     MOVE AREG-KDSORT        TO RESP-KDSORT                               
037200     PERFORM IMS-GU-BENA11-BSEQ                                           
037300     IF SEGMENT-FOUND                                                     
037400        MOVE TEXT-BEART      TO RESP-BEART                                
037500     END-IF                                                               
037600     .                                                                    
037700                                                                          
037800                                                                          
037900 C-CHECK-PART-BLOCKS SECTION.                                             
038000     MOVE 'C-CHECK-PART-BLO' TO CURRENT-SECTION                           
038100                                                                          
038200     MOVE SPACE                TO SPAR-BERADREF                           
038300     MOVE SPACE                TO SPAR-BEKUNDRF                           
038400     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
038500     MOVE NOO                  TO SPAR-FLEMBORD                           
038600     MOVE NOO                  TO SPAR-FLFORBI                            
038700     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
038800     MOVE NOO                  TO SPAR-FLORDSPE                           
038900     MOVE NOO                  TO SPAR-FLOVRLEV                           
039000     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
039100     MOVE SPACE                TO SPAR-FLRESTN                            
039200     MOVE AREG-IDARTNR         TO SPAR-IDARTNR                            
039300     MOVE AREG-FLIART          TO SPAR-FLIART                             
039400     MOVE SPACE                TO SPAR-FLMARKSP                           
039500*    MOVE KREG-IDDISTR         TO SPAR-IDDISTR                            
039600*    MOVE KREG-IDKUNDNR        TO SPAR-IDKUNDNR                           
039700     MOVE '0000000   '         TO SPAR-IDKUNDRF-RO                        
039800     MOVE AREG-IDDC            TO SPAR-IDDC                               
039900     MOVE 'VDI '               TO SPAR-IDSYSTEM                           
040000     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
040100     MOVE AREG-KDERS           TO SPAR-KDERS                              
040200     MOVE SPACE                TO SPAR-KDFAKTYP                           
040300     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
040400     MOVE +0                   TO SPAR-KDORDBEH                           
040500     MOVE 1                    TO SPAR-KDORDKL                            
040600     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
040700     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
040800     MOVE SPACE                TO SPAR-KDPRTYP                            
040900     MOVE +0                   TO SPAR-KDTPOTYP                           
041000     MOVE AREG-KDUART          TO SPAR-KDUART                             
041100     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
041200     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
041300     MOVE +0                   TO SPAR-TIRODAT                            
041400     MOVE +0                   TO SPAR-TITPO                              
041500     MOVE NOO                  TO SPAR-FLSDCLEV                           
041600     MOVE ZERO                 TO SPAR-TIREPDAT                           
041700                                                                          
041800     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
041900                                       SPAR-WDF8A-PCB                     
042000                                       SPAR-WDK6-PCB                      
042100     IF SPAR-KDORDBEK > ZERO                                              
042200        MOVE SPAR-KDORDBEK  TO RESP-KDORDBEK                              
042300        MOVE NOO            TO ALL-OK-SW                                  
042400     END-IF                                                               
042500     .                                                                    
042600                                                                          
042700                                                                          
042800 D-CHECK-CDC-STOCK SECTION.                                               
042900     MOVE 'D-CHECK-CDC     ' TO CURRENT-SECTION                           
043000                                                                          
043100     PERFORM IMS-GU-WDK611                                                
043200     IF SEGMENT-FOUND                                                     
043300        IF AREG-KDERS = +01                                               
043400           COMPUTE WS-KVLS   = CLAG-KVLS                                  
043500                             - CLAG-KVSPANT                               
043600                             - CLAG-KVRESS                                
043700                             - CLAG-KVSPARR-KVAL                          
043800        ELSE                                                              
043900           COMPUTE WS-KVLS   = CLAG-KVLS                                  
044000                             - CLAG-KVUTRS                                
044100                             - CLAG-KVSPANT                               
044200                             - CLAG-KVRESS                                
044300                             - CLAG-KVSPARR-KVAL                          
044400        END-IF                                                            
044500        IF WS-KVLS   < ZERO                                               
044600           MOVE ZERO     TO RESP-KVLS                                     
044700        ELSE                                                              
044710           MOVE WS-KVLS  TO RESP-KVLS                                     
044720        END-IF                                                            
044800     ELSE                                                                 
044900        MOVE ZERO        TO RESP-KVLS                                     
045000     END-IF                                                               
045100     .                                                                    
045200                                                                          
045300                                                                          
045400 E-CHECK-SDC-NDC-STOCK SECTION.                                           
045500     MOVE 'D-CHECK-SDC-NDC ' TO CURRENT-SECTION                           
045600                                                                          
045700     PERFORM IMS-GU-WDK711                                                
045800     IF SEGMENT-FOUND                                                     
045900        PERFORM EA-DECIDE-USE-OF-AKS                                      
046000        COMPUTE WS-KVLS   = SLAG-KVLS                                     
046100                          + WS-KVAKS                                      
046200                          - SLAG-KVOKS-DAG                                
046300                          - SLAG-KVRESS                                   
046400                          - SLAG-KVUTRS                                   
046500*..BARA FÖR KINA K722     - XLAG-KVSPANT                                  
046600                          - SLAG-KVSPARR-KVAL                             
046700        IF WS-KVLS   < ZERO                                               
046800           MOVE ZERO     TO RESP-KVLS                                     
046801        ELSE                                                              
046810           MOVE WS-KVLS  TO RESP-KVLS                                     
046900        END-IF                                                            
047000     ELSE                                                                 
047100        MOVE ZERO        TO RESP-KVLS                                     
047200     END-IF                                                               
047300     .                                                                    
047400                                                                          
047500                                                                          
047600 EA-DECIDE-USE-OF-AKS SECTION.                                            
047700     MOVE 'EA-DECIDE-USE-OF' TO CURRENT-SECTION                           
047800                                                                          
047900*    DCS-KDAKDISP IS CHANGED TO DCS-KDAKDISP-DAY/-BULK                    
048000*    WE DONT HAVE ANY ORDERCLASS TO DECIDE WHICH FIELD TO USE             
048100*    SO FOR NOW WE ALWAYS PUT ZERO TO WS-KVAKS                            
048200                                                                          
048300*    MOVE DCS-KDAKDISP         TO WS-KDAKDISP                             
048400*    IF NO-AKS                                                            
048500        MOVE ZERO              TO WS-KVAKS                                
048600*    ELSE                                                                 
048700*       IF FULL-AKS                                                       
048800*          MOVE SLAG-KVAKS-SDC TO WS-KVAKS                                
048900*       ELSE                                                              
049000*          PERFORM EA-COMPUTE-LIMITED-AKS                                 
049100*       END-IF                                                            
049200*    END-IF                                                               
049300     .                                                                    
049400                                                                          
049500                                                                          
049600 EA-COMPUTE-LIMITED-AKS SECTION.                                          
049700     MOVE 'EA-LIMITED-AKS  ' TO CURRENT-SECTION                           
049800                                                                          
049900     MOVE SLAG-KVAKS-SDC TO WS-KVAKS                                      
050000     MOVE ZERO           TO WS-RETURNS                                    
050100                                                                          
050200     PERFORM IMS-GU-WDL601                                                
050300     IF SEGMENT-FOUND                                                     
050400        PERFORM IMS-GNP-WDL611                                            
050500        PERFORM UNTIL SEGMENT-MISSING                                     
050600           COMPUTE WS-RETURNS = WS-RETURNS +                              
050700                   INL-KVAVIS - INL-KVANTMOT                              
050800           PERFORM IMS-GNP-WDL611                                         
050900        END-PERFORM                                                       
051000     END-IF                                                               
051100                                                                          
051200     COMPUTE WS-KVAKS = WS-KVAKS - WS-RETURNS                             
051300                                                                          
051400     .                                                                    
051500 F-CHECK-DISP-DATE  SECTION.                                              
051600     MOVE 'F-CHECK-DISP-D' TO CURRENT-SECTION                             
051700                                                                          
051800     MOVE REQU-IDDC               TO WS-IDDC                              
052000     IF NDC                                                               
052100        PERFORM FA-GET-ETA                                                
052200        IF ETA-SVAR-OK = JA                                               
052300           MOVE ETA-TIAAMMDD-SVAR TO RESP-TIBERANK                        
052400        END-IF                                                            
052500     ELSE                                                                 
052600        MOVE AREG-TIDISPIN        TO RESP-TIBERANK                        
052700     END-IF                                                               
052800     .                                                                    
052900                                                                          
053000                                                                          
053100 FA-GET-ETA         SECTION.                                              
053200     MOVE 'FA-GET-ETA        ' TO CURRENT-SECTION                         
053300                                                                          
053400     ACCEPT WS-ETA-DATUM FROM DATE                                        
053500     MOVE '612'                TO ETA-KDCALL                              
053600     MOVE REQU-IDDC            TO ETA-IDDC-REC                            
053700     MOVE AREG-IDARTNR         TO ETA-IDARTNR                             
053800     MOVE AREG-IDLEVNR         TO ETA-IDLEVNR                             
053900     MOVE ZERO                 TO ETA-KDFRAKT                             
054000     MOVE WS-ETA-DATUM-NUM     TO ETA-TIAAMMDD-ANROP                      
054100     MOVE 20                   TO ETA-TISEKEL-ANROP                       
054200                                                                          
054300     CALL W218ETA  USING ETA-W218LETA                                     
054400                         ETA-ARTC-PCB ETA-WDK7-PCB                        
054500                         ETA-INLC-PCB ETA-LEVA-PCB                        
054600                         ETA-WDB6-PCB ETA-WDD9-PCB                        
054700     .                                                                    
054800 G-GET-CONF-TEXT SECTION.                                                 
054900     MOVE 'G-GET-CONF-TEXT ' TO CURRENT-SECTION                           
055000                                                                          
055100     MOVE RESP-KDORDBEK    TO W-KDORDBEK                                  
055200     PERFORM IMS-GU-XXKJ11                                                
055300     IF SEGMENT-FOUND                                                     
055400        MOVE 4522-TEORDBEK TO RESP-TEORDBEK                               
055500     END-IF                                                               
055600     .                                                                    
055700                                                                          
055800                                                                          
055900*    --- DISPATCHER SECTIONS                                              
056000 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
056100                                                                          
056200     MOVE 'GETARG'               TO SUB-KDFUNC                            
056300     MOVE 'CARPARTS.PULS.STOCKVALUEQUERY'   TO SUB-ADDISPABS              
056400     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
056500                                                                          
056600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
056700                                                                          
056800     IF SUB-KDRC > 0                                                      
056900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
057000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
057100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
057200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057300     END-IF                                                               
057400     .                                                                    
057500                                                                          
057600                                                                          
057700 S02-RETURN-RESPONSE SECTION.                                             
057800                                                                          
057900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
058000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
058100                                                                          
058200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
058300                                                                          
058400     IF SUB-KDRC > 0                                                      
058500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
058600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
058700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
058800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
058900     END-IF                                                               
059000     .                                                                    
059100                                                                          
059200                                                                          
059300 IMS-GU-WDK611 SECTION.                                                   
059400     MOVE 'IMS-GU-WDK611   ' TO CURRENT-IMS-SECTION                       
059500                                                                          
059600     MOVE SPACE               TO ALL-SSA                                  
059700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
059800          DELIMITED BY SIZE INTO SSA1                                     
059900     MOVE 'WDK611   '         TO SSA2                                     
060000     MOVE '  GE'              TO GODK-STATUSKODER                         
060100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
060200     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
060300     PERFORM IMS-STATUSKONTROLL                                           
060400     .                                                                    
060500                                                                          
060600                                                                          
060700 IMS-GU-WDK711 SECTION.                                                   
060800     MOVE 'IMS-GU-WDK711   ' TO CURRENT-IMS-SECTION                       
060900                                                                          
061000     MOVE SPACE               TO ALL-SSA                                  
061100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
061200          DELIMITED BY SIZE INTO SSA1                                     
061300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
061400          DELIMITED BY SIZE INTO SSA2                                     
061500     MOVE '  GE'              TO GODK-STATUSKODER                         
061600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
061700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
061800     PERFORM IMS-STATUSKONTROLL                                           
061900     .                                                                    
062000                                                                          
062100                                                                          
062200 IMS-GU-WDB601    SECTION.                                                
062300     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
062400                                                                          
062500     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
062600          DELIMITED BY SIZE INTO SSA1                                     
062700     MOVE '  GE'              TO GODK-STATUSKODER                         
062800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
062900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
063000     PERFORM IMS-STATUSKONTROLL                                           
063100     .                                                                    
063200                                                                          
063300                                                                          
063400 IMS-GU-WDL601 SECTION.                                                   
063500     MOVE 'IMS-GU-WDL601   ' TO CURRENT-IMS-SECTION                       
063600                                                                          
063700     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
063800          DELIMITED BY SIZE INTO SSA1                                     
063900     MOVE '  GE'              TO GODK-STATUSKODER                         
064000     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
064100     MOVE WDL6-STATUS-CODE    TO STATUS-WS                                
064200     PERFORM IMS-STATUSKONTROLL                                           
064300     .                                                                    
064400                                                                          
064500                                                                          
064600 IMS-GNP-WDL611 SECTION.                                                  
064700     MOVE 'IMS-GNP-WDL611  '  TO CURRENT-IMS-SECTION                      
064800                                                                          
064900     STRING 'WDL611  (IDPTYP   =' W-IDPTYP-X                              
065000                    '&KDRT     =' W-KDRT-07-X                             
065100                    '!IDPTYP   =' W-IDPTYP-X                              
065200                    '&KDRT     =' W-KDRT-77-X ')'                         
065300          DELIMITED BY SIZE INTO SSA1                                     
065400     MOVE '  GE'              TO GODK-STATUSKODER                         
065500     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
065600     MOVE WDL6-STATUS-CODE    TO STATUS-WS                                
065700     PERFORM IMS-STATUSKONTROLL                                           
065800     .                                                                    
065900                                                                          
066000                                                                          
066100 IMS-GU-BENA11-BSEQ   SECTION.                                            
066200     MOVE 'IMS-GNP-BENA11-B' TO CURRENT-IMS-SECTION                       
066300                                                                          
066400     MOVE SPACE              TO ALL-SSA                                   
066500                                                                          
066600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
066700            DELIMITED BY SIZE INTO SSA1                                   
066800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
066900            DELIMITED BY SIZE INTO SSA2                                   
067000     MOVE '  GE' TO GODK-STATUSKODER                                      
067100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WDD311 SSA1 SSA2               
067200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
067300     PERFORM IMS-STATUSKONTROLL                                           
067400     .                                                                    
067500                                                                          
067600                                                                          
067700 IMS-GU-XXKJ11 SECTION.                                                   
067800     MOVE 'IMS-GU-XXKJ11   ' TO CURRENT-IMS-SECTION                       
067900                                                                          
068000     MOVE SPACE              TO ALL-SSA                                   
068100     STRING  'WLXXKJ01(WDGXKEY  =' W-WDGX01KEY-X ')'                      
068200            DELIMITED BY SIZE INTO SSA1                                   
068300     STRING  'WLXXKJ11(WDGXKEY >=' W-WDGX11KEY-X ')'                      
068400            DELIMITED BY SIZE INTO SSA2                                   
068500     MOVE    '  GE'             TO GODK-STATUSKODER                       
068600     CALL    CBLTDLI USING GU XXKJ-PCB DLI-IO-XXKJ11 SSA1 SSA2            
068700     MOVE    XXKJ-STATUS-CODE   TO STATUS-WS                              
068800     PERFORM IMS-STATUSKONTROLL                                           
068900     .                                                                    
069000                                                                          
069100                                                                          
069200 IMS-GU-WDD701 SECTION.                                                   
069300     MOVE 'IMS-GU-WDD701   ' TO CURRENT-IMS-SECTION                       
069400                                                                          
069500     MOVE SPACE              TO ALL-SSA                                   
069600                                                                          
069700     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
069800          DELIMITED BY SIZE INTO SSA1                                     
069900     MOVE 'WDD701   '         TO SSA2                                     
070000     MOVE '  GE'              TO GODK-STATUSKODER                         
070100     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
070200     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
070300     PERFORM IMS-STATUSKONTROLL                                           
070400     .                                                                    
070500                                                                          
070600 IMS-GNP-WDD702 SECTION.                                                  
070700     MOVE 'IMS-GNP-WDD702  ' TO CURRENT-IMS-SECTION                       
070800                                                                          
070900     MOVE SPACE              TO ALL-SSA                                   
071000                                                                          
071100     STRING 'WDD702  (FLTEXT   =' W-FLTEXT-X ')'                          
071200          DELIMITED BY SIZE INTO SSA1                                     
071300     MOVE '  GE'              TO GODK-STATUSKODER                         
071400     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
071500     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
071600     PERFORM IMS-STATUSKONTROLL                                           
071700     .                                                                    
071800                                                                          
071900 IMS-STATUSKONTROLL SECTION.                                              
072000                                                                          
072100     SET STATUS-IX TO 1                                                   
072200     SEARCH GODK-STATUS                                                   
072300       AT END                                                             
072400         CALL FELLOG                                                      
072500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
072600         CONTINUE                                                         
072700     END-SEARCH                                                           
072800     .                                                                    

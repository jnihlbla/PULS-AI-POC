000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF020200.                                                
000400 AUTHOR.         ANDERS HENRIKSSON.                                       
000500 DATE-WRITTEN.   02/01/17.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    OBS OBS OBS !!!                                                      
000900*    - IF THIS PGM IS ABENDED                                             
001000*      THE FIELD KDBEHS IN TAB T01SYST                                    
001100*      HAS TO BE UPDATED WITH SPACE                                       
001200*    OBS OBS OBS !!!                                                      
001300*                                                                         
001400*    NAME                                                                 
001500*        CARPARTS.BILLIT.VALIDATE                                         
001600*    FUNCTION.                                                            
001700*      1 THE PROGRAM IS STARTED BY ONE OF THE FOLLOWING ALT.              
001800*        . WF020100 (CARPARTS.BILLIT.RECEIVE)                             
001900*        . WF020200 (CARPARTS.BILLIT.VALIDATE)                            
002000*        . WF027800 (CARPARTS.BILLIT.VALIDATE) NOT A FUNCTION YET         
002100*      2 FORMAL/LOGICAL CONTROLS ARE PERFORMED ON EACH LINE               
002200*        WITHIN ALL NON-CONTROLLED BUNDLES                                
002300*      3 THE WHOLE BUNDLE WILL BE "ERROR-MARKED" IF ANY ERRORS ARE        
002400*        DISCOVERED IN THE LINES WITHIN THE BUNDLE                        
002500*      4 IF NEW NON-CONTROLLED BUNDLES/LINES HAVE BEEN INSERTED           
002600*          DURING THE EXECUTION PGM WILL BE RESTARTED (SEE ITEM 1)        
002700*      5 PGM WF020300 (CARPARTS.BILLIT.INSERT) IS STARTED                 
002800*        WHEN ALL NON-CONTROLLED BUNDLES HAVE BEEN VALIDATED AND          
002900*        THERE ARE NO ERROR-FREE/CONTROLLED/NOT ADDED BUNDLES LEFT        
003000*        (EXCEPT THE BUNDLES WICH HAVE JUST BEEN PROCESSED)               
003100*                                                                         
003200*        THE PROGRAM UPDATES ROWS IN TABLE T01TRAW                        
003300*        THE PROGRAM UPDATES ROWS IN TABLE T01TBUN                        
003400*        THE PROGRAM UPDATES ROWS IN TABLE T01SYST                        
003500*        THE PROGRAM READS   ROWS IN TABLE T01LSEL                        
003600*        THE PROGRAM READS   ROWS IN TABLE T01SECO                        
003700*        THE PROGRAM READS   ROWS IN TABLE T01RECO                        
003800*        THE PROGRAM READS   ROWS IN TABLE T01FCUS                        
003900*        THE PROGRAM READS   ROWS IN TABLE T01INRE                        
004000*        THE PROGRAM READS   ROWS IN TABLE T01VAT                         
004100*        THE PROGRAM READS   ROWS IN TABLE T01CURR                        
004200*        THE PROGRAM READS   ROWS IN TABLE T01DOTY                        
004300*        THE PROGRAM READS   ROWS IN TABLE T01CUGR                        
004400*        THE PROGRAM READS   ROWS IN TABLE T01BURE                        
004500*                                                                         
004600*    INDATA.                                                              
004700*        TRANSAKTION: WFT202X                                             
004800*        REQUEST:     HEADER ONLY                                         
004900*                                                                         
005000*    OUTDATA.                                                             
005100*        TRANSAKTION: WFT202X  (VALID WHEN STARTS ITSELF)                 
005200*        REQUEST:     HEADER ONLY                                         
005300*                                                                         
005400*        TRANSAKTION: WFT203X  (VALID WHEN START OF PGM WF20300)          
005500*        REQUEST:     HEADER ONLY                                         
005600*                                                                         
005700*        SÄNDNING VIA WZ01  TILL DAP                                      
005800*        MOD:         WZ04HDR (VIA WZ01) WF020201                         
005900                                                                          
006000 ENVIRONMENT DIVISION.                                                    
006100 DATA DIVISION.                                                           
006200 WORKING-STORAGE SECTION.                                                 
006300 77  IDPGM                       PIC X(08)  VALUE 'WF020200'.             
006400                                                                          
006500*    --------ERROR MESSAGE---                                             
006600 77  ERRORTEXT                   PIC X(80)  VALUE SPACE.                  
006700 77  KDRC-DISPLAY                PIC Z(5).                                
006800                                                                          
006900 77  YES                         PIC X      VALUE 'J'.                    
007000 77  NOO                         PIC X      VALUE 'N'.                    
007100                                                                          
007200 77  WS-FLFEL                    PIC X.                                   
007300 77  WS-BUNDLES-NOT-ADDED        PIC X     VALUE 'N'.                     
007400 77  WS-BUNDLES-NOT-CONTROLLED   PIC X     VALUE 'N'.                     
007500 77  RESTART-IX                  PIC S9(9) VALUE +0    COMP SYNC.         
007600 77  MAX-BUNTAR                  PIC S9(9) VALUE +1    COMP SYNC.         
007700 77  DAP-ERROR-COUNTER           PIC S9(9) VALUE +0    COMP SYNC.         
007800                                                                          
007900 01  WS-DATUM                    PIC  X(8).                               
008000 01  WS-CURRENT.                                                          
008100     03  WS-CURRENT-YEAR         PIC 9(2).                                
008200     03  WS-CURRENT-YEAR-PACKED  PIC S9(3) COMP-3.                        
008300     03  WS-DEFAULT-COUNTRY      PIC X(2)  VALUE 'XX'.                    
008400                                                                          
008500 77  KEYS-SW                     PIC X.                                   
008600     88  KEYS-OK                            VALUE 'J'.                    
008700     88  KEYS-ERROR                         VALUE 'N'.                    
008800                                                                          
008900 77  BUNT-SW                     PIC X.                                   
009000     88  BUNT-OK                            VALUE 'J'.                    
009100     88  BUNT-ERROR                         VALUE 'N'.                    
009200                                                                          
009300 77  BUNDLE-SW                   PIC X.                                   
009400     88  BUNDLE-OK                          VALUE 'J'.                    
009500     88  BUNDLE-ERROR                       VALUE 'N'.                    
009600                                                                          
009700 77  TRAW-KDANMORS-SW            PIC X(2)   VALUE SPACE.                  
009800     88  TRAW-KDANMORS-VALID  VALUE '32', '42', '52', '62', '72',         
009900                                    '75', '82', '92', '98',               
010000                                    '54', '94'.                           
010100     EJECT                                                                
010200                                                                          
010300*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
010400 01  GENERAL-SUBPROGRAMS.                                                 
010500     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
010600     03  WZ01SEND                PIC X(8)   VALUE 'WZ01SEND'.             
010700     03  WZ01RECV                PIC X(8)   VALUE 'WZ01RECV'.             
010800     EJECT                                                                
010900                                                                          
011000*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
011100                                                                          
011200 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
011300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
011400 77  RKOD-ABEND-DB2              PIC S9(4)  COMP VALUE +998.              
011500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
011600     SKIP2                                                                
011700                                                                          
011800 01  MESSAGE-CODES.                                                       
011900     03 ERR-WRONG-KEY              PIC X(3)    VALUE '022'.               
012000     03 IS-INVALID                 PIC X(3)    VALUE '023'.               
012100     03 MUST-BE-NUMERIC            PIC X(3)    VALUE '024'.               
012200     03 MUST-BE-ENTERED            PIC X(3)    VALUE '026'.               
012300     EJECT                                                                
012400                                                                          
012500*    --- AREAS FOR COMMUNICATION                                          
012600 01  FILLER                      PIC X(16)  VALUE 'RECV-CONTROL'.         
012700     SKIP3                                                                
012800 01  -COPY WZ01RECV                                                       
012900     EJECT                                                                
013000                                                                          
013100 01  RECV-AREA.                                                           
013200*    03  -COPY WZ01REQU                                                   
013300     EJECT                                                                
013400                                                                          
013500 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
013600 01  -COPY WZ01SEND                                                       
013700     EJECT                                                                
013800                                                                          
013900 01  SEND-AREA.                                                           
014000*    03  -COPY WZ01REQU                                                   
014100 01  SEND-AREA-2.                                                         
014200*    03  -COPY WZ01REQU                                                   
014300     EJECT                                                                
014400 01  HDR-AREA.                                                            
014500*    03  -COPY WZ01REQU -PRE ERROR-                                       
014600*    03  -COPY WZ04HDR                                                    
014700     EJECT                                                                
014800                                                                          
014900 01  DAP-LINE-AREA.                                                       
015000*    03  -COPY WF020201                                                   
015100     EJECT                                                                
015200                                                                          
015300 01  DAP-LINE-AREA-2.                                                     
015400*    03  -COPY WF020202                                                   
015500     EJECT                                                                
015600                                                                          
015700 01  DAP-LINE-AREA-3.                                                     
015800*    03  -COPY WF020203                                                   
015900     EJECT                                                                
016000                                                                          
016100 01  DAP-LINE-AREA-4.                                                     
016200*    03  -COPY WF020204                                                   
016300     EJECT                                                                
016400                                                                          
016500 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
016600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
016700                                                                          
016800 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
016900 01  DB2-WS.                                                              
017000     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
017100        88 CURSOR-OK                        VALUE 000.                    
017200        88 LINES-FOUND                      VALUE 000.                    
017300        88 LINES-MISSING                    VALUE 100.                    
017400        88 DUPLICATE-LINES                  VALUE 811.                    
017500        88 ATKOMST-ERROR                    VALUE 904.                    
017600                                                                          
017700     03  GOOD-SQLCODECODES.                                               
017800        05 GOOD-SQLCODE OCCURS 5                                          
017900           INDEXED BY SQLCODE-IX PIC 9(3).                                
018000                                                                          
018100 01  FILLER                    PIC X(16) VALUE 'WS-AREA         '.        
018200 01  WS-AREA.                                                             
018300     03 WS-IDLEGSEL            PIC X(4)  VALUE SPACE.                     
018400     03 WS-IDBUNDLE            PIC X(15) VALUE SPACE.                     
018500     03 WS-DAREGDAT            PIC X(8)  VALUE SPACE.                     
018600     03 WS-TIREGTID            PIC S9(10) COMP-3 VALUE ZERO.              
018700     03 WS-TIREGTID-WF0203     PIC S9(10) COMP-3 VALUE ZERO.              
018800     03 WS-IDREF               PIC X(15) VALUE SPACE.                     
018900     03 WS-DAREFDAT            PIC X(8)  VALUE SPACE.                     
019000     03 WS-IDREFRAD            PIC S9(5) COMP-3 VALUE ZERO.               
019100     03 WS-ERRORTEXT           PIC X(50) VALUE SPACE.                     
019200     03 WS-IDFELKOD            PIC X(3)  VALUE SPACE.                     
019300     03 WS-FLEXPORT            PIC X(1)  VALUE SPACE.                     
019400     03 WS-IDLANDX3-REC        PIC X(3)  VALUE SPACE.                     
019500     03 WS-IDLANDX3-SEND       PIC X(3)  VALUE SPACE.                     
019600     03 WS-DASTADAT            PIC X(8)  VALUE SPACE.                     
019700     03 WS-KDPARTTY            PIC X(3)  VALUE SPACE.                     
019800     03 WS-KDPARTGR            PIC X(15) VALUE SPACE.                     
019900     03 WS-IDPARTNR            PIC X(9)  VALUE SPACE.                     
020000     03 WS-KDVAT               PIC X(2)  VALUE SPACE.                     
020100     03 WS-KDVALISO            PIC X(3)  VALUE SPACE.                     
020200     03 WS-KDFINDOC            PIC X(4)  VALUE SPACE.                     
020300                                                                          
020400     03 DAP-IDLEGSEL           PIC X(4)  VALUE SPACE.                     
020500     03 DAP-IDBUNDLE           PIC X(15) VALUE SPACE.                     
020600     03 DAP-DAREGDAT           PIC X(8)  VALUE SPACE.                     
020700     03 DAP-IDPARTNR           PIC X(9)  VALUE SPACE.                     
020800     03 DAP-KDFINDOC           PIC X(4)  VALUE SPACE.                     
020900     03 DAP-KDINVFRQ           PIC X(3)  VALUE SPACE.                     
021000     03 DAP-IDARTNR-FINANCE    PIC X(50) VALUE SPACE.                     
021100     03 DAP-TIREGTID           PIC 9(10)  VALUE ZERO.                     
021200     03 DAP-PRARTBTO           PIC Z(6)9.9(2) VALUE ZERO.                 
021300     03 DAP-PRARTNTO           PIC Z(6)9.9(2) VALUE ZERO.                 
021400     03 DAP-IDREF              PIC X(15) VALUE SPACE.                     
021500     03 DAP-IDDC               PIC X(2)  VALUE SPACE.                     
021600     03 DAP-KDVAT              PIC X(2)  VALUE SPACE.                     
021700     03 DAP-KDVALISO           PIC X(3)  VALUE SPACE.                     
021800     03 DAP-IDEXCUST-1         PIC X(15) VALUE SPACE.                     
021900     03 DAP-IDEXCUST-2         PIC X(15) VALUE SPACE.                     
022000     03 DAP-DAREFDAT           PIC X(8)  VALUE SPACE.                     
022100     03 DAP-IDREFRAD           PIC 9(5)  VALUE ZERO.                      
022200     03 DAP-ERROR-COUNT        PIC 9(9)  VALUE ZERO.                      
022300     03 DAP-ERRORTEXT          PIC X(50) VALUE SPACE.                     
022400     03 DAP-IDFELKOD           PIC X(3)  VALUE SPACE.                     
022500                                                                          
022600 01  WS-DIVERSE-MULTIFETCH.                                               
022700     03 WS-MX                    PIC S9(3)  COMP-3.                       
022800     03 WS-UPDATE-MULTI          PIC S9(9)  COMP-3.                       
022900     03 WS-MULTIFETCH            PIC S9(3)  COMP-3.                       
023000                                                                          
023100     03 WS-TRAW-IDREFRAD   OCCURS 100 PIC S9(5) COMP-3.                   
023200     03 WS-KVANT-TRACK-SUM OCCURS 100 PIC S9(5) COMP-3.                   
023300     EJECT                                                                
023400                                                                          
023500 01  FILLER                    PIC X(16) VALUE 'TRAW-AREA       '.        
023600*    -COPY T01TRAWT -PRE TRAW-                                            
023700     EJECT                                                                
023800 01  FILLER                    PIC X(16) VALUE 'T01TBUN-AREA    '.        
023900*    -COPY T01TBUN -PRE T01TBUN-                                          
024000     EJECT                                                                
024100 01  FILLER                    PIC X(16) VALUE 'T01LSEL-AREA    '.        
024200*    -COPY T01LSEL -PRE T01LSEL-                                          
024300     EJECT                                                                
024400 01  FILLER                    PIC X(16) VALUE 'T01SECO-AREA    '.        
024500*    -COPY T01SECO -PRE T01SECO-                                          
024600     EJECT                                                                
024700 01  FILLER                    PIC X(16) VALUE 'T01RECO-AREA    '.        
024800*    -COPY T01RECO -PRE T01RECO-                                          
024900     EJECT                                                                
025000 01  FILLER                    PIC X(16) VALUE 'T01FCUS-AREA    '.        
025100*    -COPY T01FCUS -PRE T01FCUS-                                          
025200     EJECT                                                                
025300 01  FILLER                    PIC X(16) VALUE 'T01INRE-AREA    '.        
025400*    -COPY T01INRE -PRE T01INRE-                                          
025500     EJECT                                                                
025600 01  FILLER                    PIC X(16) VALUE 'T01VAT-AREA     '.        
025700*    -COPY T01VAT -PRE T01VAT-                                            
025800     EJECT                                                                
025900 01  FILLER                    PIC X(16) VALUE 'CURR-AREA       '.        
026000*    -COPY T01CURR -PRE CURR-                                             
026100     EJECT                                                                
026200 01  FILLER                    PIC X(16) VALUE 'T01CURR-AREA    '.        
026300*    -COPY T01CURR -PRE T01CURR-                                          
026400     EJECT                                                                
026500 01  FILLER                    PIC X(16) VALUE 'T01DOTY-AREA    '.        
026600*    -COPY T01DOTY -PRE T01DOTY-                                          
026700     EJECT                                                                
026800 01  FILLER                    PIC X(16) VALUE 'T01CUGR-AREA    '.        
026900*    -COPY T01CUGR -PRE T01CUGR-                                          
027000     EJECT                                                                
027100 01  FILLER                    PIC X(16) VALUE 'T01BURE-AREA    '.        
027200*    -COPY T01BURE -PRE T01BURE-                                          
027300     EJECT                                                                
027400 01  FILLER                    PIC X(16) VALUE 'T01ASNS-AREA    '.        
027500*    -COPY T01ASNS -PRE T01ASNS-                                          
027600     EJECT                                                                
027700 01  FILLER                    PIC X(16) VALUE 'T01NSDO-AREA    '.        
027800*    -COPY T01NSDO -PRE T01NSDO-                                          
027900     EJECT                                                                
028000 01  FILLER                    PIC X(16) VALUE 'SYST-AREA       '.        
028100*    -COPY T01SYST -PRE SYST-                                             
028200     EJECT                                                                
028300 01  FILLER                    PIC X(16) VALUE 'T01SYST-AREA    '.        
028400*    -COPY T01SYST -PRE T01SYST-                                          
028500     EJECT                                                                
028600                                                                          
028700     EXEC SQL INCLUDE T01TRAW END-EXEC.                                   
028800     EJECT                                                                
028900     EXEC SQL INCLUDE T01TBUN END-EXEC.                                   
029000     EJECT                                                                
029100     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
029200     EJECT                                                                
029300     EXEC SQL INCLUDE T01SECO END-EXEC.                                   
029400     EJECT                                                                
029500     EXEC SQL INCLUDE T01RECO END-EXEC.                                   
029600     EJECT                                                                
029700     EXEC SQL INCLUDE T01FCUS END-EXEC.                                   
029800     EJECT                                                                
029900     EXEC SQL INCLUDE T01INRE END-EXEC.                                   
030000     EJECT                                                                
030100     EXEC SQL INCLUDE T01VAT END-EXEC.                                    
030200     EJECT                                                                
030300     EXEC SQL INCLUDE T01CURR END-EXEC.                                   
030400     EJECT                                                                
030500     EXEC SQL INCLUDE T01DOTY END-EXEC.                                   
030600     EJECT                                                                
030700     EXEC SQL INCLUDE T01CUGR END-EXEC.                                   
030800     EJECT                                                                
030900     EXEC SQL INCLUDE T01BURE END-EXEC.                                   
031000     EJECT                                                                
031100     EXEC SQL INCLUDE T01ASNS END-EXEC.                                   
031200     EJECT                                                                
031300     EXEC SQL INCLUDE T01NSDO END-EXEC.                                   
031400     EJECT                                                                
031500     EXEC SQL INCLUDE T01SYST END-EXEC.                                   
031600     EJECT                                                                
031700                                                                          
031800 LINKAGE SECTION.                                                         
031900 PROCEDURE DIVISION.                                                      
032000 MAIN SECTION.                                                            
032100     PERFORM A-INIT                                                       
032200* READS ALL NON-CONTROLLED BUNDLES                                        
032300     PERFORM DB2-OPEN-T01TBUN-CRS                                         
032400     PERFORM DB2-FETCH-T01TBUN-CRS                                        
032500* UNTIL LINES MISSING OR OVER 1 UPDATES OF BUNT                           
032600     PERFORM UNTIL LINES-MISSING OR RESTART-IX > MAX-BUNTAR               
032700       MOVE T01TBUN-IDLEGSEL TO WS-IDLEGSEL                               
032800       MOVE T01TBUN-IDBUNDLE TO WS-IDBUNDLE                               
032900       MOVE T01TBUN-DAREGDAT TO WS-DAREGDAT                               
033000       MOVE T01TBUN-TIREGTID TO WS-TIREGTID                               
033100* READS ALL LINES WITHIN ACTUAL BUNDLE                                    
033200       PERFORM B-LINES-IN-BUNDLE                                          
033300       IF BUNT-ERROR                                                      
033400         MOVE 'J'            TO WS-FLFEL                                  
033500         MOVE NOO            TO BUNDLE-SW                                 
033600         MOVE YES            TO KEYS-SW                                   
033700         MOVE YES            TO BUNT-SW                                   
033800       ELSE                                                               
033900         MOVE 'N'            TO WS-FLFEL                                  
034000       END-IF                                                             
034100       PERFORM DB2-UPDATE-T01TBUN                                         
034200       PERFORM DB2-FETCH-T01TBUN-CRS                                      
034300     END-PERFORM                                                          
034400     PERFORM DB2-CLOSE-T01TBUN-CRS                                        
034500* IF ERRORS SEND A MESSAGE TO DAP ELSE CONTINUE                           
034600     IF BUNDLE-ERROR                                                      
034700       MOVE 'J'            TO WS-FLFEL                                    
034800       PERFORM F-SEND-TO-DAP                                              
034900     END-IF                                                               
035000* CHECKS IF NEW NON-CONTROLLED BUNDLES EXIST                              
035100     PERFORM I-SELECT-T01TBUN-NONCNTRL                                    
035200     IF WS-BUNDLES-NOT-CONTROLLED = YES                                   
035300       MOVE 'R' TO SYST-KDBEHS                                            
035400       PERFORM DB2-UPDATE-T01SYST                                         
035500       PERFORM C-RESTART-OF-OWN-TRANS                                     
035600     ELSE                                                                 
035700* CHECKS IF WF020300 IS STILL RUNNING                                     
035800       PERFORM DB2-SELECT-T01SYST                                         
035900       IF SYST-KDBEHX = SPACE                                             
036000         MOVE 'F' TO SYST-KDBEHX                                          
036100         PERFORM DB2-UPDATE-T01SYST-KDBEHX                                
036200         PERFORM D-START-PGM-WF020300                                     
036300       ELSE                                                               
036400         CONTINUE                                                         
036500       END-IF                                                             
036600       MOVE SPACE TO SYST-KDBEHS                                          
036700       PERFORM DB2-UPDATE-T01SYST                                         
036800     END-IF                                                               
036900                                                                          
037000     MOVE ZERO TO RETURN-CODE                                             
037100     GOBACK                                                               
037200     .                                                                    
037300     EJECT                                                                
037400                                                                          
037500 A-INIT SECTION.                                                          
037600     PERFORM S01-READ-OPEN                                                
037700     PERFORM S02-READ-MESSAGE                                             
037800     IF RECV-KDRC = ZERO                                                  
037900       PERFORM S03-READ-CLOSE                                             
038000                                                                          
038100       MOVE YES                   TO KEYS-SW                              
038200                                     BUNT-SW                              
038300                                     BUNDLE-SW                            
038400       MOVE NOO                   TO WS-FLFEL                             
038500                                                                          
038600       MOVE SPACE                 TO WS-IDLEGSEL                          
038700                                     WS-IDBUNDLE                          
038800                                     WS-DAREGDAT                          
038900                                     WS-IDREF                             
039000                                     WS-DAREFDAT                          
039100                                     WS-ERRORTEXT                         
039200                                     WS-IDFELKOD                          
039300                                     WS-FLEXPORT                          
039400                                     WS-IDLANDX3-REC                      
039500                                     WS-DASTADAT                          
039600                                     WS-KDPARTTY                          
039700                                     WS-KDPARTGR                          
039800                                                                          
039900       MOVE ZERO                  TO WS-TIREGTID                          
040000                                     WS-IDREFRAD                          
040100       MOVE +1                    TO RESTART-IX                           
040200                                                                          
040300       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                       
040400       MOVE FUNCTION CURRENT-DATE (3:2) TO WS-CURRENT-YEAR                
040500       MOVE WS-CURRENT-YEAR             TO WS-CURRENT-YEAR-PACKED         
040600                                                                          
040700       INITIALIZE GOOD-SQLCODECODES                                       
040800     ELSE                                                                 
040900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
041000     END-IF                                                               
041100                                                                          
041200     PERFORM DB2-SELECT-T01SYST                                           
041300     IF SYST-KDBEHS > SPACE                                               
041400* THIS PROCESS WAS RUNNING, RESTART OR START AFTER ABEND                  
041500       CONTINUE                                                           
041600     ELSE                                                                 
041700* BLOCK WF020100 FROM STARTING THIS PROGRAM WHEN RUNNING OR ABEND         
041800       MOVE 'F' TO SYST-KDBEHS                                            
041900       PERFORM DB2-UPDATE-T01SYST                                         
042000     END-IF                                                               
042100     .                                                                    
042200     EJECT                                                                
042300                                                                          
042400 B-LINES-IN-BUNDLE SECTION.                                               
042500     PERFORM DB2-OPEN-T01TRAW-CRS                                         
042600     PERFORM DB2-FETCH-T01TRAW-CRS                                        
042700     IF SQLERRD(3) > 0                                                    
042800       MOVE 000       TO SQLCODE-WS                                       
042900     END-IF                                                               
043000     PERFORM UNTIL LINES-MISSING                                          
043100       MOVE SQLERRD(3) TO WS-MULTIFETCH                                   
043200       MOVE ZERO         TO WS-MX                                         
043300       PERFORM UNTIL WS-MX = WS-MULTIFETCH                                
043400         ADD +1          TO WS-MX                                         
043500         MOVE WS-TRAW-IDREFRAD(WS-MX)   TO WS-IDREFRAD                    
043600         MOVE TRAW-IDPARTNR(WS-MX)      TO WS-IDPARTNR                    
043700         MOVE TRAW-KDVAT(WS-MX)         TO WS-KDVAT                       
043800         MOVE TRAW-KDVALISO(WS-MX)      TO WS-KDVALISO                    
043900         MOVE TRAW-KDFINDOC(WS-MX)      TO WS-KDFINDOC                    
044000         MOVE TRAW-DAREFDAT(WS-MX)      TO WS-DAREFDAT                    
044100         MOVE TRAW-IDREF(WS-MX)         TO WS-IDREF                       
044200         MOVE YES TO KEYS-SW                                              
044300         PERFORM BA-FORMAL-CONTROL                                        
044400         IF KEYS-OK                                                       
044500           PERFORM BB-EXPORT-CONTROL                                      
044600           IF KEYS-OK                                                     
044700             PERFORM BC-CONTINUE-FORMAL-CONTROL                           
044800             IF KEYS-OK                                                   
044900               PERFORM BD-LOGICAL-CONTROL                                 
045000               IF KEYS-OK                                                 
045100                 PERFORM BE-REFERENS-CONTROL                              
045200                 IF KEYS-OK                                               
045300                   CONTINUE                                               
045400                 ELSE                                                     
045500                   PERFORM BF-UPDATE-T01TRAW                              
045600                 END-IF                                                   
045700               ELSE                                                       
045800                 PERFORM BF-UPDATE-T01TRAW                                
045900               END-IF                                                     
046000             ELSE                                                         
046100               PERFORM BF-UPDATE-T01TRAW                                  
046200             END-IF                                                       
046300           ELSE                                                           
046400             PERFORM BF-UPDATE-T01TRAW                                    
046500           END-IF                                                         
046600         ELSE                                                             
046700           PERFORM BF-UPDATE-T01TRAW                                      
046800         END-IF                                                           
046900       END-PERFORM                                                        
047000       IF WS-MULTIFETCH = 100                                             
047100         PERFORM DB2-FETCH-T01TRAW-CRS                                    
047200         IF SQLERRD(3) > 0                                                
047300           MOVE 000       TO SQLCODE-WS                                   
047400         END-IF                                                           
047500       ELSE                                                               
047600         MOVE 100 TO SQLCODE-WS                                           
047700       END-IF                                                             
047800     END-PERFORM                                                          
047900     PERFORM DB2-CLOSE-T01TRAW-CRS                                        
048000     .                                                                    
048100     EJECT                                                                
048200                                                                          
048300 BA-FORMAL-CONTROL SECTION.                                               
048400     IF TRAW-IDLEGSEL(WS-MX) = SPACE                                      
048500       MOVE 'LEGAL SELLER ID MANDATORY (IDLEGSEL)' TO WS-ERRORTEXT        
048600       MOVE MUST-BE-ENTERED                        TO WS-IDFELKOD         
048700       MOVE NOO                                    TO KEYS-SW             
048800     END-IF                                                               
048900     IF KEYS-OK                                                           
049000       IF TRAW-IDBUNDLE(WS-MX) = SPACE                                    
049100         MOVE 'BUNDLE ID MANDATORY (IDBUNDLE)' TO WS-ERRORTEXT            
049200         MOVE MUST-BE-ENTERED                  TO WS-IDFELKOD             
049300         MOVE NOO                              TO KEYS-SW                 
049400       END-IF                                                             
049500     END-IF                                                               
049600     IF KEYS-OK                                                           
049700       IF TRAW-IDREF(WS-MX) = SPACE                                       
049800         MOVE 'REFERENCE ID MANDATORY (IDREF)' TO WS-ERRORTEXT            
049900         MOVE MUST-BE-ENTERED                  TO WS-IDFELKOD             
050000         MOVE NOO                              TO KEYS-SW                 
050100       END-IF                                                             
050200     END-IF                                                               
050300     IF KEYS-OK                                                           
050400       IF TRAW-DAREFDAT(WS-MX) = SPACE                                    
050500        MOVE 'REFERENCE DATE MANDATORY (DAREFDAT)' TO WS-ERRORTEXT        
050600         MOVE MUST-BE-ENTERED                      TO WS-IDFELKOD         
050700         MOVE NOO                                  TO KEYS-SW             
050800       END-IF                                                             
050900     END-IF                                                               
051000     IF KEYS-OK                                                           
051100       IF WS-IDREFRAD = ZERO                                              
051200         MOVE 'REFERENCE LINE NUMBER MANDATORY (IDREFRAD)'                
051300                              TO WS-ERRORTEXT                             
051400         MOVE IS-INVALID      TO WS-IDFELKOD                              
051500         MOVE NOO             TO KEYS-SW                                  
051600       END-IF                                                             
051700     END-IF                                                               
051800     IF KEYS-OK                                                           
051900       IF TRAW-IDLANDX3-SEND(WS-MX) = SPACE                               
052000         MOVE 'SENDING COUNTRY CODE MANDATORY (IDLANDX3-SEND)'            
052100                              TO WS-ERRORTEXT                             
052200         MOVE MUST-BE-ENTERED TO WS-IDFELKOD                              
052300         MOVE NOO             TO KEYS-SW                                  
052400       END-IF                                                             
052500     END-IF                                                               
052600     IF KEYS-OK                                                           
052700       IF TRAW-IDPARTNR(WS-MX) = SPACE                                    
052800         MOVE 'PARMA NO MANDATORY (IDPARTNR)'                             
052900                              TO WS-ERRORTEXT                             
053000         MOVE MUST-BE-ENTERED TO WS-IDFELKOD                              
053100         MOVE NOO             TO KEYS-SW                                  
053200       END-IF                                                             
053300     END-IF                                                               
053400     IF KEYS-OK                                                           
053500       IF TRAW-KDFINDOC(WS-MX) = SPACE                                    
053600         MOVE 'DOCUMENT TYPE MANDATORY (KDFINDOC)' TO WS-ERRORTEXT        
053700         MOVE MUST-BE-ENTERED                      TO WS-IDFELKOD         
053800         MOVE NOO                                  TO KEYS-SW             
053900       END-IF                                                             
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300                                                                          
054400 BB-EXPORT-CONTROL SECTION.                                               
054500     MOVE YES TO KEYS-SW                                                  
054600* CONTROL IF LEGAL SELLAR EXISTS                                          
054700     IF KEYS-OK                                                           
054800       PERFORM BBA-CONTROL-LEGAL-SELLER                                   
054900     END-IF                                                               
055000* CONTROL IF IDPARTNR EXIST FOR FINANCIAL CUSTOMER                        
055100     IF KEYS-OK                                                           
055200       PERFORM BBB-CONTROL-FINANC-CUST                                    
055300     END-IF                                                               
055400* CONTROL IF EXPORT OUTSIDE EU                                            
055500     IF KEYS-OK                                                           
055600       PERFORM BBC-CONTROL-IF-EU                                          
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000                                                                          
056100 BBA-CONTROL-LEGAL-SELLER SECTION.                                        
056200     PERFORM DB2-SELECT-T01LSEL                                           
056300     IF LINES-FOUND                                                       
056400       CONTINUE                                                           
056500     ELSE                                                                 
056600       MOVE 'LEGAL SELLER IS INVALID -2 (IDLEGSEL)'                       
056700                       TO WS-ERRORTEXT                                    
056800       MOVE IS-INVALID TO WS-IDFELKOD                                     
056900       MOVE NOO        TO KEYS-SW                                         
057000     END-IF                                                               
057100     .                                                                    
057200     EJECT                                                                
057300                                                                          
057400 BBB-CONTROL-FINANC-CUST SECTION.                                         
057500     PERFORM DB2-SELECT-T01FCUS                                           
057600     IF LINES-FOUND                                                       
057700       CONTINUE                                                           
057800     ELSE                                                                 
057900       MOVE 'PARMA NO MISSING (IDPARTNR)'                                 
058000                       TO WS-ERRORTEXT                                    
058100       MOVE IS-INVALID TO WS-IDFELKOD                                     
058200       MOVE NOO        TO KEYS-SW                                         
058300     END-IF                                                               
058400     IF KEYS-OK                                                           
058500       PERFORM DB2-SELECT-T01RECO-FCUS                                    
058600       IF LINES-FOUND                                                     
058700         CONTINUE                                                         
058800       ELSE                                                               
058900         MOVE 'RECEIVING COUNTRY MISSING (IDPARTNR COUNTRY)'              
059000                         TO WS-ERRORTEXT                                  
059100         MOVE IS-INVALID TO WS-IDFELKOD                                   
059200         MOVE NOO        TO KEYS-SW                                       
059300       END-IF                                                             
059400     END-IF                                                               
059500     .                                                                    
059600     EJECT                                                                
059700                                                                          
059800 BBC-CONTROL-IF-EU SECTION.                                               
059900     MOVE TRAW-KDANMORS(WS-MX)  TO TRAW-KDANMORS-SW                       
060000* KONTROLLERA INTRATSTAT RELATION VID RETUR AV GODS                       
060100     IF (TRAW-KDFINDOC(WS-MX) = 'CR'                                      
060200     OR  TRAW-KDFINDOC(WS-MX) = 'INT2')                                   
060300     AND TRAW-KDANMORS-VALID                                              
060400     AND TRAW-IDARTNR-FINANCE(WS-MX) > SPACE                              
060500       MOVE TRAW-IDLANDX3-REC(WS-MX)  TO WS-IDLANDX3-REC                  
060600       MOVE TRAW-IDLANDX3-SEND(WS-MX) TO WS-IDLANDX3-SEND                 
060700       PERFORM DB2-SELECT-T01INRE-REC                                     
060800       IF LINES-FOUND                                                     
060900         CONTINUE                                                         
061000       ELSE                                                               
061100         MOVE 'INTERSTATE RELATION MISSING (IDLANDX3) -1'                 
061200                         TO WS-ERRORTEXT                                  
061300         MOVE IS-INVALID TO WS-IDFELKOD                                   
061400         MOVE NOO        TO KEYS-SW                                       
061500       END-IF                                                             
061600     END-IF                                                               
061700                                                                          
061800* KONTROLLERA FINANSIELL RELATION                                         
061900     PERFORM DB2-SELECT-T01FCUS-REC                                       
062000     IF LINES-FOUND                                                       
062100       MOVE TRAW-IDLANDX3-SEND(WS-MX) TO WS-IDLANDX3-SEND                 
062200       PERFORM DB2-SELECT-T01INRE                                         
062300       IF LINES-FOUND                                                     
062400         CONTINUE                                                         
062500       ELSE                                                               
062600         MOVE 'INTERSTATE RELATION MISSING (IDLANDX3) -2'                 
062700                         TO WS-ERRORTEXT                                  
062800         MOVE IS-INVALID TO WS-IDFELKOD                                   
062900         MOVE NOO          TO KEYS-SW                                     
063000       END-IF                                                             
063100     ELSE                                                                 
063200       MOVE 'PARMA NO MISSING (IDPARTNR-2)'                               
063300                       TO WS-ERRORTEXT                                    
063400       MOVE IS-INVALID TO WS-IDFELKOD                                     
063500       MOVE NOO        TO KEYS-SW                                         
063600     END-IF                                                               
063700     .                                                                    
063800     EJECT                                                                
063900                                                                          
064000 BC-CONTINUE-FORMAL-CONTROL SECTION.                                      
064100     IF KEYS-OK                                                           
064200       IF TRAW-BEART(WS-MX) = SPACE                                       
064300         MOVE 'PART DESCRIPTION MANDATORY (BEART)' TO WS-ERRORTEXT        
064400         MOVE MUST-BE-ENTERED                      TO WS-IDFELKOD         
064500         MOVE NOO                                  TO KEYS-SW             
064600       END-IF                                                             
064700     END-IF                                                               
064800     IF KEYS-OK                                                           
064900       IF TRAW-KDVAT(WS-MX) = SPACE                                       
065000         MOVE 'VAT CODE MANDATORY (KDVAT)' TO WS-ERRORTEXT                
065100         MOVE MUST-BE-ENTERED              TO WS-IDFELKOD                 
065200         MOVE NOO                          TO KEYS-SW                     
065300       END-IF                                                             
065400     END-IF                                                               
065500     IF KEYS-OK                                                           
065600       IF  TRAW-FLSOFT(WS-MX) NOT = 'Y'                                   
065700       AND TRAW-FLSOFT(WS-MX) NOT = 'J'                                   
065800       AND TRAW-FLSOFT(WS-MX) NOT = 'N'                                   
065900         MOVE 'SOFTWARE IND INCORRECT (FLSOFT)' TO WS-ERRORTEXT           
066000         MOVE MUST-BE-ENTERED                   TO WS-IDFELKOD            
066100         MOVE NOO                               TO KEYS-SW                
066200       END-IF                                                             
066300     END-IF                                                               
066400     IF KEYS-OK                                                           
066500       IF  TRAW-FLSPECPR(WS-MX) NOT = 'Y'                                 
066600       AND TRAW-FLSPECPR(WS-MX) NOT = 'J'                                 
066700       AND TRAW-FLSPECPR(WS-MX) NOT = 'N'                                 
066800         MOVE 'SPECIAL PRICE IND INCORRECT (FLSPECPR)'                    
066900                              TO WS-ERRORTEXT                             
067000         MOVE MUST-BE-ENTERED TO WS-IDFELKOD                              
067100         MOVE NOO             TO KEYS-SW                                  
067200       END-IF                                                             
067300     END-IF                                                               
067400     IF KEYS-OK                                                           
067500       IF TRAW-PRARTBTO(WS-MX) = ZERO                                     
067600         MOVE 'LIST PRICE MANDATORY (PRARTBTO)'                           
067700                              TO WS-ERRORTEXT                             
067800         MOVE IS-INVALID      TO WS-IDFELKOD                              
067900         MOVE NOO             TO KEYS-SW                                  
068000       END-IF                                                             
068100     END-IF                                                               
068200     IF KEYS-OK                                                           
068300       IF TRAW-PRARTNTO(WS-MX) = ZERO                                     
068400         MOVE 'NET PRICE MANDATORY (PRARTNTO)' TO WS-ERRORTEXT            
068500         MOVE IS-INVALID                       TO WS-IDFELKOD             
068600         MOVE NOO                              TO KEYS-SW                 
068700       END-IF                                                             
068800     END-IF                                                               
068900     IF KEYS-OK                                                           
069000       IF TRAW-PRARTBTO(WS-MX) < TRAW-PRARTNTO(WS-MX)                     
069100         MOVE 'NET PRICE GT LIST PRICE       ' TO WS-ERRORTEXT            
069200         MOVE IS-INVALID                     TO WS-IDFELKOD               
069300         MOVE NOO                            TO KEYS-SW                   
069400       END-IF                                                             
069500     END-IF                                                               
069600     IF KEYS-OK                                                           
069700       IF TRAW-KDVALISO(WS-MX) = SPACE                                    
069800         MOVE 'CURRENCY CODE MANDATORY (KDVALISO)' TO WS-ERRORTEXT        
069900         MOVE MUST-BE-ENTERED                      TO WS-IDFELKOD         
070000         MOVE NOO                                  TO KEYS-SW             
070100       END-IF                                                             
070200     END-IF                                                               
070300     IF KEYS-OK                                                           
070400       IF TRAW-KDINVFRQ(WS-MX) = SPACE                                    
070500       OR TRAW-KDINVFRQ(WS-MX) = 'NOW'                                    
070600       OR TRAW-KDINVFRQ(WS-MX) = 'DAY'                                    
070700       OR TRAW-KDINVFRQ(WS-MX) = 'WEEK'                                   
070800       OR TRAW-KDINVFRQ(WS-MX) = 'PER'                                    
070900         CONTINUE                                                         
071000       ELSE                                                               
071100         MOVE 'INVOICING FREQUENCE INCORRECT (KDINVFRQ)'                  
071200                              TO WS-ERRORTEXT                             
071300         MOVE MUST-BE-ENTERED TO WS-IDFELKOD                              
071400         MOVE NOO             TO KEYS-SW                                  
071500       END-IF                                                             
071600     END-IF                                                               
071700     IF KEYS-OK                                                           
071800       IF  TRAW-FLFREE(WS-MX) NOT = 'Y'                                   
071900       AND TRAW-FLFREE(WS-MX) NOT = 'J'                                   
072000       AND TRAW-FLFREE(WS-MX) NOT = 'N'                                   
072100         MOVE 'FREE OF CHARGE IND INCORRECT (FLFREE)'                     
072200                              TO WS-ERRORTEXT                             
072300         MOVE MUST-BE-ENTERED TO WS-IDFELKOD                              
072400         MOVE NOO             TO KEYS-SW                                  
072500       END-IF                                                             
072600     END-IF                                                               
072700     IF KEYS-OK                                                           
072800       IF TRAW-IDSYSTEM-SEND(WS-MX) = SPACE                               
072900         MOVE 'SENDING SYSTEM MANDATORY (IDSYSTEM-SEND)'                  
073000                              TO WS-ERRORTEXT                             
073100         MOVE MUST-BE-ENTERED TO WS-IDFELKOD                              
073200         MOVE NOO             TO KEYS-SW                                  
073300       END-IF                                                             
073400     END-IF                                                               
073500     IF KEYS-OK                                                           
073600       IF  TRAW-FLPRIV(WS-MX) NOT = 'Y'                                   
073700       AND TRAW-FLPRIV(WS-MX) NOT = 'J'                                   
073800       AND TRAW-FLPRIV(WS-MX) NOT = 'N'                                   
073900         MOVE 'PRIVATE IND INCORRECT (FLPRIV)' TO WS-ERRORTEXT            
074000         MOVE MUST-BE-ENTERED                   TO WS-IDFELKOD            
074100         MOVE NOO                               TO KEYS-SW                
074200       END-IF                                                             
074300     END-IF                                                               
074400     .                                                                    
074500     EJECT                                                                
074600                                                                          
074700 BD-LOGICAL-CONTROL SECTION.                                              
074800     IF TRAW-IDLEGSEL(WS-MX) = WS-IDLEGSEL                                
074900       CONTINUE                                                           
075000     ELSE                                                                 
075100       MOVE 'LEGAL SELLER IS INVALID (IDLEGSEL)' TO WS-ERRORTEXT          
075200       MOVE IS-INVALID                           TO WS-IDFELKOD           
075300       MOVE NOO                                  TO KEYS-SW               
075400     END-IF                                                               
075500     IF KEYS-OK                                                           
075600       IF TRAW-DAREGDAT(WS-MX) = WS-DAREGDAT                              
075700         CONTINUE                                                         
075800       ELSE                                                               
075900         MOVE 'DAREGDAT IS INVALID (DAREGDAT)' TO WS-ERRORTEXT            
076000         MOVE IS-INVALID                       TO WS-IDFELKOD             
076100         MOVE NOO                              TO KEYS-SW                 
076200       END-IF                                                             
076300     END-IF                                                               
076400     IF KEYS-OK                                                           
076500       IF TRAW-TIREGTID(WS-MX) = WS-TIREGTID                              
076600         CONTINUE                                                         
076700       ELSE                                                               
076800         MOVE 'TIREGTID IS INVALID (TIREGTID)' TO WS-ERRORTEXT            
076900         MOVE IS-INVALID                       TO WS-IDFELKOD             
077000         MOVE NOO                              TO KEYS-SW                 
077100       END-IF                                                             
077200     END-IF                                                               
077300     IF KEYS-OK                                                           
077400       IF WS-DATUM < TRAW-DAREFDAT(WS-MX)                                 
077500         MOVE 'REFERENCE DATE HAS NOT YET OCCURRED (DAREFDAT)'            
077600                         TO WS-ERRORTEXT                                  
077700         MOVE IS-INVALID TO WS-IDFELKOD                                   
077800         MOVE NOO        TO KEYS-SW                                       
077900       END-IF                                                             
078000     END-IF                                                               
078100     IF KEYS-OK                                                           
078200       PERFORM DB2-SELECT-T01LSEL                                         
078300       IF LINES-FOUND                                                     
078400         IF TRAW-IDLEGSEL(WS-MX) = WS-IDLEGSEL                            
078500           PERFORM BDA-LOGICAL-CONTROL-VCC                                
078600         END-IF                                                           
078700       ELSE                                                               
078800         CONTINUE                                                         
078900       END-IF                                                             
079000     END-IF                                                               
079100     .                                                                    
079200     EJECT                                                                
079300                                                                          
079400 BDA-LOGICAL-CONTROL-VCC  SECTION.                                        
079500     IF TRAW-KDFINDOC(WS-MX) = 'CR'                                       
079600     OR TRAW-KDFINDOC(WS-MX) = 'INT2'                                     
079700     OR (TRAW-KDFINDOC(WS-MX) = 'INT'                                     
079800     AND TRAW-IDLEGSEL(WS-MX) = WS-IDLEGSEL                               
079900     AND TRAW-IDLEGSEL(WS-MX) NOT = 'VCCS'                                
080000     AND TRAW-KDANMORS(WS-MX) > ' ')                                      
080100       IF TRAW-KDINVFRQ(WS-MX) = 'NOW'                                    
080200         CONTINUE                                                         
080300       ELSE                                                               
080400         MOVE 'FREQUENCE MUST BE NOW (KDINVFRQ)'                          
080500                                  TO WS-ERRORTEXT                         
080600         MOVE IS-INVALID          TO WS-IDFELKOD                          
080700         MOVE NOO                 TO KEYS-SW                              
080800       END-IF                                                             
080900                                                                          
081000       IF KEYS-OK                                                         
081100         IF  TRAW-KDANMORS(WS-MX) = SPACE                                 
081200         AND TRAW-IDARTNR-FINANCE(WS-MX) > SPACE                          
081300           MOVE 'DISCR. CODE MANDATORY (KDANMORS)'                        
081400                                  TO WS-ERRORTEXT                         
081500           MOVE MUST-BE-ENTERED   TO WS-IDFELKOD                          
081600           MOVE NOO               TO KEYS-SW                              
081700         END-IF                                                           
081800       END-IF                                                             
081900                                                                          
082000       IF KEYS-OK                                                         
082100         MOVE TRAW-KDANMORS(WS-MX) TO TRAW-KDANMORS-SW                    
082200         IF  TRAW-KDANMORS-VALID                                          
082300         AND TRAW-IDARTNR-FINANCE(WS-MX) > SPACE                          
082400           IF TRAW-IDLANDX3-REC(WS-MX) = SPACE                            
082500             MOVE 'RECEIVING COUNTRY MANDATORY (IDLANDX3-REC)'            
082600                                  TO WS-ERRORTEXT                         
082700             MOVE MUST-BE-ENTERED TO WS-IDFELKOD                          
082800             MOVE NOO             TO KEYS-SW                              
082900           ELSE                                                           
083000             CONTINUE                                                     
083100           END-IF                                                         
083200         ELSE                                                             
083300           CONTINUE                                                       
083400         END-IF                                                           
083500       END-IF                                                             
083600     ELSE                                                                 
083700                                                                          
083800       IF KEYS-OK                                                         
083900         IF TRAW-KDFINDOC(WS-MX) = 'REF'                                  
084000         OR (TRAW-IDSYSTEM-SEND(WS-MX) = 'W418'                           
084100         AND TRAW-KDFINDOC(WS-MX) = 'CLA')                                
084200           CONTINUE                                                       
084300         ELSE                                                             
084400           IF TRAW-KDANMORS(WS-MX) > SPACE                                
084500             MOVE 'DISCR. CODE NOT ALLOWED (KDANMORS)'                    
084600                                    TO WS-ERRORTEXT                       
084700             MOVE IS-INVALID        TO WS-IDFELKOD                        
084800             MOVE NOO               TO KEYS-SW                            
084900           END-IF                                                         
085000         END-IF                                                           
085100       END-IF                                                             
085200                                                                          
085300       IF KEYS-OK                                                         
085400         IF TRAW-DAFAKREF(WS-MX) NOT = ZERO                               
085500           MOVE 'INVOICE DATE NOT ALLOWED (DAFAKREF)'                     
085600                                  TO WS-ERRORTEXT                         
085700           MOVE IS-INVALID        TO WS-IDFELKOD                          
085800           MOVE NOO               TO KEYS-SW                              
085900         END-IF                                                           
086000       END-IF                                                             
086100                                                                          
086200       IF TRAW-IDLANDX3-REC(WS-MX) NOT = SPACE                            
086300         MOVE 'RECEIVING COUNTRY NOT ALLOWED (IDLANDX3-REC)'              
086400                                  TO WS-ERRORTEXT                         
086500         MOVE IS-INVALID          TO WS-IDFELKOD                          
086600         MOVE NOO                 TO KEYS-SW                              
086700       END-IF                                                             
086800                                                                          
086900       IF KEYS-OK                                                         
087000         IF TRAW-IDARTNR-FINANCE(WS-MX) = SPACE                           
087100         OR TRAW-FLSOFT(WS-MX)          = 'Y'                             
087200         OR TRAW-FLSOFT(WS-MX)          = 'J'                             
087300         OR TRAW-IDSYSTEM-SEND(WS-MX)   = 'VIPS'                          
087400           CONTINUE                                                       
087500         ELSE                                                             
087600           IF TRAW-IDSTATNR(WS-MX) = ZERO                                 
087700             MOVE 'STATISTICAL PART CODE MANDATORY (IDSTATNR)'            
087800                                  TO WS-ERRORTEXT                         
087900             MOVE IS-INVALID      TO WS-IDFELKOD                          
088000             MOVE NOO             TO KEYS-SW                              
088100           END-IF                                                         
088200         END-IF                                                           
088300       END-IF                                                             
088400     END-IF                                                               
088500                                                                          
088600     IF KEYS-OK                                                           
088700       IF TRAW-IDSYSTEM-SEND(WS-MX) = 'W476'                              
088800         IF TRAW-KVLEVART(WS-MX) > TRAW-KVBEART(WS-MX)                    
088900           MOVE 'DEL QTY > ORDER QTY (KVLEVART)'                          
089000                                TO WS-ERRORTEXT                           
089100           MOVE IS-INVALID    TO WS-IDFELKOD                              
089200           MOVE NOO           TO KEYS-SW                                  
089300         END-IF                                                           
089400         COMPUTE WS-KVANT-TRACK-SUM(WS-MX) =                              
089500                TRAW-KVANT-TRACK-1(WS-MX)                                 
089600              + TRAW-KVANT-TRACK-2(WS-MX)                                 
089700              + TRAW-KVANT-TRACK-3(WS-MX)                                 
089800              + TRAW-KVANT-TRACK-4(WS-MX)                                 
089900              + TRAW-KVANT-TRACK-5(WS-MX)                                 
090000         IF WS-KVANT-TRACK-SUM(WS-MX) > TRAW-KVLEVART(WS-MX)              
090100           MOVE 'TRACK QTY > DELIVERED QTY (KVLEVART)'                    
090200                              TO WS-ERRORTEXT                             
090300           MOVE IS-INVALID    TO WS-IDFELKOD                              
090400           MOVE NOO           TO KEYS-SW                                  
090500         END-IF                                                           
090600       END-IF                                                             
090700       IF TRAW-IDSYSTEM-SEND(WS-MX) NOT = 'VSS'                           
090800         IF TRAW-KVLEVART(WS-MX) = ZERO                                   
090900           MOVE 'DELIVERED QUANTITY MANDATORY (KVLEVART)'                 
091000                                TO WS-ERRORTEXT                           
091100           MOVE IS-INVALID      TO WS-IDFELKOD                            
091200           MOVE NOO             TO KEYS-SW                                
091300         END-IF                                                           
091400       ELSE                                                               
091500         IF TRAW-KVBEART(WS-MX) = ZERO                                    
091600           MOVE 'ORDERED QUANTITY MANDATORY (KVBEART)'                    
091700                                TO WS-ERRORTEXT                           
091800           MOVE IS-INVALID      TO WS-IDFELKOD                            
091900           MOVE NOO             TO KEYS-SW                                
092000         END-IF                                                           
092100       END-IF                                                             
092200     END-IF                                                               
092300     .                                                                    
092400     EJECT                                                                
092500                                                                          
092600 BE-REFERENS-CONTROL SECTION.                                             
092700     PERFORM BEB-CONTROL-SENDING-COUNTRY                                  
092800     IF KEYS-OK                                                           
092900       PERFORM BEC-CONTROL-RECEIVING-COUNTRY                              
093000       IF KEYS-OK                                                         
093100         PERFORM BEE-CONTROL-VAT                                          
093200         IF KEYS-OK                                                       
093300           PERFORM BEF-CONTROL-CURRENCY                                   
093400           IF KEYS-OK                                                     
093500             PERFORM BEG-DOCUMENT-TYPE                                    
093600             IF KEYS-OK                                                   
093700               PERFORM BEK-CONTROL-DOCNRSERIE                             
093800               IF KEYS-OK                                                 
093900                 PERFORM BEL-CONTROL-APPROV-CUST                          
094000                 IF KEYS-OK                                               
094100                   PERFORM BEM-CONTROL-BUSIN-RELATI                       
094200                   IF KEYS-OK                                             
094300                     CONTINUE                                             
094400                   END-IF                                                 
094500                 END-IF                                                   
094600               END-IF                                                     
094700             END-IF                                                       
094800           END-IF                                                         
094900         END-IF                                                           
095000       END-IF                                                             
095100     END-IF                                                               
095200     .                                                                    
095300     EJECT                                                                
095400                                                                          
095500 BEB-CONTROL-SENDING-COUNTRY SECTION.                                     
095600     PERFORM DB2-SELECT-T01SECO                                           
095700     IF LINES-FOUND                                                       
095800       CONTINUE                                                           
095900     ELSE                                                                 
096000       MOVE 'SENDING COUNTRY MISSING (IDLANDX3-SEND)'                     
096100                       TO WS-ERRORTEXT                                    
096200       MOVE IS-INVALID TO WS-IDFELKOD                                     
096300       MOVE NOO        TO KEYS-SW                                         
096400     END-IF                                                               
096500     .                                                                    
096600     EJECT                                                                
096700                                                                          
096800 BEC-CONTROL-RECEIVING-COUNTRY SECTION.                                   
096900     IF TRAW-KDFINDOC(WS-MX) = 'CR'                                       
097000     OR (TRAW-KDFINDOC(WS-MX) = 'INT'                                     
097100     AND TRAW-IDLEGSEL(WS-MX) = WS-IDLEGSEL                               
097200     AND TRAW-IDLEGSEL(WS-MX) NOT = 'VCCS'                                
097300     AND TRAW-KDANMORS(WS-MX) > ' ')                                      
097400       CONTINUE                                                           
097500     ELSE                                                                 
097600       IF TRAW-IDLANDX3-REC(WS-MX) = SPACE                                
097700         CONTINUE                                                         
097800       ELSE                                                               
097900         PERFORM DB2-SELECT-T01RECO                                       
098000         IF LINES-FOUND                                                   
098100           CONTINUE                                                       
098200         ELSE                                                             
098300           MOVE 'RECEIVING COUNTRY MISSING (IDLANDX3-REC)'                
098400                           TO WS-ERRORTEXT                                
098500           MOVE IS-INVALID TO WS-IDFELKOD                                 
098600           MOVE NOO        TO KEYS-SW                                     
098700         END-IF                                                           
098800       END-IF                                                             
098900     END-IF                                                               
099000     .                                                                    
099100     EJECT                                                                
099200                                                                          
099300 BEE-CONTROL-VAT SECTION.                                                 
099400*** WHEN A PRIVATE CUSTOMER                                               
099500*** THE VAT USED IS FROM THE SENDING COUNTRY                              
099600     IF TRAW-FLPRIV(WS-MX) = 'J'                                          
099700       PERFORM DB2-SELECT-T01VAT-SEND                                     
099800       IF (LINES-FOUND)                                                   
099900         CONTINUE                                                         
100000       ELSE                                                               
100100*** WHEN VAT FREE THE VAT CODE IS MISSING IN THE TABLE                    
100200*** THE CODES USED VARY WITH THE SENDING COUNTRY                          
100300         IF ((WS-IDLANDX3-SEND = 'SE'                                     
100400         OR   WS-IDLANDX3-SEND = 'AT'                                     
100500         OR   WS-IDLANDX3-SEND = 'JP'                                     
100600         OR   WS-IDLANDX3-SEND = 'CH'                                     
100700         OR   WS-IDLANDX3-SEND = 'AU')                                    
100800         AND (TRAW-KDVAT(WS-MX) = '70'                                    
100900         OR   TRAW-KDVAT(WS-MX) = '90'                                    
101000         OR   TRAW-KDVAT(WS-MX) = '60'                                    
101100         OR   TRAW-KDVAT(WS-MX) = '80'                                    
101200         OR   TRAW-KDVAT(WS-MX) = 'RU'))                                  
101300         OR ((WS-IDLANDX3-SEND = 'GB')                                    
101400         AND (TRAW-KDVAT(WS-MX) = 'G9'                                    
101500         OR   TRAW-KDVAT(WS-MX) = 'G7'                                    
101600         OR   TRAW-KDVAT(WS-MX) = '70'                                    
101700         OR   TRAW-KDVAT(WS-MX) = '90'                                    
101800         OR   TRAW-KDVAT(WS-MX) = '60'                                    
101900         OR   TRAW-KDVAT(WS-MX) = '80'))                                  
102000         OR ((WS-IDLANDX3-SEND = 'NL')                                    
102100         AND (TRAW-KDVAT(WS-MX) = 'NI'                                    
102200         OR   TRAW-KDVAT(WS-MX) = 'NJ'                                    
102300         OR   TRAW-KDVAT(WS-MX) = 'ND'                                    
102400         OR   TRAW-KDVAT(WS-MX) = 'NC'                                    
102500         OR   TRAW-KDVAT(WS-MX) = 'NZ'                                    
102600         OR   TRAW-KDVAT(WS-MX) = '70'                                    
102700         OR   TRAW-KDVAT(WS-MX) = '90'                                    
102800         OR   TRAW-KDVAT(WS-MX) = '60'                                    
102900         OR   TRAW-KDVAT(WS-MX) = '80'))                                  
103000         OR ((WS-IDLANDX3-SEND = 'DE')                                    
103100         AND (TRAW-KDVAT(WS-MX) = 'VI'                                    
103200         OR   TRAW-KDVAT(WS-MX) = 'VJ'                                    
103300         OR   TRAW-KDVAT(WS-MX) = '60'                                    
103400         OR   TRAW-KDVAT(WS-MX) = '70'                                    
103500         OR   TRAW-KDVAT(WS-MX) = '90'                                    
103600         OR   TRAW-KDVAT(WS-MX) = '80'))                                  
103700         OR ((WS-IDLANDX3-SEND = 'IT')                                    
103800         AND (TRAW-KDVAT(WS-MX) = 'ID'                                    
103900         OR   TRAW-KDVAT(WS-MX) = 'IC'                                    
104000         OR   TRAW-KDVAT(WS-MX) = '60'                                    
104100         OR   TRAW-KDVAT(WS-MX) = '70'                                    
104200         OR   TRAW-KDVAT(WS-MX) = '90'                                    
104300         OR   TRAW-KDVAT(WS-MX) = '80'))                                  
104400         OR ((WS-IDLANDX3-SEND = 'FR')                                    
104500         AND (TRAW-KDVAT(WS-MX) = 'F3'                                    
104600         OR   TRAW-KDVAT(WS-MX) = 'F4'                                    
104700         OR   TRAW-KDVAT(WS-MX) = 'F5'                                    
104800         OR   TRAW-KDVAT(WS-MX) = '60'                                    
104900         OR   TRAW-KDVAT(WS-MX) = '70'                                    
105000         OR   TRAW-KDVAT(WS-MX) = '90'                                    
105100         OR   TRAW-KDVAT(WS-MX) = '80'))                                  
105200         OR ((WS-IDLANDX3-SEND = 'ES')                                    
105300         AND (TRAW-KDVAT(WS-MX) = 'S4'                                    
105400         OR   TRAW-KDVAT(WS-MX) = '60'                                    
105500         OR   TRAW-KDVAT(WS-MX) = '70'                                    
105600         OR   TRAW-KDVAT(WS-MX) = '90'                                    
105700         OR   TRAW-KDVAT(WS-MX) = '80'))                                  
105800         OR ((WS-IDLANDX3-SEND = 'FI')                                    
105900         AND (TRAW-KDVAT(WS-MX) = '48'                                    
106000         OR   TRAW-KDVAT(WS-MX) = '60'                                    
106100         OR   TRAW-KDVAT(WS-MX) = '70'                                    
106200         OR   TRAW-KDVAT(WS-MX) = '90'                                    
106300         OR   TRAW-KDVAT(WS-MX) = '80'))                                  
106400         OR ((WS-IDLANDX3-SEND = 'BE')                                    
106500         AND (TRAW-KDVAT(WS-MX) = 'B7'                                    
106600         OR   TRAW-KDVAT(WS-MX) = 'BD'                                    
106700         OR   TRAW-KDVAT(WS-MX) = 'BQ'                                    
106800         OR   TRAW-KDVAT(WS-MX) = 'BX'                                    
106900         OR   TRAW-KDVAT(WS-MX) = '60'                                    
107000         OR   TRAW-KDVAT(WS-MX) = '70'                                    
107100         OR   TRAW-KDVAT(WS-MX) = '90'                                    
107200         OR   TRAW-KDVAT(WS-MX) = '80'))                                  
107300         OR ((WS-IDLANDX3-SEND = 'KR')                                    
107400         AND (TRAW-KDVAT(WS-MX) = 'K2'                                    
107500         OR   TRAW-KDVAT(WS-MX) = '60'                                    
107600         OR   TRAW-KDVAT(WS-MX) = 'XX'                                    
107700         OR   TRAW-KDVAT(WS-MX) = '20'                                    
107800         OR   TRAW-KDVAT(WS-MX) = '70'                                    
107900         OR   TRAW-KDVAT(WS-MX) = '90'                                    
108000         OR   TRAW-KDVAT(WS-MX) = '80'))                                  
108100*        OR ((WS-IDLANDX3-SEND = 'HU')                                    
108200*        AND (TRAW-KDVAT(WS-MX) = '??'                                    
108300*        OR   TRAW-KDVAT(WS-MX) = '??'                                    
108400*        OR   TRAW-KDVAT(WS-MX) = '??'                                    
108500*        OR   TRAW-KDVAT(WS-MX) = '??'                                    
108600*        OR   TRAW-KDVAT(WS-MX) = '??'                                    
108700*        OR   TRAW-KDVAT(WS-MX) = '??'))                                  
108800         OR ((WS-IDLANDX3-SEND = 'PL')                                    
108900         AND (TRAW-KDVAT(WS-MX) = 'P2'                                    
109000         OR   TRAW-KDVAT(WS-MX) = 'PC'                                    
109100         OR   TRAW-KDVAT(WS-MX) = 'PD'                                    
109200         OR   TRAW-KDVAT(WS-MX) = '60'                                    
109300         OR   TRAW-KDVAT(WS-MX) = '70'                                    
109400         OR   TRAW-KDVAT(WS-MX) = '80'                                    
109500         OR   TRAW-KDVAT(WS-MX) = '90'))                                  
109600         OR ((WS-IDLANDX3-SEND = 'NO')                                    
109700         AND (TRAW-KDVAT(WS-MX) = 'Y1'                                    
109800         OR   TRAW-KDVAT(WS-MX) = '60'                                    
109900         OR   TRAW-KDVAT(WS-MX) = '70'                                    
110000         OR   TRAW-KDVAT(WS-MX) = '90'                                    
110100         OR   TRAW-KDVAT(WS-MX) = '80'))                                  
110200           CONTINUE                                                       
110300         ELSE                                                             
110400           MOVE 'VAT CODE MISSING (KDVAT-PRIV)' TO WS-ERRORTEXT           
110500           MOVE IS-INVALID                      TO WS-IDFELKOD            
110600           MOVE NOO                             TO KEYS-SW                
110700         END-IF                                                           
110800       END-IF                                                             
110900     ELSE                                                                 
111000*** WHEN ANOTHER CUSTOMER THE VAT USED IS DEPENDING ON                    
111100*** WHERE THE CUSTOMER IS SITUATED                                        
111200       PERFORM DB2-SELECT-T01VAT-REC                                      
111300       IF (LINES-FOUND)                                                   
111400         CONTINUE                                                         
111500       ELSE                                                               
111600*** WHEN VAT FREE THE VAT CODE IS MISSING IN THE TABLE                    
111700*** THE CODES USED VARY WITH THE SENDING COUNTRY                          
111800*** THIS APPLIES TO VCCS                                                  
111900         IF TRAW-IDLEGSEL(WS-MX) = WS-IDLEGSEL                            
112000           IF ((WS-IDLANDX3-SEND = 'SE'                                   
112100           OR WS-IDLANDX3-SEND = 'AT'                                     
112200           OR WS-IDLANDX3-SEND = 'JP'                                     
112300           OR WS-IDLANDX3-SEND = 'AU'                                     
112400           OR WS-IDLANDX3-SEND = 'CH')                                    
112500           AND (TRAW-KDVAT(WS-MX) = '70'                                  
112600           OR TRAW-KDVAT(WS-MX) = '90'                                    
112700           OR TRAW-KDVAT(WS-MX) = '60'                                    
112800           OR TRAW-KDVAT(WS-MX) = '80'                                    
112900           OR TRAW-KDVAT(WS-MX) = 'RU'))                                  
113000           OR ((WS-IDLANDX3-SEND = 'GB')                                  
113100           AND (TRAW-KDVAT(WS-MX) = 'G9'                                  
113200           OR TRAW-KDVAT(WS-MX) = 'G7'                                    
113300           OR TRAW-KDVAT(WS-MX) = '70'                                    
113400           OR TRAW-KDVAT(WS-MX) = '90'                                    
113500           OR TRAW-KDVAT(WS-MX) = '60'                                    
113600           OR TRAW-KDVAT(WS-MX) = '80'))                                  
113700           OR ((WS-IDLANDX3-SEND = 'NL')                                  
113800           AND (TRAW-KDVAT(WS-MX) = 'NI'                                  
113900           OR TRAW-KDVAT(WS-MX) = 'NJ'                                    
114000           OR TRAW-KDVAT(WS-MX) = 'ND'                                    
114100           OR TRAW-KDVAT(WS-MX) = 'NC'                                    
114200           OR TRAW-KDVAT(WS-MX) = 'NZ'                                    
114300           OR TRAW-KDVAT(WS-MX) = '70'                                    
114400           OR TRAW-KDVAT(WS-MX) = '90'                                    
114500           OR TRAW-KDVAT(WS-MX) = '60'                                    
114600           OR TRAW-KDVAT(WS-MX) = '80'))                                  
114700           OR ((WS-IDLANDX3-SEND = 'DE')                                  
114800           AND (TRAW-KDVAT(WS-MX) = 'VI'                                  
114900           OR TRAW-KDVAT(WS-MX) = 'VJ'                                    
115000           OR TRAW-KDVAT(WS-MX) = '60'                                    
115100           OR TRAW-KDVAT(WS-MX) = '70'                                    
115200           OR TRAW-KDVAT(WS-MX) = '90'                                    
115300           OR TRAW-KDVAT(WS-MX) = '80'))                                  
115400           OR ((WS-IDLANDX3-SEND = 'IT')                                  
115500           AND (TRAW-KDVAT(WS-MX) = 'ID'                                  
115600           OR TRAW-KDVAT(WS-MX) = 'IC'                                    
115700           OR TRAW-KDVAT(WS-MX) = '60'                                    
115800           OR TRAW-KDVAT(WS-MX) = '70'                                    
115900           OR TRAW-KDVAT(WS-MX) = '90'                                    
116000           OR TRAW-KDVAT(WS-MX) = '80'))                                  
116100           OR ((WS-IDLANDX3-SEND = 'FR')                                  
116200           AND (TRAW-KDVAT(WS-MX) = 'F3'                                  
116300           OR TRAW-KDVAT(WS-MX) = 'F4'                                    
116400           OR TRAW-KDVAT(WS-MX) = 'F5'                                    
116500           OR TRAW-KDVAT(WS-MX) = '60'                                    
116600           OR TRAW-KDVAT(WS-MX) = '70'                                    
116700           OR TRAW-KDVAT(WS-MX) = '90'                                    
116800           OR TRAW-KDVAT(WS-MX) = '80'))                                  
116900           OR ((WS-IDLANDX3-SEND = 'ES')                                  
117000           AND (TRAW-KDVAT(WS-MX) = 'S4'                                  
117100           OR TRAW-KDVAT(WS-MX) = '60'                                    
117200           OR TRAW-KDVAT(WS-MX) = '70'                                    
117300           OR TRAW-KDVAT(WS-MX) = '90'                                    
117400           OR TRAW-KDVAT(WS-MX) = '80'))                                  
117500           OR ((WS-IDLANDX3-SEND = 'FI')                                  
117600           AND (TRAW-KDVAT(WS-MX) = '48'                                  
117700           OR TRAW-KDVAT(WS-MX) = '60'                                    
117800           OR TRAW-KDVAT(WS-MX) = '70'                                    
117900           OR TRAW-KDVAT(WS-MX) = '90'                                    
118000           OR TRAW-KDVAT(WS-MX) = '80'))                                  
118100           OR ((WS-IDLANDX3-SEND = 'BE')                                  
118200           AND (TRAW-KDVAT(WS-MX) = 'B7'                                  
118300           OR TRAW-KDVAT(WS-MX) = 'BD'                                    
118400           OR TRAW-KDVAT(WS-MX) = 'BQ'                                    
118500           OR TRAW-KDVAT(WS-MX) = 'BX'                                    
118600           OR TRAW-KDVAT(WS-MX) = '60'                                    
118700           OR TRAW-KDVAT(WS-MX) = '70'                                    
118800           OR TRAW-KDVAT(WS-MX) = '90'                                    
118900           OR TRAW-KDVAT(WS-MX) = '80'))                                  
119000           OR ((WS-IDLANDX3-SEND = 'KR')                                  
119100           AND (TRAW-KDVAT(WS-MX) = 'K2'                                  
119200           OR TRAW-KDVAT(WS-MX) = '60'                                    
119300           OR TRAW-KDVAT(WS-MX) = '70'                                    
119400           OR TRAW-KDVAT(WS-MX) = 'XX'                                    
119500           OR TRAW-KDVAT(WS-MX) = '20'                                    
119600           OR TRAW-KDVAT(WS-MX) = '90'                                    
119700           OR TRAW-KDVAT(WS-MX) = '80'))                                  
119800*          OR ((WS-IDLANDX3-SEND = 'HU')                                  
119900*          AND (TRAW-KDVAT(WS-MX) = '??'                                  
120000*          OR TRAW-KDVAT(WS-MX) = '??'                                    
120100*          OR TRAW-KDVAT(WS-MX) = '??'                                    
120200*          OR TRAW-KDVAT(WS-MX) = '??'                                    
120300*          OR TRAW-KDVAT(WS-MX) = '??'                                    
120400*          OR TRAW-KDVAT(WS-MX) = '??'))                                  
120500           OR ((WS-IDLANDX3-SEND = 'PL')                                  
120600           AND (TRAW-KDVAT(WS-MX) = 'P2'                                  
120700           OR TRAW-KDVAT(WS-MX) = 'PC'                                    
120800           OR TRAW-KDVAT(WS-MX) = 'PD'                                    
120900           OR TRAW-KDVAT(WS-MX) = '60'                                    
121000           OR TRAW-KDVAT(WS-MX) = '70'                                    
121100           OR TRAW-KDVAT(WS-MX) = '80'                                    
121200           OR TRAW-KDVAT(WS-MX) = '90'))                                  
121300           OR ((WS-IDLANDX3-SEND = 'NO')                                  
121400           AND (TRAW-KDVAT(WS-MX) = 'Y1'                                  
121500           OR TRAW-KDVAT(WS-MX) = '60'                                    
121600           OR TRAW-KDVAT(WS-MX) = '70'                                    
121700           OR TRAW-KDVAT(WS-MX) = '90'                                    
121800           OR TRAW-KDVAT(WS-MX) = '80'))                                  
121900             CONTINUE                                                     
122000           ELSE                                                           
122100*   WHEN SYSTEM HAVE TO USE A DUMMY VATCODE                               
122200             IF TRAW-KDVAT(WS-MX) = 'XX'                                  
122300             OR TRAW-KDVAT(WS-MX) = 'XZ'                                  
122400               CONTINUE                                                   
122500             ELSE                                                         
122600**** TIS HAVE PROBLEM SENDING FLPRIV = J, UNTIL THEN THIS FIX             
122700               IF TRAW-IDSYSTEM-SEND(WS-MX) = 'VSS'                       
122800                 PERFORM DB2-SELECT-T01VAT-SEND                           
122900                 IF (LINES-FOUND)                                         
123000                   CONTINUE                                               
123100                 ELSE                                                     
123200                   MOVE 'VAT CODE TIS MISS(KDVAT)' TO WS-ERRORTEXT        
123300                   MOVE IS-INVALID                 TO WS-IDFELKOD         
123400                   MOVE NOO                        TO KEYS-SW             
123500                 END-IF                                                   
123600               ELSE                                                       
123700**** END FIX FOR TIS                                                      
123800                 MOVE 'VAT CODE MISSING (KDVAT)' TO WS-ERRORTEXT          
123900                 MOVE IS-INVALID                 TO WS-IDFELKOD           
124000                 MOVE NOO                        TO KEYS-SW               
124100               END-IF                                                     
124200             END-IF                                                       
124300           END-IF                                                         
124400         ELSE                                                             
124500           MOVE 'VAT CODE WRONG   (KDVAT)' TO WS-ERRORTEXT                
124600           MOVE IS-INVALID                 TO WS-IDFELKOD                 
124700           MOVE NOO                        TO KEYS-SW                     
124800         END-IF                                                           
124900       END-IF                                                             
125000     END-IF                                                               
125100     .                                                                    
125200     EJECT                                                                
125300                                                                          
125400 BEF-CONTROL-CURRENCY SECTION.                                            
125500     PERFORM DB2-SELECT-T01CURR-MAX                                       
125600     IF LINES-FOUND                                                       
125700       PERFORM DB2-SELECT-T01CURR                                         
125800       IF LINES-FOUND                                                     
125900         CONTINUE                                                         
126000       ELSE                                                               
126100         MOVE 'CURRENCY CODE MISSING (KDVALISO)'  TO WS-ERRORTEXT         
126200         MOVE IS-INVALID                         TO WS-IDFELKOD           
126300         MOVE NOO                                TO KEYS-SW               
126400       END-IF                                                             
126500     ELSE                                                                 
126600       MOVE 'CURRENCY CODE MISSING (KDVALISO)'   TO WS-ERRORTEXT          
126700       MOVE IS-INVALID                           TO WS-IDFELKOD           
126800       MOVE NOO                                  TO KEYS-SW               
126900     END-IF                                                               
127000     .                                                                    
127100     EJECT                                                                
127200                                                                          
127300 BEG-DOCUMENT-TYPE SECTION.                                               
127400     PERFORM DB2-SELECT-T01DOTY                                           
127500     IF LINES-FOUND                                                       
127600       CONTINUE                                                           
127700     ELSE                                                                 
127800       MOVE 'DOCUMENT TYPE MISSING (KDFINDOC)'                            
127900                       TO WS-ERRORTEXT                                    
128000       MOVE IS-INVALID TO WS-IDFELKOD                                     
128100       MOVE NOO        TO KEYS-SW                                         
128200     END-IF                                                               
128300     .                                                                    
128400     EJECT                                                                
128500                                                                          
128600 BEK-CONTROL-DOCNRSERIE SECTION.                                          
128700       PERFORM DB2-SELECT-DOCNRSERIE-VER3                                 
128800       IF LINES-FOUND                                                     
128900         CONTINUE                                                         
129000       ELSE                                                               
129100         PERFORM DB2-SELECT-DOCNRSERIE-VER4                               
129200         IF LINES-FOUND                                                   
129300           CONTINUE                                                       
129400         ELSE                                                             
129500           MOVE 'DOCUMENT NUMBER NOT YET ASSIGN' TO WS-ERRORTEXT          
129600           MOVE IS-INVALID TO WS-IDFELKOD                                 
129700           MOVE NOO        TO KEYS-SW                                     
129800         END-IF                                                           
129900       END-IF                                                             
130000     .                                                                    
130100     EJECT                                                                
130200                                                                          
130300 BEL-CONTROL-APPROV-CUST SECTION.                                         
130400     PERFORM DB2-SELECT-T01CUGR                                           
130500     IF LINES-FOUND                                                       
130600       CONTINUE                                                           
130700     ELSE                                                                 
130800       MOVE 'APPROVED CUSTOMER TYPE/GROUP MISSING (KDPARTTY-GR)'          
130900                       TO WS-ERRORTEXT                                    
131000       MOVE IS-INVALID TO WS-IDFELKOD                                     
131100       MOVE NOO        TO KEYS-SW                                         
131200     END-IF                                                               
131300     .                                                                    
131400     EJECT                                                                
131500                                                                          
131600 BEM-CONTROL-BUSIN-RELATI SECTION.                                        
131700     PERFORM DB2-SELECT-T01BURE                                           
131800     IF LINES-FOUND                                                       
131900       CONTINUE                                                           
132000     ELSE                                                                 
132100       MOVE 'BUSINESS RELATION MISSING (KDPARTTY-GR-DOC)'                 
132200                       TO WS-ERRORTEXT                                    
132300       MOVE IS-INVALID TO WS-IDFELKOD                                     
132400       MOVE NOO        TO KEYS-SW                                         
132500     END-IF                                                               
132600     .                                                                    
132700     EJECT                                                                
132800                                                                          
132900 BF-UPDATE-T01TRAW SECTION.                                               
133000     IF WS-ERRORTEXT > SPACE                                              
133100       MOVE TRAW-IDLEGSEL(WS-MX)        TO DAP-IDLEGSEL                   
133200       MOVE TRAW-IDBUNDLE(WS-MX)        TO DAP-IDBUNDLE                   
133300       MOVE TRAW-DAREGDAT(WS-MX)        TO DAP-DAREGDAT                   
133400       MOVE TRAW-TIREGTID(WS-MX)        TO DAP-TIREGTID                   
133500       MOVE TRAW-IDREF(WS-MX)           TO DAP-IDREF                      
133600       MOVE TRAW-IDPARTNR(WS-MX)        TO DAP-IDPARTNR                   
133700       MOVE TRAW-KDFINDOC(WS-MX)        TO DAP-KDFINDOC                   
133800       MOVE TRAW-KDINVFRQ(WS-MX)        TO DAP-KDINVFRQ                   
133900       MOVE TRAW-IDARTNR-FINANCE(WS-MX) TO DAP-IDARTNR-FINANCE            
134000       MOVE TRAW-IDDC(WS-MX)            TO DAP-IDDC                       
134100       MOVE TRAW-KDVAT(WS-MX)           TO DAP-KDVAT                      
134200       MOVE TRAW-KDVALISO(WS-MX)        TO DAP-KDVALISO                   
134300       MOVE TRAW-PRARTBTO(WS-MX)        TO DAP-PRARTBTO                   
134400       MOVE TRAW-PRARTNTO(WS-MX)        TO DAP-PRARTNTO                   
134500       MOVE TRAW-IDEXCUST-1(WS-MX)      TO DAP-IDEXCUST-1                 
134600       MOVE TRAW-IDEXCUST-2(WS-MX)      TO DAP-IDEXCUST-2                 
134700       MOVE WS-IDREFRAD                 TO DAP-IDREFRAD                   
134800       MOVE WS-IDFELKOD                 TO DAP-IDFELKOD                   
134900       MOVE WS-ERRORTEXT                TO DAP-ERRORTEXT                  
135000       MOVE WS-IDFELKOD                 TO TRAW-IDFELKOD(WS-MX)           
135100       MOVE WS-ERRORTEXT                TO TRAW-BEFEL(WS-MX)              
135200     END-IF                                                               
135300     MOVE WS-MX TO WS-UPDATE-MULTI                                        
135400     ADD 1 TO DAP-ERROR-COUNTER                                           
135500     MOVE NOO TO BUNT-SW                                                  
135600     IF WS-MX > 10                                                        
135700       PERFORM DB2-UPDATE-T01TRAW-ERROR                                   
135800     ELSE                                                                 
135900       IF WS-MX = 1                                                       
136000         PERFORM DB2-UPDATE-T01TRAW-ERROR1                                
136100       END-IF                                                             
136200       IF WS-MX = 2                                                       
136300         PERFORM DB2-UPDATE-T01TRAW-ERROR2                                
136400       END-IF                                                             
136500       IF WS-MX = 3                                                       
136600         PERFORM DB2-UPDATE-T01TRAW-ERROR3                                
136700       END-IF                                                             
136800       IF WS-MX = 4                                                       
136900         PERFORM DB2-UPDATE-T01TRAW-ERROR4                                
137000       END-IF                                                             
137100       IF WS-MX = 5                                                       
137200         PERFORM DB2-UPDATE-T01TRAW-ERROR5                                
137300       END-IF                                                             
137400       IF WS-MX = 6                                                       
137500         PERFORM DB2-UPDATE-T01TRAW-ERROR6                                
137600       END-IF                                                             
137700       IF WS-MX = 7                                                       
137800         PERFORM DB2-UPDATE-T01TRAW-ERROR7                                
137900       END-IF                                                             
138000       IF WS-MX = 8                                                       
138100         PERFORM DB2-UPDATE-T01TRAW-ERROR8                                
138200       END-IF                                                             
138300       IF WS-MX = 9                                                       
138400         PERFORM DB2-UPDATE-T01TRAW-ERROR9                                
138500       END-IF                                                             
138600       IF WS-MX = 10                                                      
138700         PERFORM DB2-UPDATE-T01TRAW-ERROR10                               
138800       END-IF                                                             
138900     END-IF                                                               
139000     .                                                                    
139100     EJECT                                                                
139200                                                                          
139300 C-RESTART-OF-OWN-TRANS SECTION.                                          
139400     PERFORM S04-SEND-TO-WF0202-OPEN                                      
139500     PERFORM S05-SEND-TO-WF0202-PUT                                       
139600     PERFORM S06-SEND-TO-WF0202-CLOSE                                     
139700     .                                                                    
139800     EJECT                                                                
139900                                                                          
140000 D-START-PGM-WF020300 SECTION.                                            
140100     PERFORM S07-SEND-TO-WF0203-OPEN                                      
140200     PERFORM S08-SEND-TO-WF0203-PUT                                       
140300     PERFORM S09-SEND-TO-WF0203-CLOSE                                     
140400     .                                                                    
140500     EJECT                                                                
140600                                                                          
140700 F-SEND-TO-DAP SECTION.                                                   
140800*****************************************************************         
140900*UPDATE  PARAMETERS*****                                                  
141000****************************************************                      
141100     MOVE 001                        TO ERROR-REQU-IDMSGVER               
141200     MOVE 'R'                        TO ERROR-REQU-KDPGMACT               
141300     MOVE IDPGM                      TO ERROR-REQU-IDUSER                 
141400                                                                          
141500     MOVE 'BILLITERROR'              TO HDR-IDOUTTYPE                     
141600     MOVE SPACE                      TO HDR-IDOUTREC                      
141700                                                                          
141800     MOVE TRAW-IDSYSTEM-SEND(WS-MX) TO HDR-IDOUTREC                       
141900     MOVE TRAW-IDBUNDLE(WS-MX)      TO HDR-IDLIST                         
142000                                                                          
142100*****UPDATES LINE ERROR 1                                                 
142200     MOVE 'DISTRICT: '      TO DAP-LINE-TEXT-1                            
142300     IF TRAW-IDSYSTEM-SEND(WS-MX) = 'VSS'                                 
142400       MOVE SPACE           TO DAP-LINE-IDEXCUST-1                        
142500     ELSE                                                                 
142600       MOVE DAP-IDEXCUST-1  TO DAP-LINE-IDEXCUST-1                        
142700     END-IF                                                               
142800     MOVE ' CUSTOMER: '     TO DAP-LINE-TEXT-2                            
142900     IF TRAW-IDSYSTEM-SEND(WS-MX) = 'VSS'                                 
143000       MOVE DAP-IDEXCUST-1  TO DAP-LINE-IDEXCUST-2                        
143100     ELSE                                                                 
143200       MOVE DAP-IDEXCUST-2  TO DAP-LINE-IDEXCUST-2                        
143300     END-IF                                                               
143400     IF TRAW-KDFINDOC(WS-MX) = 'CR'                                       
143500     OR (TRAW-KDFINDOC(WS-MX) = 'INT'                                     
143600     AND TRAW-IDLEGSEL(WS-MX) = WS-IDLEGSEL                               
143700     AND TRAW-IDLEGSEL(WS-MX) NOT = 'VCCS'                                
143800     AND TRAW-KDANMORS(WS-MX) > ' ')                                      
143900       MOVE ' REPORT: '     TO DAP-LINE-TEXT-3                            
144000     ELSE                                                                 
144100       MOVE ' ORDER:  '     TO DAP-LINE-TEXT-3                            
144200     END-IF                                                               
144300     MOVE DAP-IDREF         TO DAP-LINE-IDREF                             
144400                                                                          
144500*****UPDATES LINE ERROR 2                                                 
144600     MOVE 'PARMA: '           TO DAP-LINE-TEXT-4                          
144700     MOVE DAP-IDPARTNR        TO DAP-LINE-IDPARTNR                        
144800     MOVE ' DC: '             TO DAP-LINE-TEXT-5                          
144900     MOVE DAP-IDDC            TO DAP-LINE-IDDC                            
145000     MOVE ' DOC TYPE: '       TO DAP-LINE-TEXT-6                          
145100     MOVE DAP-KDFINDOC        TO DAP-LINE-KDFINDOC                        
145200     MOVE ' FREQUENCE: '      TO DAP-LINE-TEXT-7                          
145300     MOVE DAP-KDINVFRQ        TO DAP-LINE-KDINVFRQ                        
145400     MOVE ' PART NO: '        TO DAP-LINE-TEXT-8                          
145500     MOVE DAP-IDARTNR-FINANCE TO DAP-LINE-IDARTNR-FINANCE                 
145600                                                                          
145700*****UPDATES LINE ERROR 3                                                 
145800     MOVE 'ERROR MESSAGE: ' TO DAP-LINE-TEXT-10                           
145900     MOVE DAP-ERRORTEXT      TO DAP-LINE-ERRORTEXT                        
146000     MOVE DAP-ERROR-COUNTER  TO DAP-ERROR-COUNT                           
146100     MOVE ' TOTAL LINES IN ERROR BUNDLE: ' TO DAP-LINE-TEXT-9             
146200     MOVE DAP-ERROR-COUNT    TO DAP-LINE-ERROR-COUNT                      
146300                                                                          
146400*****UPDATES LINE ERROR 4                                                 
146500     MOVE 'VAT CODE: '      TO DAP-LINE-TEXT-11                           
146600     MOVE DAP-KDVAT         TO DAP-LINE-KDVAT                             
146700     MOVE ' CURRENCY: '     TO DAP-LINE-TEXT-12                           
146800     MOVE DAP-KDVALISO      TO DAP-LINE-KDVALISO                          
146900     MOVE ' PRICE BTO: '    TO DAP-LINE-TEXT-13                           
147000     MOVE DAP-PRARTBTO      TO DAP-LINE-PRARTBTO                          
147100     MOVE ' PRICE NTO: '    TO DAP-LINE-TEXT-14                           
147200     MOVE DAP-PRARTNTO      TO DAP-LINE-PRARTNTO                          
147300                                                                          
147400*******SENDING ERRORTEXT TO DAP                                           
147500     PERFORM S90-SEND-OPEN                                                
147600     PERFORM S90-PUT-HEADER                                               
147700     PERFORM S90-PUT-LINE-3                                               
147800     PERFORM S90-PUT-LINE                                                 
147900     PERFORM S90-PUT-LINE-2                                               
148000     PERFORM S90-PUT-LINE-4                                               
148100     PERFORM S90-SEND-CLOSE                                               
148200     .                                                                    
148300     EJECT                                                                
148400                                                                          
148500 I-SELECT-T01TBUN-NONCNTRL SECTION.                                       
148600     PERFORM DB2-SELECT-T01TBUN-NONCNTRL                                  
148700     IF LINES-FOUND                                                       
148800       MOVE YES   TO WS-BUNDLES-NOT-CONTROLLED                            
148900     ELSE                                                                 
149000       MOVE NOO   TO WS-BUNDLES-NOT-CONTROLLED                            
149100     END-IF                                                               
149200     .                                                                    
149300     EJECT                                                                
149400                                                                          
149500*    --- DISPATCHER-SECTIONS--------                                      
149600 S01-READ-OPEN SECTION.                                                   
149700     MOVE 'CARPARTS.BILLIT.VALIDATE' TO RECV-ADDISPABS                    
149800     MOVE 'OPEN'                     TO RECV-KDFUNC                       
149900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
150000                         RECV-OPEN-AREA                                   
150100     IF RECV-KDRC > ZERO                                                  
150200       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
150300       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
150400       DELIMITED BY SIZE INTO ERRORTEXT                                   
150500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
150600     END-IF                                                               
150700     .                                                                    
150800     EJECT                                                                
150900                                                                          
151000 S02-READ-MESSAGE SECTION.                                                
151100     MOVE 'GET'                            TO RECV-KDFUNC                 
151200     MOVE LENGTH OF RECV-AREA              TO RECV-KVDLEN                 
151300     CALL WZ01RECV USING RECV-CONTROL-AREA                                
151400                         RECV-KVDLEN                                      
151500                         RECV-AREA                                        
151600     IF RECV-KDRC > 1                                                     
151700       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
151800       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
151900       DELIMITED BY SIZE INTO ERRORTEXT                                   
152000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
152100     END-IF                                                               
152200     .                                                                    
152300     EJECT                                                                
152400                                                                          
152500 S03-READ-CLOSE SECTION.                                                  
152600     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
152700     CALL WZ01RECV USING RECV-CONTROL-AREA                                
152800                                                                          
152900     IF RECV-KDRC > 0                                                     
153000       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
153100       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
153200       DELIMITED BY SIZE INTO ERRORTEXT                                   
153300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
153400     END-IF                                                               
153500     .                                                                    
153600     EJECT                                                                
153700                                                                          
153800 S04-SEND-TO-WF0202-OPEN SECTION.                                         
153900     MOVE 'CARPARTS.BILLIT.VALIDATE' TO SEND-ADDISPABS                    
154000     MOVE 'OPEN'                     TO SEND-KDFUNC                       
154100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
154200                         SEND-OPEN-AREA                                   
154300     IF SEND-KDRC > ZERO                                                  
154400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
154500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
154600       DELIMITED BY SIZE INTO ERRORTEXT                                   
154700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
154800     END-IF                                                               
154900     .                                                                    
155000     EJECT                                                                
155100                                                                          
155200 S05-SEND-TO-WF0202-PUT SECTION.                                          
155300     MOVE 'PUT'                           TO SEND-KDFUNC                  
155400     MOVE LENGTH OF SEND-AREA             TO SEND-KVDLEN                  
155500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
155600                         SEND-KVDLEN                                      
155700                         SEND-AREA                                        
155800     IF SEND-KDRC > ZERO                                                  
155900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
156000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
156100       DELIMITED BY SIZE INTO ERRORTEXT                                   
156200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
156300     END-IF                                                               
156400     .                                                                    
156500     EJECT                                                                
156600                                                                          
156700 S06-SEND-TO-WF0202-CLOSE SECTION.                                        
156800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
156900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
157000                                                                          
157100     IF SEND-KDRC > 0                                                     
157200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
157300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
157400       DELIMITED BY SIZE INTO ERRORTEXT                                   
157500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
157600     END-IF                                                               
157700     .                                                                    
157800     EJECT                                                                
157900                                                                          
158000 S07-SEND-TO-WF0203-OPEN SECTION.                                         
158100     MOVE 'CARPARTS.BILLIT.INSERT'   TO SEND-ADDISPABS                    
158200     MOVE 'OPEN'                     TO SEND-KDFUNC                       
158300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
158400                         SEND-OPEN-AREA                                   
158500     IF SEND-KDRC > ZERO                                                  
158600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
158700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
158800       DELIMITED BY SIZE INTO ERRORTEXT                                   
158900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
159000     END-IF                                                               
159100     .                                                                    
159200     EJECT                                                                
159300                                                                          
159400 S08-SEND-TO-WF0203-PUT SECTION.                                          
159500     MOVE 'PUT'                            TO SEND-KDFUNC                 
159600     MOVE LENGTH OF SEND-AREA-2            TO SEND-KVDLEN                 
159700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
159800                         SEND-KVDLEN                                      
159900                         SEND-AREA-2                                      
160000     IF SEND-KDRC > 1                                                     
160100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
160200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
160300       DELIMITED BY SIZE INTO ERRORTEXT                                   
160400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
160500     END-IF                                                               
160600     .                                                                    
160700     EJECT                                                                
160800                                                                          
160900 S09-SEND-TO-WF0203-CLOSE SECTION.                                        
161000     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
161100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
161200                                                                          
161300     IF SEND-KDRC > 0                                                     
161400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
161500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
161600       DELIMITED BY SIZE INTO ERRORTEXT                                   
161700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
161800     END-IF                                                               
161900     .                                                                    
162000     EJECT                                                                
162100                                                                          
162200 S90-SEND-OPEN SECTION.                                                   
162300     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
162400     MOVE 'OPEN'                          TO SEND-KDFUNC                  
162500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
162600                         SEND-OPEN-AREA                                   
162700     IF SEND-KDRC > ZERO                                                  
162800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
162900       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
163000       DELIMITED BY SIZE INTO ERRORTEXT                                   
163100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
163200     END-IF                                                               
163300     .                                                                    
163400     EJECT                                                                
163500                                                                          
163600 S90-PUT-HEADER SECTION.                                                  
163700     MOVE 'PUT'                           TO SEND-KDFUNC                  
163800     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
163900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
164000                         SEND-KVDLEN                                      
164100                         HDR-AREA                                         
164200     IF SEND-KDRC > ZERO                                                  
164300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
164400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
164500       DELIMITED BY SIZE INTO ERRORTEXT                                   
164600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
164700     END-IF                                                               
164800     .                                                                    
164900     EJECT                                                                
165000                                                                          
165100 S90-PUT-LINE SECTION.                                                    
165200     MOVE 'PUT'                           TO SEND-KDFUNC                  
165300     MOVE LENGTH OF DAP-LINE-AREA         TO SEND-KVDLEN                  
165400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
165500                         SEND-KVDLEN                                      
165600                         DAP-LINE-AREA                                    
165700     IF SEND-KDRC > ZERO                                                  
165800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
165900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
166000       DELIMITED BY SIZE INTO ERRORTEXT                                   
166100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
166200     END-IF                                                               
166300     .                                                                    
166400     EJECT                                                                
166500                                                                          
166600 S90-PUT-LINE-2 SECTION.                                                  
166700     MOVE 'PUT'                           TO SEND-KDFUNC                  
166800     MOVE LENGTH OF DAP-LINE-AREA-2       TO SEND-KVDLEN                  
166900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
167000                         SEND-KVDLEN                                      
167100                         DAP-LINE-AREA-2                                  
167200     IF SEND-KDRC > ZERO                                                  
167300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
167400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
167500       DELIMITED BY SIZE INTO ERRORTEXT                                   
167600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
167700     END-IF                                                               
167800     .                                                                    
167900     EJECT                                                                
168000                                                                          
168100 S90-PUT-LINE-3 SECTION.                                                  
168200     MOVE 'PUT'                           TO SEND-KDFUNC                  
168300     MOVE LENGTH OF DAP-LINE-AREA-3       TO SEND-KVDLEN                  
168400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
168500                         SEND-KVDLEN                                      
168600                         DAP-LINE-AREA-3                                  
168700     IF SEND-KDRC > ZERO                                                  
168800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
168900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
169000       DELIMITED BY SIZE INTO ERRORTEXT                                   
169100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
169200     END-IF                                                               
169300     .                                                                    
169400     EJECT                                                                
169500                                                                          
169600 S90-PUT-LINE-4 SECTION.                                                  
169700     MOVE 'PUT'                           TO SEND-KDFUNC                  
169800     MOVE LENGTH OF DAP-LINE-AREA-4       TO SEND-KVDLEN                  
169900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
170000                         SEND-KVDLEN                                      
170100                         DAP-LINE-AREA-4                                  
170200     IF SEND-KDRC > ZERO                                                  
170300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
170400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
170500       DELIMITED BY SIZE INTO ERRORTEXT                                   
170600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
170700     END-IF                                                               
170800     .                                                                    
170900     EJECT                                                                
171000                                                                          
171100 S90-SEND-CLOSE SECTION.                                                  
171200     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
171300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
171400     .                                                                    
171500     EJECT                                                                
171600                                                                          
171700 DB2-OPEN-T01TBUN-CRS SECTION.                                            
171800     MOVE 000100  TO GOOD-SQLCODECODES                                    
171900     EXEC SQL DECLARE T01TBUN-CRS CURSOR FOR                              
172000       SELECT IDLEGSEL,                                                   
172100              IDBUNDLE,                                                   
172200              DAREGDAT,                                                   
172300              TIREGTID,                                                   
172400              FLFEL,                                                      
172500              FLKNTRL,                                                    
172600              FLKLAR                                                      
172700                                                                          
172800       FROM   T01TBUN                                                     
172900                                                                          
173000       WHERE  FLKNTRL = :NOO                                              
173100       AND    FLKLAR  = :NOO                                              
173200                                                                          
173300       FOR UPDATE OF FLFEL,                                               
173400                     FLKNTRL                                              
173500     END-EXEC                                                             
173600                                                                          
173700     MOVE 000100       TO GOOD-SQLCODECODES                               
173800     EXEC SQL OPEN T01TBUN-CRS END-EXEC                                   
173900     .                                                                    
174000     EJECT                                                                
174100                                                                          
174200 DB2-FETCH-T01TBUN-CRS SECTION.                                           
174300     MOVE 000100       TO GOOD-SQLCODECODES                               
174400     EXEC SQL FETCH T01TBUN-CRS INTO                                      
174500       :T01TBUN-IDLEGSEL,                                                 
174600       :T01TBUN-IDBUNDLE,                                                 
174700       :T01TBUN-DAREGDAT,                                                 
174800       :T01TBUN-TIREGTID,                                                 
174900       :T01TBUN-FLFEL,                                                    
175000       :T01TBUN-FLKNTRL,                                                  
175100       :T01TBUN-FLKLAR                                                    
175200     END-EXEC                                                             
175300                                                                          
175400     MOVE SQLCODE      TO SQLCODE-WS                                      
175500     PERFORM DB2-STATUS-CONTROL                                           
175600     .                                                                    
175700     EJECT                                                                
175800                                                                          
175900 DB2-CLOSE-T01TBUN-CRS SECTION.                                           
176000     EXEC SQL CLOSE T01TBUN-CRS END-EXEC                                  
176100     .                                                                    
176200     EJECT                                                                
176300                                                                          
176400 DB2-UPDATE-T01TBUN SECTION.                                              
176500     MOVE 000     TO GOOD-SQLCODECODES                                    
176600     EXEC SQL UPDATE T01TBUN                                              
176700       SET FLFEL    = :WS-FLFEL,                                          
176800           FLKNTRL  = :YES                                                
176900                                                                          
177000       WHERE CURRENT OF T01TBUN-CRS                                       
177100     END-EXEC                                                             
177200                                                                          
177300     ADD 1        TO RESTART-IX                                           
177400     MOVE SQLCODE TO SQLCODE-WS                                           
177500     PERFORM DB2-STATUS-CONTROL                                           
177600     .                                                                    
177700     EJECT                                                                
177800                                                                          
177900 DB2-OPEN-T01TRAW-CRS SECTION.                                            
178000     MOVE 000100  TO GOOD-SQLCODECODES                                    
178100     EXEC SQL                                                             
178200       DECLARE T01TRAW-CRS CURSOR WITH ROWSET POSITIONING FOR             
178300       SELECT IDLEGSEL                                                    
178400             ,IDBUNDLE                                                    
178500             ,DAREGDAT                                                    
178600             ,TIREGTID                                                    
178700             ,IDREF                                                       
178800             ,DAREFDAT                                                    
178900             ,IDREFRAD                                                    
179000             ,BEVOLREF                                                    
179100             ,IDLANDX3_SEND                                               
179200             ,IDLANDX3_REC                                                
179300             ,IDLEVNR                                                     
179400             ,IDPARTNR                                                    
179500             ,IDEXCUST_1                                                  
179600             ,IDEXCUST_2                                                  
179700             ,IDEXCUST_3                                                  
179800             ,IDOPTION_1                                                  
179900             ,IDOPTION_2                                                  
180000             ,IDOPTION_3                                                  
180100             ,IDOPTION_4                                                  
180200             ,IDOPTION_5                                                  
180300             ,IDAPPEND                                                    
180400             ,IDARTNR_FINANCE                                             
180500             ,IDSTATNR                                                    
180600             ,VKORDBTO_KOLLI                                              
180700             ,VKARTNTO                                                    
180800             ,PRARTBTO                                                    
180900             ,PRARTNTO                                                    
181000             ,REARTRAB                                                    
181100             ,KVBEART                                                     
181200             ,KVLEVART                                                    
181300             ,BEART                                                       
181400             ,FLSOFT                                                      
181500             ,FLSPECPR                                                    
181600             ,FLFREE                                                      
181700             ,FLPRIV                                                      
181800             ,KDVAT                                                       
181900             ,KDVALISO                                                    
182000             ,KDINVFRQ                                                    
182100             ,KDFINDOC                                                    
182200             ,IDBREAK_1                                                   
182300             ,IDBREAK_2                                                   
182400             ,IDSEQ_1                                                     
182500             ,IDSEQ_2                                                     
182600             ,IDSEQ_3                                                     
182700             ,KDARTURS                                                    
182800             ,KDANMORS                                                    
182900             ,IDFAKREF                                                    
183000             ,DAFAKREF                                                    
183100             ,IDDC                                                        
183200             ,KDFRAKT                                                     
183300             ,BELEVVIL                                                    
183400             ,IDACCNT_1                                                   
183500             ,IDACCNT_2                                                   
183600             ,IDACCNT_3                                                   
183700             ,IDACCNT_4                                                   
183800             ,IDSYSTEM_SEND                                               
183900             ,IDSYSTEM_REC                                                
184000             ,IDFELKOD                                                    
184100             ,BEFEL                                                       
184200             ,FLPCOO                                                      
184300             ,IDLEVNR_ART                                                 
184400             ,IDTRACK_1                                                   
184500             ,KVANT_TRACK_1                                               
184600             ,IDTRACK_2                                                   
184700             ,KVANT_TRACK_2                                               
184800             ,IDTRACK_3                                                   
184900             ,KVANT_TRACK_3                                               
185000             ,IDTRACK_4                                                   
185100             ,KVANT_TRACK_4                                               
185200             ,IDTRACK_5                                                   
185300             ,KVANT_TRACK_5                                               
185400                                                                          
185500       FROM T01TRAW                                                       
185600                                                                          
185700       WHERE IDLEGSEL = :T01TBUN-IDLEGSEL                                 
185800       AND   IDBUNDLE = :T01TBUN-IDBUNDLE                                 
185900       AND   DAREGDAT = :T01TBUN-DAREGDAT                                 
186000       AND   TIREGTID = :T01TBUN-TIREGTID                                 
186100                                                                          
186200       FOR UPDATE OF IDFELKOD,                                            
186300                     BEFEL                                                
186400     END-EXEC                                                             
186500                                                                          
186600     MOVE 000100       TO GOOD-SQLCODECODES                               
186700     EXEC SQL OPEN T01TRAW-CRS END-EXEC                                   
186800     .                                                                    
186900     EJECT                                                                
187000                                                                          
187100 DB2-FETCH-T01TRAW-CRS SECTION.                                           
187200     MOVE 000100       TO GOOD-SQLCODECODES                               
187300     EXEC SQL                                                             
187400     FETCH NEXT ROWSET FROM T01TRAW-CRS FOR 100 ROWS                      
187500     INTO :TRAW-IDLEGSEL                                                  
187600         ,:TRAW-IDBUNDLE                                                  
187700         ,:TRAW-DAREGDAT                                                  
187800         ,:TRAW-TIREGTID                                                  
187900         ,:TRAW-IDREF                                                     
188000         ,:TRAW-DAREFDAT                                                  
188100         ,:WS-TRAW-IDREFRAD                                               
188200         ,:TRAW-BEVOLREF                                                  
188300         ,:TRAW-IDLANDX3-SEND                                             
188400         ,:TRAW-IDLANDX3-REC                                              
188500         ,:TRAW-IDLEVNR                                                   
188600         ,:TRAW-IDPARTNR                                                  
188700         ,:TRAW-IDEXCUST-1                                                
188800         ,:TRAW-IDEXCUST-2                                                
188900         ,:TRAW-IDEXCUST-3                                                
189000         ,:TRAW-IDOPTION-1                                                
189100         ,:TRAW-IDOPTION-2                                                
189200         ,:TRAW-IDOPTION-3                                                
189300         ,:TRAW-IDOPTION-4                                                
189400         ,:TRAW-IDOPTION-5                                                
189500         ,:TRAW-IDAPPEND                                                  
189600         ,:TRAW-IDARTNR-FINANCE                                           
189700         ,:TRAW-IDSTATNR                                                  
189800         ,:TRAW-VKORDBTO-KOLLI                                            
189900         ,:TRAW-VKARTNTO                                                  
190000         ,:TRAW-PRARTBTO                                                  
190100         ,:TRAW-PRARTNTO                                                  
190200         ,:TRAW-REARTRAB                                                  
190300         ,:TRAW-KVBEART                                                   
190400         ,:TRAW-KVLEVART                                                  
190500         ,:TRAW-BEART                                                     
190600         ,:TRAW-FLSOFT                                                    
190700         ,:TRAW-FLSPECPR                                                  
190800         ,:TRAW-FLFREE                                                    
190900         ,:TRAW-FLPRIV                                                    
191000         ,:TRAW-KDVAT                                                     
191100         ,:TRAW-KDVALISO                                                  
191200         ,:TRAW-KDINVFRQ                                                  
191300         ,:TRAW-KDFINDOC                                                  
191400         ,:TRAW-IDBREAK-1                                                 
191500         ,:TRAW-IDBREAK-2                                                 
191600         ,:TRAW-IDSEQ-1                                                   
191700         ,:TRAW-IDSEQ-2                                                   
191800         ,:TRAW-IDSEQ-3                                                   
191900         ,:TRAW-KDARTURS                                                  
192000         ,:TRAW-KDANMORS                                                  
192100         ,:TRAW-IDFAKREF                                                  
192200         ,:TRAW-DAFAKREF                                                  
192300         ,:TRAW-IDDC                                                      
192400         ,:TRAW-KDFRAKT                                                   
192500         ,:TRAW-BELEVVIL                                                  
192600         ,:TRAW-IDACCNT-1                                                 
192700         ,:TRAW-IDACCNT-2                                                 
192800         ,:TRAW-IDACCNT-3                                                 
192900         ,:TRAW-IDACCNT-4                                                 
193000         ,:TRAW-IDSYSTEM-SEND                                             
193100         ,:TRAW-IDSYSTEM-REC                                              
193200         ,:TRAW-IDFELKOD                                                  
193300         ,:TRAW-BEFEL                                                     
193400         ,:TRAW-FLPCOO                                                    
193500         ,:TRAW-IDLEVNR-ART                                               
193600         ,:TRAW-IDTRACK-1                                                 
193700         ,:TRAW-KVANT-TRACK-1                                             
193800         ,:TRAW-IDTRACK-2                                                 
193900         ,:TRAW-KVANT-TRACK-2                                             
194000         ,:TRAW-IDTRACK-3                                                 
194100         ,:TRAW-KVANT-TRACK-3                                             
194200         ,:TRAW-IDTRACK-4                                                 
194300         ,:TRAW-KVANT-TRACK-4                                             
194400         ,:TRAW-IDTRACK-5                                                 
194500         ,:TRAW-KVANT-TRACK-5                                             
194600     END-EXEC                                                             
194700     MOVE SQLCODE      TO SQLCODE-WS                                      
194800     PERFORM DB2-STATUS-CONTROL                                           
194900     .                                                                    
195000     EJECT                                                                
195100                                                                          
195200 DB2-CLOSE-T01TRAW-CRS SECTION.                                           
195300     EXEC SQL CLOSE T01TRAW-CRS END-EXEC                                  
195400     .                                                                    
195500     EJECT                                                                
195600                                                                          
195700 DB2-UPDATE-T01TRAW-ERROR SECTION.                                        
195800     MOVE 000     TO GOOD-SQLCODECODES                                    
195900     EXEC SQL UPDATE T01TRAW                                              
196000       SET IDFELKOD = :TRAW-IDFELKOD                                      
196100          ,BEFEL    = :TRAW-BEFEL                                         
196200*     WHERE CURRENT OF T01TRAW-CRS                                        
196300      WHERE CURRENT OF T01TRAW-CRS FOR ROW :WS-UPDATE-MULTI               
196400                                                         OF ROWSET        
196500     END-EXEC                                                             
196600                                                                          
196700     MOVE SQLCODE TO SQLCODE-WS                                           
196800     PERFORM DB2-STATUS-CONTROL                                           
196900     .                                                                    
197000     EJECT                                                                
197100                                                                          
197200 DB2-UPDATE-T01TRAW-ERROR1 SECTION.                                       
197300     MOVE 000     TO GOOD-SQLCODECODES                                    
197400     EXEC SQL UPDATE T01TRAW                                              
197500       SET IDFELKOD = :TRAW-IDFELKOD                                      
197600          ,BEFEL    = :TRAW-BEFEL                                         
197700      WHERE CURRENT OF T01TRAW-CRS FOR ROW 1 OF ROWSET                    
197800     END-EXEC                                                             
197900                                                                          
198000     MOVE SQLCODE TO SQLCODE-WS                                           
198100     PERFORM DB2-STATUS-CONTROL                                           
198200     .                                                                    
198300     EJECT                                                                
198400                                                                          
198500 DB2-UPDATE-T01TRAW-ERROR2 SECTION.                                       
198600     MOVE 000     TO GOOD-SQLCODECODES                                    
198700     EXEC SQL UPDATE T01TRAW                                              
198800       SET IDFELKOD = :TRAW-IDFELKOD                                      
198900          ,BEFEL    = :TRAW-BEFEL                                         
199000      WHERE CURRENT OF T01TRAW-CRS FOR ROW 2 OF ROWSET                    
199100     END-EXEC                                                             
199200                                                                          
199300     MOVE SQLCODE TO SQLCODE-WS                                           
199400     PERFORM DB2-STATUS-CONTROL                                           
199500     .                                                                    
199600     EJECT                                                                
199700                                                                          
199800 DB2-UPDATE-T01TRAW-ERROR3 SECTION.                                       
199900     MOVE 000     TO GOOD-SQLCODECODES                                    
200000     EXEC SQL UPDATE T01TRAW                                              
200100       SET IDFELKOD = :TRAW-IDFELKOD                                      
200200          ,BEFEL    = :TRAW-BEFEL                                         
200300      WHERE CURRENT OF T01TRAW-CRS FOR ROW 3 OF ROWSET                    
200400     END-EXEC                                                             
200500                                                                          
200600     MOVE SQLCODE TO SQLCODE-WS                                           
200700     PERFORM DB2-STATUS-CONTROL                                           
200800     .                                                                    
200900     EJECT                                                                
201000                                                                          
201100 DB2-UPDATE-T01TRAW-ERROR4 SECTION.                                       
201200     MOVE 000     TO GOOD-SQLCODECODES                                    
201300     EXEC SQL UPDATE T01TRAW                                              
201400       SET IDFELKOD = :TRAW-IDFELKOD                                      
201500          ,BEFEL    = :TRAW-BEFEL                                         
201600      WHERE CURRENT OF T01TRAW-CRS FOR ROW 4 OF ROWSET                    
201700     END-EXEC                                                             
201800                                                                          
201900     MOVE SQLCODE TO SQLCODE-WS                                           
202000     PERFORM DB2-STATUS-CONTROL                                           
202100     .                                                                    
202200     EJECT                                                                
202300                                                                          
202400 DB2-UPDATE-T01TRAW-ERROR5 SECTION.                                       
202500     MOVE 000     TO GOOD-SQLCODECODES                                    
202600     EXEC SQL UPDATE T01TRAW                                              
202700       SET IDFELKOD = :TRAW-IDFELKOD                                      
202800          ,BEFEL    = :TRAW-BEFEL                                         
202900      WHERE CURRENT OF T01TRAW-CRS FOR ROW 5 OF ROWSET                    
203000     END-EXEC                                                             
203100                                                                          
203200     MOVE SQLCODE TO SQLCODE-WS                                           
203300     PERFORM DB2-STATUS-CONTROL                                           
203400     .                                                                    
203500     EJECT                                                                
203600                                                                          
203700 DB2-UPDATE-T01TRAW-ERROR6 SECTION.                                       
203800     MOVE 000     TO GOOD-SQLCODECODES                                    
203900     EXEC SQL UPDATE T01TRAW                                              
204000       SET IDFELKOD = :TRAW-IDFELKOD                                      
204100          ,BEFEL    = :TRAW-BEFEL                                         
204200      WHERE CURRENT OF T01TRAW-CRS FOR ROW 6 OF ROWSET                    
204300     END-EXEC                                                             
204400                                                                          
204500     MOVE SQLCODE TO SQLCODE-WS                                           
204600     PERFORM DB2-STATUS-CONTROL                                           
204700     .                                                                    
204800     EJECT                                                                
204900                                                                          
205000 DB2-UPDATE-T01TRAW-ERROR7 SECTION.                                       
205100     MOVE 000     TO GOOD-SQLCODECODES                                    
205200     EXEC SQL UPDATE T01TRAW                                              
205300       SET IDFELKOD = :TRAW-IDFELKOD                                      
205400          ,BEFEL    = :TRAW-BEFEL                                         
205500      WHERE CURRENT OF T01TRAW-CRS FOR ROW 7 OF ROWSET                    
205600     END-EXEC                                                             
205700                                                                          
205800     MOVE SQLCODE TO SQLCODE-WS                                           
205900     PERFORM DB2-STATUS-CONTROL                                           
206000     .                                                                    
206100     EJECT                                                                
206200                                                                          
206300 DB2-UPDATE-T01TRAW-ERROR8 SECTION.                                       
206400     MOVE 000     TO GOOD-SQLCODECODES                                    
206500     EXEC SQL UPDATE T01TRAW                                              
206600       SET IDFELKOD = :TRAW-IDFELKOD                                      
206700          ,BEFEL    = :TRAW-BEFEL                                         
206800      WHERE CURRENT OF T01TRAW-CRS FOR ROW 8 OF ROWSET                    
206900     END-EXEC                                                             
207000                                                                          
207100     MOVE SQLCODE TO SQLCODE-WS                                           
207200     PERFORM DB2-STATUS-CONTROL                                           
207300     .                                                                    
207400     EJECT                                                                
207500                                                                          
207600 DB2-UPDATE-T01TRAW-ERROR9 SECTION.                                       
207700     MOVE 000     TO GOOD-SQLCODECODES                                    
207800     EXEC SQL UPDATE T01TRAW                                              
207900       SET IDFELKOD = :TRAW-IDFELKOD                                      
208000          ,BEFEL    = :TRAW-BEFEL                                         
208100      WHERE CURRENT OF T01TRAW-CRS FOR ROW 9 OF ROWSET                    
208200     END-EXEC                                                             
208300                                                                          
208400     MOVE SQLCODE TO SQLCODE-WS                                           
208500     PERFORM DB2-STATUS-CONTROL                                           
208600     .                                                                    
208700     EJECT                                                                
208800                                                                          
208900 DB2-UPDATE-T01TRAW-ERROR10 SECTION.                                      
209000     MOVE 000     TO GOOD-SQLCODECODES                                    
209100     EXEC SQL UPDATE T01TRAW                                              
209200       SET IDFELKOD = :TRAW-IDFELKOD                                      
209300          ,BEFEL    = :TRAW-BEFEL                                         
209400      WHERE CURRENT OF T01TRAW-CRS FOR ROW 10 OF ROWSET                   
209500     END-EXEC                                                             
209600                                                                          
209700     MOVE SQLCODE TO SQLCODE-WS                                           
209800     PERFORM DB2-STATUS-CONTROL                                           
209900     .                                                                    
210000     EJECT                                                                
210100                                                                          
210200 DB2-SELECT-T01SYST SECTION.                                              
210300     MOVE 000100    TO GOOD-SQLCODECODES                                  
210400     EXEC SQL                                                             
210500       SELECT   KDBEHS                                                    
210600             ,  KDBEHX                                                    
210700                                                                          
210800       INTO    :SYST-KDBEHS                                               
210900             , :SYST-KDBEHX                                               
211000                                                                          
211100       FROM     T01SYST                                                   
211200                                                                          
211300     END-EXEC                                                             
211400                                                                          
211500     MOVE SQLCODE      TO SQLCODE-WS                                      
211600     PERFORM DB2-STATUS-CONTROL                                           
211700     .                                                                    
211800     EJECT                                                                
211900                                                                          
212000 DB2-UPDATE-T01SYST SECTION.                                              
212100     MOVE 000     TO GOOD-SQLCODECODES                                    
212200     EXEC SQL UPDATE T01SYST                                              
212300                                                                          
212400       SET KDBEHS   = :SYST-KDBEHS                                        
212500                                                                          
212600     END-EXEC                                                             
212700                                                                          
212800     MOVE SQLCODE TO SQLCODE-WS                                           
212900     PERFORM DB2-STATUS-CONTROL                                           
213000     .                                                                    
213100     EJECT                                                                
213200                                                                          
213300 DB2-UPDATE-T01SYST-KDBEHX SECTION.                                       
213400     MOVE 000     TO GOOD-SQLCODECODES                                    
213500     EXEC SQL UPDATE T01SYST                                              
213600                                                                          
213700       SET KDBEHX   = :SYST-KDBEHX                                        
213800                                                                          
213900     END-EXEC                                                             
214000                                                                          
214100     MOVE SQLCODE TO SQLCODE-WS                                           
214200     PERFORM DB2-STATUS-CONTROL                                           
214300     .                                                                    
214400     EJECT                                                                
214500                                                                          
214600 DB2-SELECT-T01TBUN-NONCNTRL SECTION.                                     
214700     MOVE 000100305    TO GOOD-SQLCODECODES                               
214800     EXEC SQL                                                             
214900       SELECT   MIN(IDLEGSEL)                                             
215000                                                                          
215100       INTO    :T01TBUN-IDLEGSEL                                          
215200                                                                          
215300       FROM     T01TBUN                                                   
215400                                                                          
215500       WHERE    FLKNTRL  = :NOO                                           
215600                                                                          
215700     END-EXEC                                                             
215800                                                                          
215900     MOVE SQLCODE      TO SQLCODE-WS                                      
216000     PERFORM DB2-STATUS-CONTROL                                           
216100     .                                                                    
216200     EJECT                                                                
216300                                                                          
216400 DB2-SELECT-T01LSEL SECTION.                                              
216500     MOVE 000100  TO GOOD-SQLCODECODES                                    
216600     EXEC SQL                                                             
216700       SELECT IDLEGSEL                                                    
216800                                                                          
216900       INTO :WS-IDLEGSEL                                                  
217000                                                                          
217100       FROM T01LSEL                                                       
217200                                                                          
217300       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
217400       AND   KDSTATUS = 001                                               
217500     END-EXEC                                                             
217600     MOVE SQLCODE      TO SQLCODE-WS                                      
217700     PERFORM DB2-STATUS-CONTROL                                           
217800     .                                                                    
217900     EJECT                                                                
218000                                                                          
218100 DB2-SELECT-T01SECO SECTION.                                              
218200     MOVE 000100  TO GOOD-SQLCODECODES                                    
218300     EXEC SQL                                                             
218400       SELECT IDLEGSEL                                                    
218500                                                                          
218600       INTO :WS-IDLEGSEL                                                  
218700                                                                          
218800       FROM T01SECO                                                       
218900                                                                          
219000       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
219100       AND   IDLANDX3 = :WS-IDLANDX3-SEND                                 
219200       AND   KDSTATUS = 001                                               
219300       AND   DADELDAT = '00000000'                                        
219400     END-EXEC                                                             
219500     MOVE SQLCODE      TO SQLCODE-WS                                      
219600     PERFORM DB2-STATUS-CONTROL                                           
219700     .                                                                    
219800     EJECT                                                                
219900                                                                          
220000 DB2-SELECT-T01INRE SECTION.                                              
220100     MOVE 000100  TO GOOD-SQLCODECODES                                    
220200     EXEC SQL                                                             
220300       SELECT FLEXPORT                                                    
220400                                                                          
220500       INTO :WS-FLEXPORT                                                  
220600                                                                          
220700       FROM T01INRE                                                       
220800                                                                          
220900       WHERE IDLEGSEL      = :WS-IDLEGSEL                                 
221000       AND   IDLANDX3_SEND = :WS-IDLANDX3-SEND                            
221100       AND   IDLANDX3_REC  = :WS-IDLANDX3-REC                             
221200       AND   KDSTATUS = 001                                               
221300       AND   DADELDAT = '00000000'                                        
221400     END-EXEC                                                             
221500     MOVE SQLCODE      TO SQLCODE-WS                                      
221600     PERFORM DB2-STATUS-CONTROL                                           
221700     .                                                                    
221800     EJECT                                                                
221900                                                                          
222000 DB2-SELECT-T01INRE-REC SECTION.                                          
222100     MOVE 000100  TO GOOD-SQLCODECODES                                    
222200     EXEC SQL                                                             
222300       SELECT FLEXPORT                                                    
222400                                                                          
222500       INTO :WS-FLEXPORT                                                  
222600                                                                          
222700       FROM T01INRE                                                       
222800                                                                          
222900       WHERE IDLEGSEL      = :WS-IDLEGSEL                                 
223000       AND   IDLANDX3_SEND = :WS-IDLANDX3-SEND                            
223100       AND   IDLANDX3_REC  = :WS-IDLANDX3-REC                             
223200       AND   KDSTATUS = 001                                               
223300       AND   DADELDAT = '00000000'                                        
223400     END-EXEC                                                             
223500     MOVE SQLCODE      TO SQLCODE-WS                                      
223600     PERFORM DB2-STATUS-CONTROL                                           
223700     .                                                                    
223800     EJECT                                                                
223900                                                                          
224000 DB2-SELECT-T01RECO SECTION.                                              
224100     MOVE 000100  TO GOOD-SQLCODECODES                                    
224200     EXEC SQL                                                             
224300       SELECT IDLEGSEL                                                    
224400                                                                          
224500       INTO :WS-IDLEGSEL                                                  
224600                                                                          
224700       FROM T01RECO                                                       
224800                                                                          
224900       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
225000       AND   IDLANDX3 = :WS-IDLANDX3-REC                                  
225100       AND   KDSTATUS = 001                                               
225200       AND   DADELDAT = '00000000'                                        
225300     END-EXEC                                                             
225400     MOVE SQLCODE      TO SQLCODE-WS                                      
225500     PERFORM DB2-STATUS-CONTROL                                           
225600     .                                                                    
225700     EJECT                                                                
225800                                                                          
225900 DB2-SELECT-T01FCUS SECTION.                                              
226000     MOVE 000100  TO GOOD-SQLCODECODES                                    
226100     EXEC SQL                                                             
226200       SELECT IDLEGSEL                                                    
226300             ,IDLANDX3                                                    
226400                                                                          
226500       INTO :WS-IDLEGSEL                                                  
226600           ,:WS-IDLANDX3-REC                                              
226700                                                                          
226800       FROM T01FCUS                                                       
226900                                                                          
227000       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
227100       AND   IDPARTNR = :WS-IDPARTNR                                      
227200       AND   KDSTATUS = 001                                               
227300       AND   DADELDAT = '00000000'                                        
227400     END-EXEC                                                             
227500     MOVE SQLCODE      TO SQLCODE-WS                                      
227600     PERFORM DB2-STATUS-CONTROL                                           
227700     .                                                                    
227800     EJECT                                                                
227900                                                                          
228000 DB2-SELECT-T01FCUS-REC SECTION.                                          
228100     MOVE 000100  TO GOOD-SQLCODECODES                                    
228200     EXEC SQL                                                             
228300       SELECT IDLANDX3                                                    
228400             ,KDPARTTY                                                    
228500             ,KDPARTGR                                                    
228600                                                                          
228700       INTO :WS-IDLANDX3-REC                                              
228800           ,:WS-KDPARTTY                                                  
228900           ,:WS-KDPARTGR                                                  
229000                                                                          
229100       FROM T01FCUS                                                       
229200                                                                          
229300       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
229400       AND   IDPARTNR = :WS-IDPARTNR                                      
229500       AND   KDSTATUS = 001                                               
229600       AND   DADELDAT = '00000000'                                        
229700     END-EXEC                                                             
229800     MOVE SQLCODE      TO SQLCODE-WS                                      
229900     PERFORM DB2-STATUS-CONTROL                                           
230000     .                                                                    
230100     EJECT                                                                
230200                                                                          
230300 DB2-SELECT-T01VAT-REC SECTION.                                           
230400     MOVE 000100  TO GOOD-SQLCODECODES                                    
230500     EXEC SQL                                                             
230600       SELECT IDLEGSEL                                                    
230700                                                                          
230800       INTO :WS-IDLEGSEL                                                  
230900                                                                          
231000       FROM T01VAT                                                        
231100                                                                          
231200       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
231300       AND   KDVAT    = :WS-KDVAT                                         
231400       AND   IDLANDX2 = :WS-IDLANDX3-REC                                  
231500       AND   DADELDAT = '00000000'                                        
231600     END-EXEC                                                             
231700     MOVE SQLCODE      TO SQLCODE-WS                                      
231800     PERFORM DB2-STATUS-CONTROL                                           
231900     .                                                                    
232000     EJECT                                                                
232100                                                                          
232200 DB2-SELECT-T01VAT-SEND SECTION.                                          
232300     MOVE 000100  TO GOOD-SQLCODECODES                                    
232400     EXEC SQL                                                             
232500       SELECT IDLEGSEL                                                    
232600                                                                          
232700       INTO :WS-IDLEGSEL                                                  
232800                                                                          
232900       FROM T01VAT                                                        
233000                                                                          
233100       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
233200       AND   KDVAT    = :WS-KDVAT                                         
233300       AND   IDLANDX2 = :WS-IDLANDX3-SEND                                 
233400       AND   DADELDAT = '00000000'                                        
233500     END-EXEC                                                             
233600     MOVE SQLCODE      TO SQLCODE-WS                                      
233700     PERFORM DB2-STATUS-CONTROL                                           
233800     .                                                                    
233900     EJECT                                                                
234000                                                                          
234100 DB2-SELECT-T01CURR SECTION.                                              
234200     MOVE 000100  TO GOOD-SQLCODECODES                                    
234300     EXEC SQL                                                             
234400       SELECT IDLEGSEL                                                    
234500                                                                          
234600       INTO :WS-IDLEGSEL                                                  
234700                                                                          
234800       FROM T01CURR                                                       
234900                                                                          
235000       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
235100       AND   KDVALISO = :WS-KDVALISO                                      
235200       AND   DASTADAT = :WS-DASTADAT                                      
235300     END-EXEC                                                             
235400     MOVE SQLCODE      TO SQLCODE-WS                                      
235500     PERFORM DB2-STATUS-CONTROL                                           
235600     .                                                                    
235700     EJECT                                                                
235800                                                                          
235900 DB2-SELECT-T01CURR-MAX SECTION.                                          
236000     MOVE 000100305  TO GOOD-SQLCODECODES                                 
236100     EXEC SQL                                                             
236200       SELECT MAX(DASTADAT)                                               
236300                                                                          
236400       INTO :WS-DASTADAT                                                  
236500                                                                          
236600       FROM T01CURR                                                       
236700                                                                          
236800       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
236900       AND   KDVALISO = :WS-KDVALISO                                      
237000     END-EXEC                                                             
237100     MOVE SQLCODE      TO SQLCODE-WS                                      
237200     PERFORM DB2-STATUS-CONTROL                                           
237300     .                                                                    
237400     EJECT                                                                
237500                                                                          
237600 DB2-SELECT-T01RECO-FCUS SECTION.                                         
237700     MOVE 000100  TO GOOD-SQLCODECODES                                    
237800     EXEC SQL                                                             
237900       SELECT IDLEGSEL                                                    
238000                                                                          
238100       INTO :WS-IDLEGSEL                                                  
238200                                                                          
238300       FROM T01RECO                                                       
238400                                                                          
238500       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
238600       AND   IDLANDX3 = :WS-IDLANDX3-REC                                  
238700       AND   KDSTATUS = 001                                               
238800       AND   DADELDAT = '00000000'                                        
238900     END-EXEC                                                             
239000     MOVE SQLCODE      TO SQLCODE-WS                                      
239100     PERFORM DB2-STATUS-CONTROL                                           
239200     .                                                                    
239300     EJECT                                                                
239400                                                                          
239500 DB2-SELECT-T01DOTY SECTION.                                              
239600     MOVE 000100  TO GOOD-SQLCODECODES                                    
239700     EXEC SQL                                                             
239800       SELECT IDLEGSEL                                                    
239900                                                                          
240000       INTO :WS-IDLEGSEL                                                  
240100                                                                          
240200       FROM T01DOTY                                                       
240300                                                                          
240400       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
240500       AND   KDFINDOC = :WS-KDFINDOC                                      
240600       AND   KDSTATUS = 001                                               
240700       AND   DADELDAT = '00000000'                                        
240800     END-EXEC                                                             
240900     MOVE SQLCODE      TO SQLCODE-WS                                      
241000     PERFORM DB2-STATUS-CONTROL                                           
241100     .                                                                    
241200     EJECT                                                                
241300                                                                          
241400 DB2-SELECT-T01CUGR SECTION.                                              
241500     MOVE 000100  TO GOOD-SQLCODECODES                                    
241600     EXEC SQL                                                             
241700       SELECT IDLEGSEL                                                    
241800                                                                          
241900       INTO :WS-IDLEGSEL                                                  
242000                                                                          
242100       FROM T01CUGR                                                       
242200                                                                          
242300       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
242400       AND   KDPARTTY = :WS-KDPARTTY                                      
242500       AND   KDPARTGR = :WS-KDPARTGR                                      
242600       AND   KDSTATUS = 001                                               
242700       AND   DADELDAT = '00000000'                                        
242800     END-EXEC                                                             
242900     MOVE SQLCODE      TO SQLCODE-WS                                      
243000     PERFORM DB2-STATUS-CONTROL                                           
243100     .                                                                    
243200     EJECT                                                                
243300                                                                          
243400 DB2-SELECT-T01BURE SECTION.                                              
243500     MOVE 000100  TO GOOD-SQLCODECODES                                    
243600     EXEC SQL                                                             
243700       SELECT IDLEGSEL                                                    
243800                                                                          
243900       INTO :WS-IDLEGSEL                                                  
244000                                                                          
244100       FROM T01BURE                                                       
244200                                                                          
244300       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
244400       AND   KDFINDOC = :WS-KDFINDOC                                      
244500       AND   KDPARTTY = :WS-KDPARTTY                                      
244600       AND   KDPARTGR = :WS-KDPARTGR                                      
244700       AND   DADELDAT = '00000000'                                        
244800       AND   KDSTATUS = 001                                               
244900     END-EXEC                                                             
245000     MOVE SQLCODE      TO SQLCODE-WS                                      
245100     PERFORM DB2-STATUS-CONTROL                                           
245200     .                                                                    
245300     EJECT                                                                
245400                                                                          
245500 DB2-SELECT-DOCNRSERIE-VER3 SECTION.                                      
245600     MOVE 000100  TO GOOD-SQLCODECODES                                    
245700     EXEC SQL                                                             
245800       SELECT B.IDLEGSEL                                                  
245900                                                                          
246000       INTO :WS-IDLEGSEL                                                  
246100                                                                          
246200       FROM T01NSDO A,                                                    
246300            T01TRAW B,                                                    
246400            T01FCUS C,                                                    
246500            T01ASNS D                                                     
246600                                                                          
246700       WHERE B.IDLEGSEL      = :WS-IDLEGSEL                               
246800         AND B.IDBUNDLE      = :WS-IDBUNDLE                               
246900         AND B.DAREGDAT      = :WS-DAREGDAT                               
247000         AND B.TIREGTID      = :WS-TIREGTID                               
247100         AND B.IDREF         = :WS-IDREF                                  
247200         AND B.DAREFDAT      = :WS-DAREFDAT                               
247300         AND B.IDREFRAD      = :WS-IDREFRAD                               
247400         AND C.IDLEGSEL      = B.IDLEGSEL                                 
247500         AND C.IDPARTNR      = B.IDPARTNR                                 
247600         AND C.KDSTATUS      = 001                                        
247700         AND C.DADELDAT      = '00000000'                                 
247800         AND D.IDLEGSEL      = C.IDLEGSEL                                 
247900         AND D.KDFINDOC      = B.KDFINDOC                                 
248000         AND D.KDPARTTY      = C.KDPARTTY                                 
248100         AND D.KDPARTGR      = C.KDPARTGR                                 
248200         AND D.IDLANDX3      = B.IDLANDX3_SEND                            
248300         AND A.IDLEGSEL      = D.IDLEGSEL                                 
248400         AND A.IDLOPNR       = D.IDLOPNR                                  
248500     END-EXEC                                                             
248600     MOVE SQLCODE      TO SQLCODE-WS                                      
248700     PERFORM DB2-STATUS-CONTROL                                           
248800     .                                                                    
248900     EJECT                                                                
249000                                                                          
249100 DB2-SELECT-DOCNRSERIE-VER4 SECTION.                                      
249200     MOVE 000100  TO GOOD-SQLCODECODES                                    
249300     EXEC SQL                                                             
249400       SELECT B.IDLEGSEL                                                  
249500                                                                          
249600       INTO :WS-IDLEGSEL                                                  
249700                                                                          
249800       FROM T01NSDO A,                                                    
249900            T01TRAW B,                                                    
250000            T01FCUS C,                                                    
250100            T01ASNS D                                                     
250200                                                                          
250300       WHERE B.IDLEGSEL      = :WS-IDLEGSEL                               
250400         AND B.IDBUNDLE      = :WS-IDBUNDLE                               
250500         AND B.DAREGDAT      = :WS-DAREGDAT                               
250600         AND B.TIREGTID      = :WS-TIREGTID                               
250700         AND B.IDREF         = :WS-IDREF                                  
250800         AND B.DAREFDAT      = :WS-DAREFDAT                               
250900         AND B.IDREFRAD      = :WS-IDREFRAD                               
251000         AND C.IDLEGSEL      = B.IDLEGSEL                                 
251100         AND C.IDPARTNR      = B.IDPARTNR                                 
251200         AND C.KDSTATUS      = 001                                        
251300         AND C.DADELDAT      = '00000000'                                 
251400         AND D.IDLEGSEL      = C.IDLEGSEL                                 
251500         AND D.KDFINDOC      = B.KDFINDOC                                 
251600         AND D.KDPARTTY      = C.KDPARTTY                                 
251700         AND D.KDPARTGR      = C.KDPARTGR                                 
251800         AND D.IDLANDX3      = :WS-DEFAULT-COUNTRY                        
251900         AND A.IDLEGSEL      = D.IDLEGSEL                                 
252000         AND A.IDLOPNR       = D.IDLOPNR                                  
252100     END-EXEC                                                             
252200     MOVE SQLCODE      TO SQLCODE-WS                                      
252300     PERFORM DB2-STATUS-CONTROL                                           
252400     .                                                                    
252500     EJECT                                                                
252600                                                                          
252700 DB2-STATUS-CONTROL SECTION.                                              
252800     SET SQLCODE-IX TO 1                                                  
252900     SEARCH GOOD-SQLCODE                                                  
253000       AT END                                                             
253100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
253200          DELIMITED BY SIZE INTO ERRORTEXT                                
253300          CALL ABEND USING RKOD-ABEND-DB2                                 
253400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
253500     END-SEARCH                                                           
253600     .                                                                    
253700     EJECT                                                                

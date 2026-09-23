000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF020300.                                                
000400 AUTHOR.         HENRIKSSON ANDERS.                                       
000500 DATE-WRITTEN.   02/02/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    OBS OBS OBS !!!                                                      
000900*    - IF THIS PGM IS ABENDED                                             
001000*      THE FIELD KDBEHX IN TAB T01SYST                                    
001100*      HAS TO BE UPDATED WITH SPACE                                       
001200*    OBS OBS OBS !!!                                                      
001300*                                                                         
001400*    NAME                                                                 
001500*      CARPARTS.BILLIT.INSERT                                             
001600*    FUNCTION:                                                            
001700*    - PGM IS STARTED BY PGM WF020200                                     
001800*      (CARPARTS.BILLIT.VALIDATE)                                         
001900*    - PGM DETERMINES EXECUTION FREQUENCE (WEEK,DAY,NOW)                  
002000*    - PGM TRANSFERS LINES TO THE APPROVED LINE REGISTER                  
002100*    - PGM INSERTS A STARTLINE IN THE APPROVED BUNDLE REGISTER            
002200*                                                                         
002300*      THE PROGRAM READS           ROWS IN TABLE T01TRAW                  
002400*      THE PROGRAM UPDATES         ROWS IN TABLE T01TBUN                  
002500*      THE PROGRAM READS           ROWS IN TABLE T01LSEL                  
002600*      THE PROGRAM READS           ROWS IN TABLE T01FCUS                  
002700*      THE PROGRAM READS           ROWS IN TABLE T01CUGR                  
002800*      THE PROGRAM READS           ROWS IN TABLE T01INRE                  
002900*      THE PROGRAM UPDATES         ROWS IN TABLE T01SYST                  
003000*      THE PROGRAM INSERTS         ROWS IN TABLE T01ALIN                  
003100*      THE PROGRAM INSERTS         ROWS IN TABLE T01ABUN                  
003200*                                                                         
003300*    INDATA.                                                              
003400*      TRANSAKTION: WF0203X                                               
003500*      REQUEST:     HEADER ONLY                                           
003600*                                                                         
003700                                                                          
003800     SKIP3                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000                                                                          
004100 DATA DIVISION.                                                           
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400 77  IDPGM                       PIC X(08)  VALUE 'WF020300'.             
004500                                                                          
004600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND                
004700 77  ERRORTEXT                   PIC X(80)  VALUE SPACE.                  
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900                                                                          
005000 77  YES                         PIC X      VALUE 'J'.                    
005100 77  NOO                         PIC X      VALUE 'N'.                    
005200                                                                          
005300 77  LINES-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005400 77  RESTART-IX                  PIC S9(9) VALUE +0    COMP SYNC.         
005500 77  MAX-LINES                   PIC S9(9) VALUE +500  COMP SYNC.         
005600                                                                          
005700 77  WS-BUNDLES-NOT-CONTROLLED   PIC X      VALUE 'N'.                    
005800     EJECT                                                                
005900                                                                          
006000*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006300     03  WZ01SEND                PIC X(8)   VALUE 'WZ01SEND'.             
006400     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
006500     SKIP3                                                                
006600*    --- PARAMETERS TO ABEND                                              
006700                                                                          
006800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
007100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007200     EJECT                                                                
007300*                                                                         
007400 01  FILLER                      PIC X(16)   VALUE 'RECV-CONTROL'.        
007500     SKIP3                                                                
007600 01  -COPY WZ01RECV                                                       
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
007900     SKIP3                                                                
008000 01  RECV-AREA.                                                           
008100*    03  -COPY WZ01REQU                                                   
008200     EJECT                                                                
008300                                                                          
008400 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
008500 01  -COPY WZ01SEND                                                       
008600     EJECT                                                                
008700                                                                          
008800 01  SEND-AREA.                                                           
008900*    03  -COPY WZ01REQU                                                   
009000                                                                          
009100 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
009200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009300                                                                          
009400 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
009500 01  DB2-WS.                                                              
009600     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
009700         88  CURSOR-OK                       VALUE 000.                   
009800         88  LINES-FINNS                     VALUE 000.                   
009900         88  LINES-FOUND                     VALUE 000.                   
010000         88  LINES-MISSING                   VALUE 100.                   
010100         88  DUPLICATE-LINES                 VALUE 811.                   
010200         88  RESOURCE-ERROR                  VALUE 904.                   
010300                                                                          
010400     03  GOOD-SQLCODECODES.                                               
010500         05  GOOD-SQLCODE OCCURS 5                                        
010600             INDEXED BY SQLCODE-IX PIC 9(3).                              
010700     EJECT                                                                
010800                                                                          
010900 01  FILLER                    PIC X(16) VALUE 'WS-AREA         '.        
011000 01  WS-AREA.                                                             
011100     03 WS-DASTADAT            PIC X(8)  VALUE SPACE.                     
011200     03 WS-IDLANDX3-REC        PIC X(3)  VALUE SPACE.                     
011300     03 WS-IDFELKOD            PIC X(3)  VALUE SPACE.                     
011400     03 WS-ERRORTEXT           PIC X(50) VALUE SPACE.                     
011500     03 WS-IDREFRAD            PIC S9(5)  VALUE ZERO COMP-3.              
011600                                                                          
011700 01  WS-DIVERSE-MULTIFETCH.                                               
011800     03 WS-MX                    PIC S9(3)  COMP-3.                       
011900     03 WS-MULTIFETCH            PIC S9(3)  COMP-3.                       
012000     03 WS-IDLEGSEL              PIC X(4).                                
012100     03 WS-IDPARTNR              PIC X(9).                                
012200     03 WS-IDLANDX3-SEND         PIC X(3).                                
012300     03 WS-BELEVVIL              PIC X(35).                               
012400                                                                          
012500     03 WS-ALIN-DASTADAT OCCURS 100  PIC X(8).                            
012600     03 WS-ALIN-IDREFRAD OCCURS 100  PIC S9(5) COMP-3.                    
012700     EJECT                                                                
012800                                                                          
012900 01  FILLER                    PIC X(16)    VALUE 'T01TRAW-AREA'.         
013000*01  -COPY T01TRAW -PRE T01TRAW-                                          
013100     EJECT                                                                
013200                                                                          
013300 01  FILLER                    PIC X(16)    VALUE 'TRAW-AREA'.            
013400*01  -COPY T01TRAWT -PRE TRAW-                                            
013500     EJECT                                                                
013600                                                                          
013700 01  FILLER                    PIC X(16)    VALUE 'T01TBUN-AREA'.         
013800*01  -COPY T01TBUN -PRE T01TBUN-                                          
013900     EJECT                                                                
014000                                                                          
014100 01  FILLER                    PIC X(16)    VALUE 'T01ALIN-AREA'.         
014200*01  -COPY T01ALINT -PRE T01ALIN-                                         
014300     EJECT                                                                
014400                                                                          
014500 01  FILLER                    PIC X(16)    VALUE 'T01ABUN-AREA'.         
014600*01  -COPY T01ABUN -PRE T01ABUN-                                          
014700     EJECT                                                                
014800                                                                          
014900 01  FILLER                    PIC X(16)    VALUE 'ABUN-AREA'.            
015000*01  -COPY T01ABUN -PRE ABUN-                                             
015100     EJECT                                                                
015200                                                                          
015300 01  FILLER                    PIC X(16)    VALUE 'T01LSEL-AREA'.         
015400*01  -COPY T01LSEL -PRE T01LSEL-                                          
015500     EJECT                                                                
015600                                                                          
015700 01  FILLER                    PIC X(16)    VALUE 'T01CUGR-AREA'.         
015800*01  -COPY T01CUGR -PRE T01CUGR-                                          
015900     EJECT                                                                
016000                                                                          
016100 01  FILLER                    PIC X(16)    VALUE 'T01FCUS-AREA'.         
016200*01  -COPY T01FCUS -PRE T01FCUS-                                          
016300     EJECT                                                                
016400                                                                          
016500 01  FILLER                    PIC X(16)    VALUE 'T01INRE-AREA'.         
016600*01  -COPY T01INRE -PRE T01INRE-                                          
016700     EJECT                                                                
016800                                                                          
016900 01  FILLER                    PIC X(16)    VALUE 'T01SYST-AREA'.         
017000*01  -COPY T01SYST -PRE T01SYST-                                          
017100     EJECT                                                                
017200                                                                          
017300 01  FILLER                    PIC X(16)    VALUE 'SYST-AREA'.            
017400*01  -COPY T01SYST -PRE SYST-                                             
017500     EJECT                                                                
017600                                                                          
017700     EXEC SQL INCLUDE T01TRAW END-EXEC.                                   
017800     EJECT                                                                
017900     EXEC SQL INCLUDE T01TBUN END-EXEC.                                   
018000     EJECT                                                                
018100     EXEC SQL INCLUDE T01ALIN END-EXEC.                                   
018200     EJECT                                                                
018300     EXEC SQL INCLUDE T01ABUN END-EXEC.                                   
018400     EJECT                                                                
018500     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
018600     EJECT                                                                
018700     EXEC SQL INCLUDE T01CUGR END-EXEC.                                   
018800     EJECT                                                                
018900     EXEC SQL INCLUDE T01FCUS END-EXEC.                                   
019000     EJECT                                                                
019100     EXEC SQL INCLUDE T01INRE END-EXEC.                                   
019200     EJECT                                                                
019300     EXEC SQL INCLUDE T01SYST END-EXEC.                                   
019400     EJECT                                                                
019500                                                                          
019600 LINKAGE SECTION.                                                         
019700 PROCEDURE DIVISION.                                                      
019800 MAIN SECTION.                                                            
019900     PERFORM A-INIT                                                       
020000                                                                          
020100     PERFORM DB2-LOCK-T01ABUN                                             
020200     PERFORM DB2-LOCK-T01ALIN                                             
020300     PERFORM DB2-OPEN-T01TBUN-CRS                                         
020400     PERFORM DB2-FETCH-T01TBUN-CRS                                        
020500     PERFORM UNTIL LINES-MISSING OR RESTART-IX > MAX-LINES                
020600       PERFORM B-INSERT-T01ALIN                                           
020700       PERFORM C-INSERT-T01ABUN                                           
020800       PERFORM DB2-UPDATE-T01TBUN                                         
020900       PERFORM DB2-FETCH-T01TBUN-CRS                                      
021000     END-PERFORM                                                          
021100                                                                          
021200     PERFORM DB2-CLOSE-T01TBUN-CRS                                        
021300                                                                          
021400* CHECKS IF NEW NON-FINISHED BUNDLES EXIST                                
021500     PERFORM I-SELECT-T01TBUN-NONCNTRL                                    
021600     IF WS-BUNDLES-NOT-CONTROLLED = YES                                   
021700       MOVE 'R' TO SYST-KDBEHX                                            
021800       PERFORM DB2-UPDATE-T01SYST                                         
021900       PERFORM D-RESTART-OF-OWN-TRANS                                     
022000     ELSE                                                                 
022100       MOVE SPACE TO SYST-KDBEHX                                          
022200       PERFORM DB2-UPDATE-T01SYST                                         
022300     END-IF                                                               
022400                                                                          
022500     MOVE ZERO TO RETURN-CODE                                             
022600     GOBACK                                                               
022700     .                                                                    
022800     EJECT                                                                
022900                                                                          
023000 A-INIT SECTION.                                                          
023100     PERFORM S01-READ-OPEN                                                
023200     PERFORM S02-READ-MESSAGE                                             
023300     IF RECV-KDRC = ZERO                                                  
023400       PERFORM S03-READ-CLOSE                                             
023500       MOVE '00000000'            TO WS-DASTADAT                          
023600       INITIALIZE GOOD-SQLCODECODES                                       
023700       MOVE ZERO TO RESTART-IX                                            
023800     ELSE                                                                 
023900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
024000     END-IF                                                               
024100                                                                          
024200     PERFORM DB2-SELECT-T01SYST                                           
024300     IF SYST-KDBEHX > SPACE                                               
024400* THIS PROCESS WAS RUNNING, RESTART OR START AFTER ABEND                  
024500       CONTINUE                                                           
024600     ELSE                                                                 
024700* PREPARING ITSELF FOR RESTART                                            
024800       MOVE 'F' TO SYST-KDBEHX                                            
024900       PERFORM DB2-UPDATE-T01SYST                                         
025000     END-IF                                                               
025100                                                                          
025200     .                                                                    
025300     EJECT                                                                
025400                                                                          
025500 B-INSERT-T01ALIN SECTION.                                                
025600     PERFORM DB2-OPEN-T01TRAW-CRS                                         
025700     PERFORM DB2-FETCH-T01TRAW-CRS                                        
025800     IF SQLERRD(3) > 0                                                    
025900       MOVE 000     TO SQLCODE-WS                                         
026000     END-IF                                                               
026100     PERFORM UNTIL LINES-MISSING                                          
026200       MOVE SQLERRD(3) TO WS-MULTIFETCH                                   
026300       MOVE ZERO       TO WS-MX                                           
026400       PERFORM UNTIL WS-MX = WS-MULTIFETCH                                
026500         ADD +1        TO WS-MX                                           
026600         MOVE TRAW-IDLEGSEL(WS-MX)      TO WS-IDLEGSEL                    
026700         MOVE TRAW-IDPARTNR(WS-MX)      TO WS-IDPARTNR                    
026800         MOVE TRAW-IDLANDX3-SEND(WS-MX) TO WS-IDLANDX3-SEND               
026900         MOVE WS-ALIN-IDREFRAD(WS-MX)   TO WS-IDREFRAD                    
027000         MOVE WS-DASTADAT               TO WS-ALIN-DASTADAT(WS-MX)        
027100         MOVE WS-IDREFRAD               TO WS-ALIN-IDREFRAD(WS-MX)        
027200         PERFORM BA-DESIDE-FREQUENCE                                      
027300         PERFORM BB-ADJUST-FOR-VSS                                        
027400         PERFORM BC-ADJUST-FOR-SAR                                        
027500         PERFORM BD-ADJUST-FOR-W612                                       
027510         PERFORM BE-ADJUST-FOR-W418                                       
027520         PERFORM BF-ADJUST-FOR-W41-VAT                                    
027530         PERFORM BG-ADJUST-FOR-NI-VAT                                     
027600       END-PERFORM                                                        
027700       PERFORM DB2-INSERT-T01ALIN                                         
027800       IF WS-MULTIFETCH = 100                                             
027900         PERFORM DB2-FETCH-T01TRAW-CRS                                    
028000         IF SQLERRD(3) > 0                                                
028100           MOVE 000     TO SQLCODE-WS                                     
028200         END-IF                                                           
028300       ELSE                                                               
028400         MOVE 100 TO SQLCODE-WS                                           
028500       END-IF                                                             
028600     END-PERFORM                                                          
028700     PERFORM DB2-CLOSE-T01TRAW-CRS                                        
028800     .                                                                    
028900     EJECT                                                                
029000                                                                          
029100 BA-DESIDE-FREQUENCE SECTION.                                             
029200     IF TRAW-KDINVFRQ(WS-MX) = SPACE                                      
029300       PERFORM DB2-SELECT-T01FCUS                                         
029400       PERFORM DB2-SELECT-T01INRE                                         
029500       IF T01INRE-FLEXPORT = YES                                          
029600         MOVE 'NOW'         TO TRAW-KDINVFRQ(WS-MX)                       
029700       END-IF                                                             
029800     END-IF                                                               
029900                                                                          
030000     IF TRAW-KDINVFRQ(WS-MX) = SPACE                                      
030100       PERFORM DB2-SELECT-T01FCUS                                         
030200       PERFORM DB2-SELECT-T01CUGR                                         
030300       IF T01CUGR-KDINVFRQ = SPACE                                        
030400         PERFORM DB2-SELECT-T01LSEL                                       
030500         MOVE T01LSEL-KDINVFRQ TO TRAW-KDINVFRQ(WS-MX)                    
030600       ELSE                                                               
030700         MOVE T01CUGR-KDINVFRQ TO TRAW-KDINVFRQ(WS-MX)                    
030800       END-IF                                                             
030900     END-IF                                                               
031000     .                                                                    
031100     EJECT                                                                
031200                                                                          
031300 BB-ADJUST-FOR-VSS SECTION.                                               
031400     IF TRAW-IDSYSTEM-SEND(WS-MX) = 'VSS'                                 
031500       MOVE TRAW-BEANST(WS-MX)  TO WS-BELEVVIL(1:25)                      
031600       MOVE TRAW-IDUSER(WS-MX)  TO WS-BELEVVIL(26:10)                     
031700       MOVE WS-BELEVVIL         TO TRAW-BELEVVIL(WS-MX)                   
031800       MOVE SPACE               TO TRAW-BEANST(WS-MX)                     
031900       MOVE SPACE               TO TRAW-IDUSER(WS-MX)                     
032000       MOVE TRAW-KVBEART(WS-MX) TO TRAW-KVLEVART(WS-MX)                   
032010       MOVE TRAW-IDDC(WS-MX)    TO TRAW-IDEXCUST-3(WS-MX)                 
032020       MOVE SPACE               TO TRAW-IDDC(WS-MX)                       
032100     END-IF                                                               
032200     .                                                                    
032300     EJECT                                                                
032400                                                                          
032500 BC-ADJUST-FOR-SAR SECTION.                                               
032600     IF TRAW-IDSYSTEM-SEND(WS-MX) = 'SAR'                                 
032700       IF TRAW-KDANMORS(WS-MX) = '01'                                     
032800         COMPUTE TRAW-PRARTBTO(WS-MX) = TRAW-PRARTBTO(WS-MX) * -1         
032900         COMPUTE TRAW-PRARTNTO(WS-MX) = TRAW-PRARTNTO(WS-MX) * -1         
033000       END-IF                                                             
033100     END-IF                                                               
033200     .                                                                    
033300     EJECT                                                                
033400                                                                          
033500 BD-ADJUST-FOR-W612 SECTION.                                              
033600     IF TRAW-IDSYSTEM-SEND(WS-MX) = 'W612'                                
033700       IF TRAW-KDANMORS(WS-MX) = '00'                                     
033800         COMPUTE TRAW-PRARTBTO(WS-MX) = TRAW-PRARTBTO(WS-MX) * -1         
033900         COMPUTE TRAW-PRARTNTO(WS-MX) = TRAW-PRARTNTO(WS-MX) * -1         
034000       END-IF                                                             
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400                                                                          
034500 BE-ADJUST-FOR-W418 SECTION.                                              
034510     IF  TRAW-IDSYSTEM-SEND(WS-MX) = 'W418'                               
034511     AND TRAW-KDFINDOC(WS-MX) = 'CLA'                                     
034520       IF TRAW-KDANMORS(WS-MX) = '74'                                     
034530         COMPUTE TRAW-PRARTBTO(WS-MX) = TRAW-PRARTBTO(WS-MX) * -1         
034540         COMPUTE TRAW-PRARTNTO(WS-MX) = TRAW-PRARTNTO(WS-MX) * -1         
034550       END-IF                                                             
034560     END-IF                                                               
034570     .                                                                    
034580     EJECT                                                                
034590                                                                          
034591 BF-ADJUST-FOR-W41-VAT SECTION.                                           
034592     IF  TRAW-IDSYSTEM-SEND(WS-MX) = 'W418'                               
034593     OR  TRAW-IDSYSTEM-SEND(WS-MX) = 'W41X'                               
034594       IF TRAW-IDDC(WS-MX) = '24'                                         
034595         IF TRAW-KDVAT(WS-MX) = 'S2'                                      
034596           MOVE 'S4' TO TRAW-KDVAT(WS-MX)                                 
034598         END-IF                                                           
034599       END-IF                                                             
034600     END-IF                                                               
034601     .                                                                    
034602     EJECT                                                                
034603                                                                          
034604 BG-ADJUST-FOR-NI-VAT SECTION.                                            
034605                                                                          
034606**** NORTHERN IRELAND / TIS CUSTOMERS                                     
034607     IF (TRAW-IDEXCUST-1(WS-MX) = '1378'                                  
034608     AND TRAW-IDEXCUST-2(WS-MX) = '22004')                                
034609     OR  TRAW-IDEXCUST-1(WS-MX) = '11822004'                              
034610       IF TRAW-KDVAT(WS-MX) = '90'                                        
034611         MOVE '70' TO TRAW-KDVAT(WS-MX)                                   
034612       END-IF                                                             
034613     END-IF                                                               
034614     .                                                                    
034615     EJECT                                                                
034616                                                                          
034620 C-INSERT-T01ABUN SECTION.                                                
034700     PERFORM DB2-OPEN-CRS-NEW-ABUN                                        
034800     PERFORM DB2-FETCH-CRS-NEW-ABUN                                       
034900     PERFORM UNTIL LINES-MISSING                                          
035000       PERFORM DB2-INSERT-ABUN                                            
035100       PERFORM DB2-FETCH-CRS-NEW-ABUN                                     
035200     END-PERFORM                                                          
035300     PERFORM DB2-CLOSE-CRS-NEW-ABUN                                       
035400     .                                                                    
035500     EJECT                                                                
035600                                                                          
035700 D-RESTART-OF-OWN-TRANS SECTION.                                          
035800     PERFORM S04-SEND-TO-WF0203-OPEN                                      
035900     PERFORM S05-SEND-TO-WF0203-PUT                                       
036000     PERFORM S06-SEND-TO-WF0203-CLOSE                                     
036100     .                                                                    
036200     EJECT                                                                
036300                                                                          
036400 I-SELECT-T01TBUN-NONCNTRL SECTION.                                       
036500     PERFORM DB2-SELECT-T01TBUN-NONCNTRL                                  
036600     IF LINES-FOUND                                                       
036700       MOVE YES   TO WS-BUNDLES-NOT-CONTROLLED                            
036800     ELSE                                                                 
036900       MOVE NOO   TO WS-BUNDLES-NOT-CONTROLLED                            
037000     END-IF                                                               
037100     .                                                                    
037200     EJECT                                                                
037300                                                                          
037400*    --- DISPATCHER-SECTIONS                                              
037500 S01-READ-OPEN SECTION.                                                   
037600                                                                          
037700     MOVE 'OPEN'                   TO RECV-KDFUNC                         
037800     MOVE 'CARPARTS.BILLIT.INSERT' TO RECV-ADDISPABS                      
037900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
038000                         RECV-OPEN-AREA                                   
038100     IF RECV-KDRC > ZERO                                                  
038200       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
038300       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
038400       DELIMITED BY SIZE INTO ERRORTEXT                                   
038500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
038600     END-IF                                                               
038700     .                                                                    
038800     SKIP3                                                                
038900                                                                          
039000 S02-READ-MESSAGE SECTION.                                                
039100                                                                          
039200     MOVE 'GET'                            TO RECV-KDFUNC                 
039300     MOVE LENGTH OF RECV-AREA              TO RECV-KVDLEN                 
039400     CALL WZ01RECV USING RECV-CONTROL-AREA                                
039500                         RECV-KVDLEN                                      
039600                         RECV-AREA                                        
039700     IF RECV-KDRC > 1                                                     
039800       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
039900       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
040000       DELIMITED BY SIZE INTO ERRORTEXT                                   
040100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
040200     END-IF                                                               
040300     .                                                                    
040400     SKIP3                                                                
040500                                                                          
040600 S03-READ-CLOSE SECTION.                                                  
040700                                                                          
040800     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
040900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
041000                                                                          
041100     IF RECV-KDRC > 0                                                     
041200       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
041300       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
041400       DELIMITED BY SIZE INTO ERRORTEXT                                   
041500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
041600     END-IF                                                               
041700     .                                                                    
041800     EJECT                                                                
041900                                                                          
042000 S04-SEND-TO-WF0203-OPEN SECTION.                                         
042100     MOVE 'CARPARTS.BILLIT.INSERT'   TO SEND-ADDISPABS                    
042200     MOVE 'OPEN'                     TO SEND-KDFUNC                       
042300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
042400                         SEND-OPEN-AREA                                   
042500     IF SEND-KDRC > ZERO                                                  
042600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
042700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
042800       DELIMITED BY SIZE INTO ERRORTEXT                                   
042900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
043000     END-IF                                                               
043100     .                                                                    
043200     EJECT                                                                
043300                                                                          
043400 S05-SEND-TO-WF0203-PUT SECTION.                                          
043500     MOVE 'PUT'                            TO SEND-KDFUNC                 
043600     MOVE LENGTH OF SEND-AREA              TO SEND-KVDLEN                 
043700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
043800                         SEND-KVDLEN                                      
043900                         SEND-AREA                                        
044000     IF SEND-KDRC > 1                                                     
044100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
044200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
044300       DELIMITED BY SIZE INTO ERRORTEXT                                   
044400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800                                                                          
044900 S06-SEND-TO-WF0203-CLOSE SECTION.                                        
045000     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
045100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
045200                                                                          
045300     IF SEND-KDRC > 0                                                     
045400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
045500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
045600       DELIMITED BY SIZE INTO ERRORTEXT                                   
045700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100                                                                          
046200 DB2-LOCK-T01ABUN SECTION.                                                
046300     MOVE 000     TO GOOD-SQLCODECODES                                    
046400                                                                          
046500     EXEC SQL                                                             
046600       LOCK TABLE T01ABUN                                                 
046700       IN EXCLUSIVE MODE                                                  
046800     END-EXEC                                                             
046900     .                                                                    
047000                                                                          
047100 DB2-LOCK-T01ALIN SECTION.                                                
047200     MOVE 000     TO GOOD-SQLCODECODES                                    
047300                                                                          
047400     EXEC SQL                                                             
047500       LOCK TABLE T01ALIN                                                 
047600       IN EXCLUSIVE MODE                                                  
047700     END-EXEC                                                             
047800     .                                                                    
047900     EJECT                                                                
048000                                                                          
048100 DB2-OPEN-T01TBUN-CRS SECTION.                                            
048200     MOVE 000100  TO GOOD-SQLCODECODES                                    
048300     EXEC SQL DECLARE T01TBUN-CRS CURSOR FOR                              
048400       SELECT IDLEGSEL,                                                   
048500              IDBUNDLE,                                                   
048600              DAREGDAT,                                                   
048700              TIREGTID                                                    
048800                                                                          
048900       FROM   T01TBUN                                                     
049000                                                                          
049100       WHERE  FLFEL   = :NOO                                              
049200       AND    FLKNTRL = :YES                                              
049300       AND    FLKLAR  = :NOO                                              
049400                                                                          
049500       FOR UPDATE OF FLKLAR                                               
049600     END-EXEC                                                             
049700                                                                          
049800     MOVE 000100       TO GOOD-SQLCODECODES                               
049900     EXEC SQL OPEN T01TBUN-CRS END-EXEC                                   
050000     .                                                                    
050100     EJECT                                                                
050200                                                                          
050300 DB2-FETCH-T01TBUN-CRS SECTION.                                           
050400     MOVE 000100       TO GOOD-SQLCODECODES                               
050500     EXEC SQL FETCH T01TBUN-CRS INTO                                      
050600       :T01TBUN-IDLEGSEL,                                                 
050700       :T01TBUN-IDBUNDLE,                                                 
050800       :T01TBUN-DAREGDAT,                                                 
050900       :T01TBUN-TIREGTID                                                  
051000     END-EXEC                                                             
051100                                                                          
051200     MOVE SQLCODE      TO SQLCODE-WS                                      
051300     PERFORM DB2-STATUS-CONTROL                                           
051400     .                                                                    
051500     EJECT                                                                
051600                                                                          
051700 DB2-CLOSE-T01TBUN-CRS SECTION.                                           
051800     EXEC SQL CLOSE T01TBUN-CRS END-EXEC                                  
051900     .                                                                    
052000     EJECT                                                                
052100                                                                          
052200 DB2-UPDATE-T01TBUN SECTION.                                              
052300     MOVE 000     TO GOOD-SQLCODECODES                                    
052400     EXEC SQL UPDATE T01TBUN                                              
052500       SET FLKLAR   = :YES                                                
052600                                                                          
052700       WHERE CURRENT OF T01TBUN-CRS                                       
052800     END-EXEC                                                             
052900                                                                          
053000     ADD 1        TO RESTART-IX                                           
053100     MOVE SQLCODE TO SQLCODE-WS                                           
053200     PERFORM DB2-STATUS-CONTROL                                           
053300     .                                                                    
053400     EJECT                                                                
053500                                                                          
053600 DB2-OPEN-T01TRAW-CRS SECTION.                                            
053700     MOVE 000100  TO GOOD-SQLCODECODES                                    
053800     EXEC SQL                                                             
053900       DECLARE T01TRAW-CRS CURSOR WITH ROWSET POSITIONING FOR             
054000       SELECT IDLEGSEL                                                    
054100             ,IDBUNDLE                                                    
054200             ,DAREGDAT                                                    
054300             ,TIREGTID                                                    
054400             ,IDREF                                                       
054500             ,DAREFDAT                                                    
054600             ,IDREFRAD                                                    
054700             ,BEVOLREF                                                    
054800             ,IDLANDX3_SEND                                               
054900             ,IDLANDX3_REC                                                
055000             ,IDLEVNR                                                     
055100             ,IDPARTNR                                                    
055200             ,IDEXCUST_1                                                  
055300             ,IDEXCUST_2                                                  
055400             ,IDEXCUST_3                                                  
055500             ,IDOPTION_1                                                  
055600             ,IDOPTION_2                                                  
055700             ,IDOPTION_3                                                  
055800             ,IDOPTION_4                                                  
055900             ,IDOPTION_5                                                  
056000             ,IDAPPEND                                                    
056100             ,IDARTNR_FINANCE                                             
056200             ,IDSTATNR                                                    
056300             ,VKORDBTO_KOLLI                                              
056400             ,VKARTNTO                                                    
056500             ,PRARTBTO                                                    
056600             ,PRARTNTO                                                    
056700             ,REARTRAB                                                    
056800             ,KVBEART                                                     
056900             ,KVLEVART                                                    
057000             ,BEART                                                       
057100             ,FLSOFT                                                      
057200             ,FLSPECPR                                                    
057300             ,FLFREE                                                      
057400             ,FLPRIV                                                      
057500             ,KDVAT                                                       
057600             ,KDVALISO                                                    
057700             ,KDINVFRQ                                                    
057800             ,KDFINDOC                                                    
057900             ,IDBREAK_1                                                   
058000             ,IDBREAK_2                                                   
058100             ,IDSEQ_1                                                     
058200             ,IDSEQ_2                                                     
058300             ,IDSEQ_3                                                     
058400             ,KDARTURS                                                    
058500             ,KDANMORS                                                    
058600             ,IDFAKREF                                                    
058700             ,DAFAKREF                                                    
058800             ,IDDC                                                        
058900             ,KDFRAKT                                                     
059000             ,BELEVVIL                                                    
059100             ,IDACCNT_1                                                   
059200             ,IDACCNT_2                                                   
059300             ,IDACCNT_3                                                   
059400             ,IDACCNT_4                                                   
059500             ,IDSYSTEM_SEND                                               
059600             ,IDSYSTEM_REC                                                
059700             ,BEANST                                                      
059800             ,IDUSER                                                      
059900             ,BETEXT                                                      
060000             ,BETEXT_CRE                                                  
060100             ,IDARTNR_CNTRL                                               
060110             ,FLPCOO                                                      
060120             ,IDLEVNR_ART                                                 
060130             ,IDTRACK_1                                                   
060140             ,KVANT_TRACK_1                                               
060150             ,IDTRACK_2                                                   
060160             ,KVANT_TRACK_2                                               
060170             ,IDTRACK_3                                                   
060180             ,KVANT_TRACK_3                                               
060190             ,IDTRACK_4                                                   
060191             ,KVANT_TRACK_4                                               
060192             ,IDTRACK_5                                                   
060193             ,KVANT_TRACK_5                                               
060194             ,KDPRMOD                                                     
060200                                                                          
060300       FROM T01TRAW                                                       
060400                                                                          
060500       WHERE IDLEGSEL = :T01TBUN-IDLEGSEL                                 
060600       AND   IDBUNDLE = :T01TBUN-IDBUNDLE                                 
060700       AND   DAREGDAT = :T01TBUN-DAREGDAT                                 
060800       AND   TIREGTID = :T01TBUN-TIREGTID                                 
060900     END-EXEC                                                             
061000                                                                          
061100     MOVE 000100       TO GOOD-SQLCODECODES                               
061200     EXEC SQL OPEN T01TRAW-CRS END-EXEC                                   
061300     .                                                                    
061400     EJECT                                                                
061500                                                                          
061600 DB2-FETCH-T01TRAW-CRS SECTION.                                           
061700     MOVE 000100       TO GOOD-SQLCODECODES                               
061800     EXEC SQL                                                             
061900       FETCH NEXT ROWSET FROM T01TRAW-CRS FOR 100 ROWS                    
062000       INTO :TRAW-IDLEGSEL                                                
062100           ,:TRAW-IDBUNDLE                                                
062200           ,:TRAW-DAREGDAT                                                
062300           ,:TRAW-TIREGTID                                                
062400           ,:TRAW-IDREF                                                   
062500           ,:TRAW-DAREFDAT                                                
062600           ,:WS-ALIN-IDREFRAD                                             
062700           ,:TRAW-BEVOLREF                                                
062800           ,:TRAW-IDLANDX3-SEND                                           
062900           ,:TRAW-IDLANDX3-REC                                            
063000           ,:TRAW-IDLEVNR                                                 
063100           ,:TRAW-IDPARTNR                                                
063200           ,:TRAW-IDEXCUST-1                                              
063300           ,:TRAW-IDEXCUST-2                                              
063400           ,:TRAW-IDEXCUST-3                                              
063500           ,:TRAW-IDOPTION-1                                              
063600           ,:TRAW-IDOPTION-2                                              
063700           ,:TRAW-IDOPTION-3                                              
063800           ,:TRAW-IDOPTION-4                                              
063900           ,:TRAW-IDOPTION-5                                              
064000           ,:TRAW-IDAPPEND                                                
064100           ,:TRAW-IDARTNR-FINANCE                                         
064200           ,:TRAW-IDSTATNR                                                
064300           ,:TRAW-VKORDBTO-KOLLI                                          
064400           ,:TRAW-VKARTNTO                                                
064500           ,:TRAW-PRARTBTO                                                
064600           ,:TRAW-PRARTNTO                                                
064700           ,:TRAW-REARTRAB                                                
064800           ,:TRAW-KVBEART                                                 
064900           ,:TRAW-KVLEVART                                                
065000           ,:TRAW-BEART                                                   
065100           ,:TRAW-FLSOFT                                                  
065200           ,:TRAW-FLSPECPR                                                
065300           ,:TRAW-FLFREE                                                  
065400           ,:TRAW-FLPRIV                                                  
065500           ,:TRAW-KDVAT                                                   
065600           ,:TRAW-KDVALISO                                                
065700           ,:TRAW-KDINVFRQ                                                
065800           ,:TRAW-KDFINDOC                                                
065900           ,:TRAW-IDBREAK-1                                               
066000           ,:TRAW-IDBREAK-2                                               
066100           ,:TRAW-IDSEQ-1                                                 
066200           ,:TRAW-IDSEQ-2                                                 
066300           ,:TRAW-IDSEQ-3                                                 
066400           ,:TRAW-KDARTURS                                                
066500           ,:TRAW-KDANMORS                                                
066600           ,:TRAW-IDFAKREF                                                
066700           ,:TRAW-DAFAKREF                                                
066800           ,:TRAW-IDDC                                                    
066900           ,:TRAW-KDFRAKT                                                 
067000           ,:TRAW-BELEVVIL                                                
067100           ,:TRAW-IDACCNT-1                                               
067200           ,:TRAW-IDACCNT-2                                               
067300           ,:TRAW-IDACCNT-3                                               
067400           ,:TRAW-IDACCNT-4                                               
067500           ,:TRAW-IDSYSTEM-SEND                                           
067600           ,:TRAW-IDSYSTEM-REC                                            
067700           ,:TRAW-BEANST                                                  
067800           ,:TRAW-IDUSER                                                  
067900           ,:TRAW-BETEXT                                                  
068000           ,:TRAW-BETEXT-CRE                                              
068100           ,:TRAW-IDARTNR-CNTRL                                           
068110           ,:TRAW-FLPCOO                                                  
068120           ,:TRAW-IDLEVNR-ART                                             
068130           ,:TRAW-IDTRACK-1                                               
068140           ,:TRAW-KVANT-TRACK-1                                           
068150           ,:TRAW-IDTRACK-2                                               
068160           ,:TRAW-KVANT-TRACK-2                                           
068170           ,:TRAW-IDTRACK-3                                               
068180           ,:TRAW-KVANT-TRACK-3                                           
068190           ,:TRAW-IDTRACK-4                                               
068191           ,:TRAW-KVANT-TRACK-4                                           
068192           ,:TRAW-IDTRACK-5                                               
068193           ,:TRAW-KVANT-TRACK-5                                           
068194           ,:TRAW-KDPRMOD                                                 
068200     END-EXEC                                                             
068300     MOVE SQLCODE      TO SQLCODE-WS                                      
068400     PERFORM DB2-STATUS-CONTROL                                           
068500     .                                                                    
068600     EJECT                                                                
068700                                                                          
068800 DB2-CLOSE-T01TRAW-CRS SECTION.                                           
068900     SKIP2                                                                
069000     EXEC SQL CLOSE T01TRAW-CRS END-EXEC                                  
069100     .                                                                    
069200     EJECT                                                                
069300                                                                          
069400 DB2-INSERT-T01ALIN SECTION.                                              
069500     SKIP2                                                                
069600     MOVE 000   TO GOOD-SQLCODECODES                                      
069700     EXEC SQL                                                             
069800         INSERT INTO T01ALIN                                              
069900         (                                                                
070000          IDLEGSEL                                                        
070100         ,IDBUNDLE                                                        
070200         ,DAREGDAT                                                        
070300         ,TIREGTID                                                        
070400         ,IDREF                                                           
070500         ,DAREFDAT                                                        
070600         ,IDREFRAD                                                        
070700         ,BEVOLREF                                                        
070800         ,IDLANDX3_SEND                                                   
070900         ,IDLANDX3_REC                                                    
071000         ,IDLEVNR                                                         
071100         ,IDPARTNR                                                        
071200         ,IDEXCUST_1                                                      
071300         ,IDEXCUST_2                                                      
071400         ,IDEXCUST_3                                                      
071500         ,IDOPTION_1                                                      
071600         ,IDOPTION_2                                                      
071700         ,IDOPTION_3                                                      
071800         ,IDOPTION_4                                                      
071900         ,IDOPTION_5                                                      
072000         ,IDAPPEND                                                        
072100         ,IDARTNR_FINANCE                                                 
072200         ,IDSTATNR                                                        
072300         ,VKORDBTO_KOLLI                                                  
072400         ,VKARTNTO                                                        
072500         ,PRARTBTO                                                        
072600         ,PRARTNTO                                                        
072700         ,REARTRAB                                                        
072800         ,KVBEART                                                         
072900         ,KVLEVART                                                        
073000         ,BEART                                                           
073100         ,FLSOFT                                                          
073200         ,FLSPECPR                                                        
073300         ,FLFREE                                                          
073400         ,FLPRIV                                                          
073500         ,KDVAT                                                           
073600         ,KDVALISO                                                        
073700         ,KDINVFRQ                                                        
073800         ,KDFINDOC                                                        
073900         ,IDBREAK_1                                                       
074000         ,IDBREAK_2                                                       
074100         ,IDSEQ_1                                                         
074200         ,IDSEQ_2                                                         
074300         ,IDSEQ_3                                                         
074400         ,KDARTURS                                                        
074500         ,KDANMORS                                                        
074600         ,IDFAKREF                                                        
074700         ,DAFAKREF                                                        
074800         ,IDDC                                                            
074900         ,KDFRAKT                                                         
075000         ,BELEVVIL                                                        
075100         ,IDACCNT_1                                                       
075200         ,IDACCNT_2                                                       
075300         ,IDACCNT_3                                                       
075400         ,IDACCNT_4                                                       
075500         ,IDSYSTEM_SEND                                                   
075600         ,IDSYSTEM_REC                                                    
075700         ,DASTADAT                                                        
075800         ,BEANST                                                          
075900         ,IDUSER                                                          
076000         ,BETEXT                                                          
076100         ,BETEXT_CRE                                                      
076200         ,IDARTNR_CNTRL                                                   
076210         ,FLPCOO                                                          
076220         ,IDLEVNR_ART                                                     
076230         ,IDTRACK_1                                                       
076240         ,KVANT_TRACK_1                                                   
076250         ,IDTRACK_2                                                       
076260         ,KVANT_TRACK_2                                                   
076270         ,IDTRACK_3                                                       
076280         ,KVANT_TRACK_3                                                   
076290         ,IDTRACK_4                                                       
076291         ,KVANT_TRACK_4                                                   
076292         ,IDTRACK_5                                                       
076293         ,KVANT_TRACK_5                                                   
076294         ,KDPRMOD                                                         
076300          )                                                               
076400         VALUES(                                                          
076500                :TRAW-IDLEGSEL                                            
076600               ,:TRAW-IDBUNDLE                                            
076700               ,:TRAW-DAREGDAT                                            
076800               ,:TRAW-TIREGTID                                            
076900               ,:TRAW-IDREF                                               
077000               ,:TRAW-DAREFDAT                                            
077100               ,:WS-ALIN-IDREFRAD                                         
077200               ,:TRAW-BEVOLREF                                            
077300               ,:TRAW-IDLANDX3-SEND                                       
077400               ,:TRAW-IDLANDX3-REC                                        
077500               ,:TRAW-IDLEVNR                                             
077600               ,:TRAW-IDPARTNR                                            
077700               ,:TRAW-IDEXCUST-1                                          
077800               ,:TRAW-IDEXCUST-2                                          
077900               ,:TRAW-IDEXCUST-3                                          
078000               ,:TRAW-IDOPTION-1                                          
078100               ,:TRAW-IDOPTION-2                                          
078200               ,:TRAW-IDOPTION-3                                          
078300               ,:TRAW-IDOPTION-4                                          
078400               ,:TRAW-IDOPTION-5                                          
078500               ,:TRAW-IDAPPEND                                            
078600               ,:TRAW-IDARTNR-FINANCE                                     
078700               ,:TRAW-IDSTATNR                                            
078800               ,:TRAW-VKORDBTO-KOLLI                                      
078900               ,:TRAW-VKARTNTO                                            
079000               ,:TRAW-PRARTBTO                                            
079100               ,:TRAW-PRARTNTO                                            
079200               ,:TRAW-REARTRAB                                            
079300               ,:TRAW-KVBEART                                             
079400               ,:TRAW-KVLEVART                                            
079500               ,:TRAW-BEART                                               
079600               ,:TRAW-FLSOFT                                              
079700               ,:TRAW-FLSPECPR                                            
079800               ,:TRAW-FLFREE                                              
079900               ,:TRAW-FLPRIV                                              
080000               ,:TRAW-KDVAT                                               
080100               ,:TRAW-KDVALISO                                            
080200               ,:TRAW-KDINVFRQ                                            
080300               ,:TRAW-KDFINDOC                                            
080400               ,:TRAW-IDBREAK-1                                           
080500               ,:TRAW-IDBREAK-2                                           
080600               ,:TRAW-IDSEQ-1                                             
080700               ,:TRAW-IDSEQ-2                                             
080800               ,:TRAW-IDSEQ-3                                             
080900               ,:TRAW-KDARTURS                                            
081000               ,:TRAW-KDANMORS                                            
081100               ,:TRAW-IDFAKREF                                            
081200               ,:TRAW-DAFAKREF                                            
081300               ,:TRAW-IDDC                                                
081400               ,:TRAW-KDFRAKT                                             
081500               ,:TRAW-BELEVVIL                                            
081600               ,:TRAW-IDACCNT-1                                           
081700               ,:TRAW-IDACCNT-2                                           
081800               ,:TRAW-IDACCNT-3                                           
081900               ,:TRAW-IDACCNT-4                                           
082000               ,:TRAW-IDSYSTEM-SEND                                       
082100               ,:TRAW-IDSYSTEM-REC                                        
082200               ,:WS-ALIN-DASTADAT                                         
082300               ,:TRAW-BEANST                                              
082400               ,:TRAW-IDUSER                                              
082500               ,:TRAW-BETEXT                                              
082600               ,:TRAW-BETEXT-CRE                                          
082700               ,:TRAW-IDARTNR-CNTRL                                       
082710               ,:TRAW-FLPCOO                                              
082720               ,:TRAW-IDLEVNR-ART                                         
082730               ,:TRAW-IDTRACK-1                                           
082740               ,:TRAW-KVANT-TRACK-1                                       
082750               ,:TRAW-IDTRACK-2                                           
082760               ,:TRAW-KVANT-TRACK-2                                       
082770               ,:TRAW-IDTRACK-3                                           
082780               ,:TRAW-KVANT-TRACK-3                                       
082790               ,:TRAW-IDTRACK-4                                           
082791               ,:TRAW-KVANT-TRACK-4                                       
082792               ,:TRAW-IDTRACK-5                                           
082793               ,:TRAW-KVANT-TRACK-5                                       
082794               ,:TRAW-KDPRMOD                                             
082800                ) FOR :WS-MULTIFETCH ROWS ATOMIC                          
082900     END-EXEC                                                             
083000                                                                          
083100     ADD 1        TO RESTART-IX                                           
083200     MOVE SQLCODE TO SQLCODE-WS                                           
083300     PERFORM DB2-STATUS-CONTROL                                           
083400     .                                                                    
083500     EJECT                                                                
083600                                                                          
083700 DB2-OPEN-CRS-NEW-ABUN SECTION.                                           
083800     MOVE 000100  TO GOOD-SQLCODECODES                                    
083900     EXEC SQL DECLARE NEW-CRS CURSOR FOR                                  
084000       SELECT DISTINCT                                                    
084100              IDLEGSEL,                                                   
084200              IDBUNDLE,                                                   
084300              DAREGDAT,                                                   
084400              TIREGTID                                                    
084500                                                                          
084600       FROM   T01ALIN                                                     
084700                                                                          
084800       WHERE IDLEGSEL = :T01TBUN-IDLEGSEL                                 
084900       AND   IDBUNDLE = :T01TBUN-IDBUNDLE                                 
085000       AND   DAREGDAT = :T01TBUN-DAREGDAT                                 
085100       AND   TIREGTID = :T01TBUN-TIREGTID                                 
085200                                                                          
085300     END-EXEC                                                             
085400                                                                          
085500     MOVE 000100         TO GOOD-SQLCODECODES                             
085600     EXEC SQL OPEN NEW-CRS END-EXEC                                       
085700     MOVE SQLCODE        TO SQLCODE-WS                                    
085800     PERFORM DB2-STATUS-CONTROL                                           
085900     .                                                                    
086000     EJECT                                                                
086100                                                                          
086200 DB2-FETCH-CRS-NEW-ABUN SECTION.                                          
086300     MOVE 000100       TO GOOD-SQLCODECODES                               
086400     EXEC SQL FETCH NEW-CRS INTO                                          
086500            :ABUN-IDLEGSEL,                                               
086600            :ABUN-IDBUNDLE,                                               
086700            :ABUN-DAREGDAT,                                               
086800            :ABUN-TIREGTID                                                
086900     END-EXEC                                                             
087000                                                                          
087100     MOVE SQLCODE        TO SQLCODE-WS                                    
087200     PERFORM DB2-STATUS-CONTROL                                           
087300     .                                                                    
087400     EJECT                                                                
087500                                                                          
087600 DB2-INSERT-ABUN SECTION.                                                 
087700     MOVE 000    TO GOOD-SQLCODECODES                                     
087800     EXEC SQL INSERT INTO T01ABUN                                         
087900        (                                                                 
088000         IDLEGSEL,                                                        
088100         IDBUNDLE,                                                        
088200         DAREGDAT,                                                        
088300         TIREGTID                                                         
088400        )                                                                 
088500       VALUES                                                             
088600        (                                                                 
088700         :ABUN-IDLEGSEL,                                                  
088800         :ABUN-IDBUNDLE,                                                  
088900         :ABUN-DAREGDAT,                                                  
089000         :ABUN-TIREGTID                                                   
089100        )                                                                 
089200     END-EXEC                                                             
089300                                                                          
089400     ADD 1        TO RESTART-IX                                           
089500     MOVE SQLCODE        TO SQLCODE-WS                                    
089600     PERFORM DB2-STATUS-CONTROL                                           
089700     .                                                                    
089800     EJECT                                                                
089900                                                                          
090000 DB2-CLOSE-CRS-NEW-ABUN SECTION.                                          
090100     SKIP2                                                                
090200     EXEC SQL CLOSE NEW-CRS                                               
090300     END-EXEC                                                             
090400     .                                                                    
090500     EJECT                                                                
090600                                                                          
090700 DB2-SELECT-T01INRE SECTION.                                              
090800     MOVE 000     TO GOOD-SQLCODECODES                                    
090900     EXEC SQL                                                             
091000       SELECT FLEXPORT                                                    
091100                                                                          
091200       INTO :T01INRE-FLEXPORT                                             
091300                                                                          
091400       FROM T01INRE                                                       
091500                                                                          
091600       WHERE IDLEGSEL      = :WS-IDLEGSEL                                 
091700       AND   IDLANDX3_SEND = :WS-IDLANDX3-SEND                            
091800       AND   IDLANDX3_REC  = :WS-IDLANDX3-REC                             
091900       AND   KDSTATUS = 001                                               
092000       AND   DADELDAT = '00000000'                                        
092100     END-EXEC                                                             
092200     MOVE SQLCODE      TO SQLCODE-WS                                      
092300     PERFORM DB2-STATUS-CONTROL                                           
092400     .                                                                    
092500     EJECT                                                                
092600                                                                          
092700 DB2-SELECT-T01FCUS SECTION.                                              
092800     MOVE 000     TO GOOD-SQLCODECODES                                    
092900     EXEC SQL                                                             
093000       SELECT IDLEGSEL                                                    
093100             ,IDLANDX3                                                    
093200             ,KDPARTTY                                                    
093300             ,KDPARTGR                                                    
093400                                                                          
093500       INTO :T01FCUS-IDLEGSEL                                             
093600           ,:WS-IDLANDX3-REC                                              
093700           ,:T01FCUS-KDPARTTY                                             
093800           ,:T01FCUS-KDPARTGR                                             
093900                                                                          
094000       FROM T01FCUS                                                       
094100                                                                          
094200       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
094300       AND   IDPARTNR = :WS-IDPARTNR                                      
094400       AND   KDSTATUS = 001                                               
094500       AND   DADELDAT = '00000000'                                        
094600     END-EXEC                                                             
094700     MOVE SQLCODE      TO SQLCODE-WS                                      
094800     PERFORM DB2-STATUS-CONTROL                                           
094900     .                                                                    
095000     EJECT                                                                
095100                                                                          
095200 DB2-SELECT-T01CUGR SECTION.                                              
095300     MOVE 000     TO GOOD-SQLCODECODES                                    
095400     EXEC SQL                                                             
095500       SELECT KDINVFRQ                                                    
095600                                                                          
095700       INTO :T01CUGR-KDINVFRQ                                             
095800                                                                          
095900       FROM T01CUGR                                                       
096000                                                                          
096100       WHERE IDLEGSEL = :T01FCUS-IDLEGSEL                                 
096200       AND   KDPARTTY = :T01FCUS-KDPARTTY                                 
096300       AND   KDPARTGR = :T01FCUS-KDPARTGR                                 
096400       AND   KDSTATUS = 001                                               
096500       AND   DADELDAT = '00000000'                                        
096600     END-EXEC                                                             
096700     MOVE SQLCODE      TO SQLCODE-WS                                      
096800     PERFORM DB2-STATUS-CONTROL                                           
096900     .                                                                    
097000     EJECT                                                                
097100                                                                          
097200 DB2-SELECT-T01LSEL SECTION.                                              
097300     MOVE 000     TO GOOD-SQLCODECODES                                    
097400     EXEC SQL                                                             
097500       SELECT KDINVFRQ                                                    
097600                                                                          
097700       INTO :T01LSEL-KDINVFRQ                                             
097800                                                                          
097900       FROM T01LSEL                                                       
098000                                                                          
098100       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
098200       AND   KDSTATUS = 001                                               
098300     END-EXEC                                                             
098400     MOVE SQLCODE      TO SQLCODE-WS                                      
098500     PERFORM DB2-STATUS-CONTROL                                           
098600     .                                                                    
098700     EJECT                                                                
098800                                                                          
098900 DB2-SELECT-T01TBUN-NONCNTRL SECTION.                                     
099000     MOVE 000100305    TO GOOD-SQLCODECODES                               
099100     EXEC SQL                                                             
099200       SELECT   MIN(IDLEGSEL)                                             
099300                                                                          
099400       INTO    :T01TBUN-IDLEGSEL                                          
099500                                                                          
099600       FROM     T01TBUN                                                   
099700                                                                          
099800       WHERE    FLKLAR   = :NOO                                           
099900       AND      FLFEL    = :NOO                                           
100000       AND      FLKNTRL  = :YES                                           
100300     END-EXEC                                                             
100400                                                                          
100500     MOVE SQLCODE      TO SQLCODE-WS                                      
100600     PERFORM DB2-STATUS-CONTROL                                           
100700     .                                                                    
100800     EJECT                                                                
100900                                                                          
101000 DB2-SELECT-T01SYST SECTION.                                              
101100     MOVE 000       TO GOOD-SQLCODECODES                                  
101200     EXEC SQL                                                             
101300       SELECT   KDBEHX                                                    
101400                                                                          
101500       INTO    :SYST-KDBEHX                                               
101600                                                                          
101700       FROM     T01SYST                                                   
101800                                                                          
101900     END-EXEC                                                             
102000                                                                          
102100     MOVE SQLCODE      TO SQLCODE-WS                                      
102200     PERFORM DB2-STATUS-CONTROL                                           
102300     .                                                                    
102400     EJECT                                                                
102500                                                                          
102600 DB2-UPDATE-T01SYST SECTION.                                              
102700     MOVE 000     TO GOOD-SQLCODECODES                                    
102800     EXEC SQL UPDATE T01SYST                                              
102900                                                                          
103000       SET KDBEHX   = :SYST-KDBEHX                                        
103100                                                                          
103200     END-EXEC                                                             
103300                                                                          
103400     MOVE SQLCODE TO SQLCODE-WS                                           
103500     PERFORM DB2-STATUS-CONTROL                                           
103600     .                                                                    
103700     EJECT                                                                
103800                                                                          
103900 DB2-STATUS-CONTROL   SECTION.                                            
104000     SKIP2                                                                
104100     SET SQLCODE-IX TO 1                                                  
104200     SEARCH GOOD-SQLCODE                                                  
104300       AT END                                                             
104400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
104500          DELIMITED BY SIZE INTO ERRORTEXT                                
104600          CALL ABEND USING RKOD-ABEND-DB2                                 
104700       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
104800     END-SEARCH                                                           
104900     .                                                                    

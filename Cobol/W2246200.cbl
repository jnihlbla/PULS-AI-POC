000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2246200.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   13/10/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER EN INFIL MED AKTUELLA LEVNR SAMT EN             
001000*        INFIL MED KINA/NA-USA ARTIKLAR MED GÄLLANDE AVROP.               
001100*        UPPDATERAR HÄNDELSEBAS WDR5, H-TYP 2247 (FLLEVPLP=J) MED         
001200*        ARTIKEL/DC/LEVERANTÖR.WDR501 COPY-TEXT WDGX2247.                 
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WDR5 (H-TYP 2247)                          
001500*                                                                         
001600*                                                                         
001700*    2013-10-16  E-TRACKER: 10205391  RUTIN FÖR VECKO-/PERIOD-            
001800*                                     BATCH BILD 2117.                    
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- LEVERANTÖRER KINA/NA-USA NDC'R                             
002900     SELECT W22460                     ASSIGN TO W22462D1.                
003000     SKIP2                                                                
003100*          --- ARTIKLAR MED KDAVROP = 2                                   
003200     SELECT W22461                     ASSIGN TO W22462D2.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W22460                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W22460      -L.                                                
004300     SKIP3                                                                
004400 FD  W22461                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  -COPY W22461      -L.                                                
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100                                                                          
005200 77  IDPGM                       PIC X(8)    VALUE 'W2246200'.            
005300 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
005400 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
005500                                                                          
005600 77  W-W22460-KVPOST-IN          PIC S9(9)   VALUE +0   COMP-3.           
005700 77  W-W22461-KVPOST-IN          PIC S9(9)   VALUE +0   COMP-3.           
005800                                                                          
005900                                                                          
006000 01  CHKP-VAR.                                                            
006100     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006200     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006300     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006400     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006500     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006600     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900     SKIP2                                                                
007000 01  FELTEXT.                                                             
007100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007300                                                                          
007400 77  W22460-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W22460                       VALUE 'J'.                   
007600                                                                          
007700 77  W22461-EOF-SW               PIC X       VALUE 'N'.                   
007800     88  END-OF-W22461                       VALUE 'J'.                   
007900     EJECT                                                                
008000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008100 01  FILLER REDEFINES DAGENS-DATUM.                                       
008200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008500     EJECT                                                                
008600 01  DYNAMISKA-SUBPROGRAM.                                                
008700*                                                                         
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL POSTSUM                                          
009300*                                                                         
009400*01  -COPY W0005   -PRE  POSTSUM-                                         
009500     EJECT                                                                
009600 01  IN60-AREA-START             PIC X(24)   VALUE                        
009700                                             'IN60-AREA-START'.           
009800     SKIP2                                                                
009900                                                                          
010000*01  AREA -COPY W22460     -PRE IN60-                                     
010100     EJECT                                                                
010200 01  IN61-AREA-START             PIC X(24)   VALUE                        
010300                                             'IN61-AREA-START'.           
010400     SKIP2                                                                
010500                                                                          
010600*01  AREA -COPY W22461     -PRE IN61-                                     
010700*                                                                         
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000     SKIP3                                                                
011100 01  NYCKLAR-TILL-DLI.                                                    
011200     03  W-WDGXKEY-2247-X.                                                
011300         05  W-IDHTYP            PIC X(04)    VALUE '2247'.               
011400         05  W-FLLEVPLP          PIC X        VALUE 'J'.                  
011500         05  FILLER              PIC X(25)    VALUE LOW-VALUE.            
011600     03  W-KY2248-X.                                                      
011700         05  W-IDDC-2248         PIC X(02)    VALUE SPACE.                
011800         05  W-IDLEVNR-2248      PIC X(05)    VALUE SPACE.                
011900         05  W-IDARTNR-2248      PIC S9(9)    VALUE ZERO COMP-3.          
012000     SKIP2                                                                
012100*    --- STATUS-KOD FRÅN IMS                                              
012200 01  STATUS-WS                   PIC XX.                                  
012300     88  SEGMENT-FINNS                       VALUE '  '.                  
012400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012700     88  IMS-EJ-OK                           VALUE 'XD'.                  
012800     SKIP2                                                                
012900 01  GODK-STATUSKODER.                                                    
013000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     SKIP3                                                                
013200 01  SSA1                        PIC X(64).                               
013300 01  SSA2                        PIC X(64).                               
013400     EJECT                                                                
013500*    --- IMS FUNKTIONSKODER                                               
013600*01  -COPY W0003                                                          
013700     EJECT                                                                
013800*    ---  DLI INPUT-OUTPUT AREA                                           
013900                                                                          
014000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2248'.                    
014100 01  DLI-IO-WDGX2248.                                                     
014200*    03  -COPY WDGX2248                                                   
014300                                                                          
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600                                                                          
014700*01  -COPY W0009   -PRE MSG-                                              
014800                                                                          
014900*01  -COPY W0008  -PRE 2247-                                              
015000     05  FILLER                  PIC X.                                   
015100     EJECT                                                                
015200 PROCEDURE DIVISION  USING MSG-PCB 2247-PCB.                              
015300 MAIN SECTION.                                                            
015400     ENTRY 'DLITCBL' USING MSG-PCB 2247-PCB.                              
015500                                                                          
015600     SKIP2                                                                
015700     PERFORM A-INIT                                                       
015800     PERFORM S01-LAES-W22460                                              
015900     PERFORM S02-LAES-W22461                                              
016000     PERFORM UNTIL END-OF-W22460 OR END-OF-W22461                         
016100       IF CHKP-ANT > CHKP-MAX                                             
016200         PERFORM X-TAG-CHECKPOINT                                         
016300       END-IF                                                             
016400       IF IN61-IDLEVNR < IN60-IDLEVNR                                     
016500          PERFORM S02-LAES-W22461                                         
016600       ELSE                                                               
016700          IF IN61-IDLEVNR > IN60-IDLEVNR                                  
016800             PERFORM S01-LAES-W22460                                      
016900          ELSE                                                            
017000*----------  IN61-IDLEVNR = IN60-IDLEVNR                                  
017100             IF IN61-IDDC < IN60-IDDC                                     
017200               PERFORM S02-LAES-W22461                                    
017300             ELSE                                                         
017400               IF IN61-IDDC > IN60-IDDC                                   
017500                 PERFORM S01-LAES-W22460                                  
017600               ELSE                                                       
017700                  MOVE IN61-IDDC     TO 2248-IDDC                         
017800                  MOVE IN61-IDLEVNR  TO 2248-IDLEVNR                      
017900                  MOVE IN61-IDARTNR  TO 2248-IDARTNR                      
018000                  MOVE ZERO          TO 2248-KVDAGAR                      
018100                  MOVE ZERO          TO 2248-KVBEART                      
018200                  PERFORM IMS-ISRT-WDGX2248                               
018300*----------          DET ÄR OK MED STATUS II                              
018400                  ADD +1           TO CHKP-ANT                            
018500                  PERFORM S02-LAES-W22461                                 
018600               END-IF                                                     
018700             END-IF                                                       
018800          END-IF                                                          
018900       END-IF                                                             
019000     END-PERFORM                                                          
019100                                                                          
019200                                                                          
019300     PERFORM Z-FINIT                                                      
019400                                                                          
019500     MOVE ZERO TO RETURN-CODE                                             
019600     GOBACK                                                               
019700     .                                                                    
019800     EJECT                                                                
019900 A-INIT SECTION.                                                          
020000     MOVE 'A-INIT '  TO CURRENT-SECTION                                   
020100     SKIP2                                                                
020200                                                                          
020300     PERFORM IMS-RESTART                                                  
020400                                                                          
020500     OPEN INPUT W22460                                                    
020600                                                                          
020700                W22461                                                    
020800                                                                          
020900                                                                          
021000     MOVE +0    TO W-W22460-KVPOST-IN                                     
021100                   W-W22461-KVPOST-IN                                     
021200                                                                          
021300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021400     .                                                                    
021500     EJECT                                                                
021600 Z-FINIT SECTION.                                                         
021700                                                                          
021800                                                                          
021900     CLOSE W22460                                                         
022000                                                                          
022100           W22461                                                         
022200     SKIP2                                                                
022300     MOVE 'S' TO POSTSUM-OPKOD                                            
022400     CALL POSTSUM USING POSTSUM-PARM                                      
022500     .                                                                    
022600     EJECT                                                                
022700 S01-LAES-W22460  SECTION.                                                
022800     MOVE 'S01-LAES-W22460 '  TO CURRENT-SECTION                          
022900     SKIP2                                                                
023000*    LÄS SORTERADE AKTUELLA LEVNR                                         
023100                                                                          
023200     READ W22460 INTO IN60-AREA                                           
023300     AT END                                                               
023400        MOVE HIGH-VALUE TO IN60-AREA                                      
023500        SET END-OF-W22460 TO TRUE                                         
023600                                                                          
023700     NOT AT END                                                           
023800        MOVE 'W22460'   TO POSTSUM-FDNAMN                                 
023900        MOVE 'W22462D1' TO POSTSUM-DDNAMN2                                
024000        MOVE 'LEV'      TO POSTSUM-TRANSTYP                               
024100        CALL POSTSUM USING POSTSUM-PARM                                   
024200                                                                          
024300        ADD 1 TO W-W22460-KVPOST-IN                                       
024400     END-READ                                                             
024500     .                                                                    
024600     EJECT                                                                
024700 S02-LAES-W22461  SECTION.                                                
024800     MOVE 'S02-LAES-W22461 '  TO CURRENT-SECTION                          
024900     SKIP2                                                                
025000*    LÄS SORTERADE (LEVNR) ARTIKLAR MED GODKÄNDA AVROP.                   
025100                                                                          
025200     READ W22461 INTO IN61-AREA                                           
025300     AT END                                                               
025400        MOVE HIGH-VALUE TO IN61-AREA                                      
025500        SET END-OF-W22461 TO TRUE                                         
025600                                                                          
025700     NOT AT END                                                           
025800        MOVE 'W22461'   TO POSTSUM-FDNAMN                                 
025900        MOVE 'W22462D2' TO POSTSUM-DDNAMN2                                
026000        MOVE 'IN61'     TO POSTSUM-TRANSTYP                               
026100        CALL POSTSUM USING POSTSUM-PARM                                   
026200                                                                          
026300        ADD 1 TO W-W22461-KVPOST-IN                                       
026400     END-READ                                                             
026500     .                                                                    
026600     EJECT                                                                
026700 X-TAG-CHECKPOINT   SECTION.                                              
026800                                                                          
026900* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
027000* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
027100     PERFORM IMS-CHECKPOINT                                               
027200     MOVE ZERO TO CHKP-ANT                                                
027300* --- LÄS OM DATABAS OM DET BEHÖVS                                        
027400     .                                                                    
027500     EJECT                                                                
027600* --- IMS SEKTIONER ---                                                   
027700                                                                          
027800     EJECT                                                                
027900 IMS-ISRT-WDGX2248 SECTION.                                               
028000     MOVE 'IMS-ISRT-WDGX2248 '   TO DBS-SECTION                           
028100                                                                          
028200     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2247-X ')'                    
028300          DELIMITED BY SIZE INTO SSA1                                     
028400     MOVE 'WDGX2248'          TO SSA2                                     
028500     MOVE '  II' TO GODK-STATUSKODER                                      
028600     CALL CBLTDLI USING ISRT 2247-PCB DLI-IO-WDGX2248 SSA1 SSA2           
028700     MOVE 2247-STATUS-CODE TO STATUS-WS                                   
028800     PERFORM IMS-STATUSKONTROLL                                           
028900                                                                          
029000     IF SEGMENT-FINNS-REDAN                                               
029100        MOVE 'II'       TO POSTSUM-TRANSTYP                               
029200     ELSE                                                                 
029300        MOVE 'ISRT'     TO POSTSUM-TRANSTYP                               
029400     END-IF                                                               
029500     MOVE '2248'     TO POSTSUM-FDNAMN                                    
029600     MOVE 'WDR5'     TO POSTSUM-DDNAMN2                                   
029700     CALL POSTSUM USING POSTSUM-PARM                                      
029800     .                                                                    
029900     EJECT                                                                
030000 IMS-RESTART SECTION.                                                     
030100     SKIP2                                                                
030200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
030300     MOVE '  ' TO GODK-STATUSKODER                                        
030400     CALL CBLTDLI USING XRST MSG-PCB                                      
030500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
030600                        CHKP-AREA-LENGTH CHKP-AREA                        
030700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030800     PERFORM IMS-STATUSKONTROLL                                           
030900     .                                                                    
031000     SKIP3                                                                
031100 IMS-CHECKPOINT SECTION.                                                  
031200     SKIP2                                                                
031300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
031400     MOVE '  XD' TO GODK-STATUSKODER                                      
031500     CALL CBLTDLI USING CHKP MSG-PCB                                      
031600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
031700                        CHKP-AREA-LENGTH CHKP-AREA                        
031800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031900     PERFORM IMS-STATUSKONTROLL                                           
032000                                                                          
032100     IF IMS-EJ-OK                                                         
032200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
032300       DISPLAY FELTEXT                                                    
032400       CALL FELLOG                                                        
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 IMS-STATUSKONTROLL SECTION.                                              
032900     SKIP2                                                                
033000     SET STATUS-IX TO 1                                                   
033100     SEARCH GODK-STATUS                                                   
033200       AT END                                                             
033300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033400           DELIMITED BY SIZE INTO FELTEXT                                 
033500         DISPLAY FELTEXT                                                  
033600         CALL FELLOG                                                      
033700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033800         CONTINUE                                                         
033900     END-SEARCH                                                           
034000     .                                                                    

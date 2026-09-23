000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2246400.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   13/11/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER FIL W22464 MED ARTIKLAR SOM SKALL DELETAS PÅ WDR5 /        
001000*        WDGX2248.OBS  NY ROT PÅ WDR5 = 2247J ! (-COPY WDGX2247)          
001110*                                                                         
001200*        UPPDATERAR IDOVERFNR PÅ WDR2 HT-2205 / WDGX2206.                 
001300*        NYA VÄRDET FRÅN PROGRAM W2246300.                                
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WDR5                                       
001600*        PROGRAMMET UPPDATERAR WDR2                                       
001700*                                                                         
001710* BMP                                                                     
001800*                                                                         
001900*    2013-11-04  E-TRACKER: 10205391                                      
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- INFIL DELETEPOSTER                                         
003000     SELECT W22464                     ASSIGN TO W22464D1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W22464                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W22464      -L.                                                
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W2246400'.            
004500 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
004600 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
004700                                                                          
004800 77  W-W22464-KVPOST-IN          PIC S9(9)   VALUE +0   COMP-3.           
004900                                                                          
005000 01  CHKP-VAR.                                                            
005100     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005200     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005300     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005400     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005500     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005600     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900     SKIP2                                                                
006000 01  FELTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006300                                                                          
006400 77  W22464-EOF-SW               PIC X       VALUE 'N'.                   
006500     88  END-OF-W22464                       VALUE 'J'.                   
006600     EJECT                                                                
006700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006800 01  FILLER REDEFINES DAGENS-DATUM.                                       
006900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007200     EJECT                                                                
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400*                                                                         
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008300 01  IN64-AREA-START             PIC X(24)   VALUE                        
008400                                             'IN64-AREA-START'.           
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W22464     -PRE IN64-                                     
008800*                                                                         
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009100     SKIP3                                                                
009200 01  NYCKLAR-TILL-DLI.                                                    
009300     03  W-WDGXKEY-2247-X.                                                
009400         05  W-IDHTYP            PIC X(04)   VALUE '2247'.                
009500         05  W-FLLEVPLP          PIC X       VALUE 'J'.                   
009600         05  FILLER              PIC X(25)   VALUE LOW-VALUE.             
009700                                                                          
009800     03  W-KY2248-X.                                                      
009900         05  W-IDDC-2248         PIC X(02)   VALUE SPACE.                 
010000         05  W-IDLEVNR-2248      PIC X(05)   VALUE SPACE.                 
010100         05  W-IDARTNR-2248      PIC S9(9)   VALUE ZERO COMP-3.           
010200                                                                          
010300     03  W-WDGXKEY-2205-X.                                                
010400         05  W-IDHTYP            PIC X(4)    VALUE '2205'.                
010500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
010600                                                                          
010700     03  W-KY2206-X.                                                      
010800         05  W-IDLEVNR-2206      PIC X(5)    VALUE SPACE.                 
010900         05  W-IDDC-2206         PIC X(2)    VALUE SPACE.                 
011000     SKIP2                                                                
011100*    --- STATUS-KOD FRÅN IMS                                              
011200 01  STATUS-WS                   PIC XX.                                  
011300     88  SEGMENT-FINNS                       VALUE '  '.                  
011400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011700     88  IMS-EJ-OK                           VALUE 'XD'.                  
011800     SKIP2                                                                
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(64).                               
012400     EJECT                                                                
012500*    --- IMS FUNKTIONSKODER                                               
012600*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2248'.                    
013500 01  DLI-IO-WDGX2248.                                                     
013600*    03  -COPY WDGX2248                                                   
013700     EJECT                                                                
014200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2206'.                    
014300 01  DLI-IO-WDGX2206.                                                     
014400*    03  -COPY WDGX2206                                                   
014500     EJECT                                                                
014600 LINKAGE SECTION.                                                         
014700                                                                          
014800*01  -COPY W0009   -PRE MSG-                                              
014900                                                                          
015000*01  -COPY W0008  -PRE 2247-                                              
015100     05  FILLER                  PIC X.                                   
015200                                                                          
015300*01  -COPY W0008  -PRE 2205-                                              
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600 PROCEDURE DIVISION  USING MSG-PCB 2247-PCB 2205-PCB.                     
015700 MAIN SECTION.                                                            
015800     ENTRY 'DLITCBL' USING MSG-PCB 2247-PCB 2205-PCB.                     
015900                                                                          
016000     SKIP2                                                                
016100     PERFORM A-INIT                                                       
016200     PERFORM S01-LAES-W22464                                              
016300     PERFORM UNTIL END-OF-W22464                                          
016400       IF CHKP-ANT > CHKP-MAX                                             
016500         PERFORM X-TAG-CHECKPOINT                                         
016600       END-IF                                                             
016700                                                                          
016800       PERFORM B-DELETE-WDGX2248                                          
016900                                                                          
017000       IF IN64-IDOVERFNR NOT = ZERO                                       
017100         PERFORM C-REPLACE-WDGX2206                                       
017200       END-IF                                                             
017300                                                                          
017400       PERFORM S01-LAES-W22464                                            
017500     END-PERFORM                                                          
017600                                                                          
017700                                                                          
017800     PERFORM Z-FINIT                                                      
017900                                                                          
018000     MOVE ZERO TO RETURN-CODE                                             
018100     GOBACK                                                               
018200     .                                                                    
018300     EJECT                                                                
018400 A-INIT SECTION.                                                          
018500     MOVE 'A-INIT '  TO CURRENT-SECTION                                   
018600     SKIP2                                                                
018700                                                                          
018800     PERFORM IMS-RESTART                                                  
018900                                                                          
019000     OPEN INPUT W22464                                                    
019100                                                                          
019200     MOVE +0    TO W-W22464-KVPOST-IN                                     
019300                                                                          
019400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019500     .                                                                    
019600     EJECT                                                                
019700 B-DELETE-WDGX2248  SECTION.                                              
019800     MOVE 'B-DELETE-WDGX2248  '    TO CURRENT-SECTION                     
019900                                                                          
020000     MOVE IN64-IDDC        TO W-IDDC-2248                                 
020100     MOVE IN64-IDLEVNR     TO W-IDLEVNR-2248                              
020200     MOVE IN64-IDARTNR     TO W-IDARTNR-2248                              
020300                                                                          
020400     PERFORM IMS-GHU-WDGX2248                                             
020500                                                                          
020600     IF SEGMENT-FINNS                                                     
020700        PERFORM IMS-DLET-WDGX2248                                         
020800        ADD +1 TO CHKP-ANT                                                
020900     END-IF                                                               
021000                                                                          
021100     .                                                                    
021200     EJECT                                                                
021300 C-REPLACE-WDGX2206  SECTION.                                             
021400     MOVE 'C-REPLACE-WDGX2206  '    TO CURRENT-SECTION                    
021500                                                                          
021600     MOVE IN64-IDLEVNR     TO W-IDLEVNR-2206                              
021700     MOVE IN64-IDDC        TO W-IDDC-2206                                 
021800                                                                          
021900     PERFORM IMS-GHU-WDGX2206                                             
022000                                                                          
022100     IF SEGMENT-FINNS                                                     
022200        MOVE IN64-IDOVERFNR   TO 2206-IDOVERFNR                           
022300                                                                          
022500        PERFORM IMS-REPL-WDGX2206                                         
022600        ADD +1 TO CHKP-ANT                                                
022700     END-IF                                                               
022800                                                                          
022900     .                                                                    
023000     EJECT                                                                
023100 Z-FINIT SECTION.                                                         
023200     MOVE 'Z-FINIT '  TO CURRENT-SECTION                                  
023300                                                                          
023400                                                                          
023500     CLOSE W22464                                                         
023600     SKIP2                                                                
023700     MOVE 'S' TO POSTSUM-OPKOD                                            
023800     CALL POSTSUM USING POSTSUM-PARM                                      
023900     .                                                                    
024000     EJECT                                                                
024100 S01-LAES-W22464  SECTION.                                                
024200     MOVE 'S01-LAES-W22464 '  TO CURRENT-SECTION                          
024300     SKIP2                                                                
024400     READ W22464 INTO IN64-AREA                                           
024500     AT END                                                               
024600        MOVE HIGH-VALUE TO IN64-AREA                                      
024700        SET END-OF-W22464 TO TRUE                                         
024800                                                                          
024900     NOT AT END                                                           
025000        MOVE 'W22464'    TO POSTSUM-FDNAMN                                
025100        MOVE 'W22464D1'  TO POSTSUM-DDNAMN2                               
025200        MOVE 'IN'        TO POSTSUM-TRANSTYP                              
025300        CALL POSTSUM USING POSTSUM-PARM                                   
025400                                                                          
025500        ADD 1 TO W-W22464-KVPOST-IN                                       
025600     END-READ                                                             
025700     .                                                                    
025800     EJECT                                                                
025900 X-TAG-CHECKPOINT   SECTION.                                              
026000     MOVE 'X-TAG-CHECKPOINT '  TO CURRENT-SECTION                         
026100                                                                          
026200* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
026300* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
026400                                                                          
026500     PERFORM IMS-CHECKPOINT                                               
026600     MOVE ZERO TO CHKP-ANT                                                
026700                                                                          
026800* --- LÄS OM DATABAS OM DET BEHÖVS                                        
026900     .                                                                    
027000     EJECT                                                                
027100* --- IMS SEKTIONER ---                                                   
027200                                                                          
027300     EJECT                                                                
027400 IMS-GHU-WDGX2248 SECTION.                                                
027500     MOVE 'IMS-GHU-WDGX2248 '  TO DBS-SECTION                             
027600                                                                          
027700     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2247-X ')'                    
027800          DELIMITED BY SIZE INTO SSA1                                     
027900     STRING 'WDGX2248(KY2248   =' W-KY2248-X ')'                          
028000          DELIMITED BY SIZE INTO SSA2                                     
028100     MOVE '  GE' TO GODK-STATUSKODER                                      
028200     CALL CBLTDLI USING GHU 2247-PCB DLI-IO-WDGX2248 SSA1 SSA2            
028300     MOVE 2247-STATUS-CODE TO STATUS-WS                                   
028400     PERFORM IMS-STATUSKONTROLL                                           
028500     .                                                                    
028600     EJECT                                                                
028700 IMS-DLET-WDGX2248 SECTION.                                               
028800     MOVE 'IMS-DLET-WDGX2248 ' TO DBS-SECTION                             
028900                                                                          
029000     MOVE '  ' TO GODK-STATUSKODER                                        
029100     CALL CBLTDLI USING DLET 2247-PCB DLI-IO-WDGX2248                     
029200     MOVE 2247-STATUS-CODE TO STATUS-WS                                   
029300     PERFORM IMS-STATUSKONTROLL                                           
029400                                                                          
029500     MOVE '2248'     TO POSTSUM-FDNAMN                                    
029600     MOVE 'WDR5'     TO POSTSUM-DDNAMN2                                   
029700     MOVE 'DLET'     TO POSTSUM-TRANSTYP                                  
029800     CALL POSTSUM USING POSTSUM-PARM                                      
029900     .                                                                    
030000     EJECT                                                                
030100 IMS-GHU-WDGX2206 SECTION.                                                
030200     MOVE 'IMS-GHU-WDGX2206 '  TO DBS-SECTION                             
030300                                                                          
030400     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2205-X ')'                    
030500          DELIMITED BY SIZE INTO SSA1                                     
030600     STRING 'WDGX2206(KY2206   =' W-KY2206-X ')'                          
030700          DELIMITED BY SIZE INTO SSA2                                     
030800     MOVE '  GE' TO GODK-STATUSKODER                                      
030900     CALL CBLTDLI USING GHU 2205-PCB DLI-IO-WDGX2206 SSA1 SSA2            
031000     MOVE 2205-STATUS-CODE TO STATUS-WS                                   
031100     PERFORM IMS-STATUSKONTROLL                                           
031200     .                                                                    
031300     EJECT                                                                
031400 IMS-REPL-WDGX2206 SECTION.                                               
031500     MOVE 'IMS-REPL-WDGX2206 ' TO DBS-SECTION                             
031600                                                                          
031700     MOVE '  ' TO GODK-STATUSKODER                                        
031800     CALL CBLTDLI USING REPL 2205-PCB DLI-IO-WDGX2206                     
031900     MOVE 2205-STATUS-CODE TO STATUS-WS                                   
032000     PERFORM IMS-STATUSKONTROLL                                           
032100                                                                          
032200     MOVE '2206'     TO POSTSUM-FDNAMN                                    
032300     MOVE 'WDR2'     TO POSTSUM-DDNAMN2                                   
032400     MOVE 'REPL'     TO POSTSUM-TRANSTYP                                  
032500     CALL POSTSUM USING POSTSUM-PARM                                      
032600     .                                                                    
032700     EJECT                                                                
032800 IMS-RESTART SECTION.                                                     
032900     MOVE 'IMS-RESTART '   TO DBS-SECTION                                 
033000     SKIP2                                                                
033100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
033200     MOVE '  ' TO GODK-STATUSKODER                                        
033300     CALL CBLTDLI USING XRST MSG-PCB                                      
033400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
033500                        CHKP-AREA-LENGTH CHKP-AREA                        
033600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
033700     PERFORM IMS-STATUSKONTROLL                                           
033800     .                                                                    
033900     SKIP3                                                                
034000 IMS-CHECKPOINT SECTION.                                                  
034100     MOVE 'IMS-CHECKPOINT '   TO DBS-SECTION                              
034200     SKIP2                                                                
034300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
034400     MOVE '  XD' TO GODK-STATUSKODER                                      
034500     CALL CBLTDLI USING CHKP MSG-PCB                                      
034600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
034700                        CHKP-AREA-LENGTH CHKP-AREA                        
034800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034900     PERFORM IMS-STATUSKONTROLL                                           
035000                                                                          
035100     IF IMS-EJ-OK                                                         
035200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
035300       DISPLAY FELTEXT                                                    
035400       CALL FELLOG                                                        
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 IMS-STATUSKONTROLL SECTION.                                              
035900     SKIP2                                                                
036000     SET STATUS-IX TO 1                                                   
036100     SEARCH GODK-STATUS                                                   
036200       AT END                                                             
036300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036400           DELIMITED BY SIZE INTO FELTEXT                                 
036500         DISPLAY FELTEXT                                                  
036600         CALL FELLOG                                                      
036700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036800         CONTINUE                                                         
036900     END-SEARCH                                                           
037000     .                                                                    

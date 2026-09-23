000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2723700.                                                
000300 AUTHOR.         JOHAN NIHLBLAD.                                          
000400 DATE-WRITTEN.   15/11/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR DATA PÅ WDK6 VID ERSÄTTNINGAR I REFILL                
001000*        FÖR REFILL TILL CDC                                              
001100*        PROGRAMMET UPPDATERAR WDK6                                       
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- BUYERKOD SAMT REFILLKÖPTABELL                              
002200     SELECT W27236                     ASSIGN TO W27237D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W27236                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W27236      -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W2723700'.            
003900 01  CHKP-VAR.                                                            
004000 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004100 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004200 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004300 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004400 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004500 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800     SKIP2                                                                
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200                                                                          
005300 77  W27236-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W27236                       VALUE 'J'.                   
005500                                                                          
005600     EJECT                                                                
005700 01  ARBETSAREOR.                                                         
005800                                                                          
005900     03 WS-ANTAL-W27236          PIC 9(8)   VALUE ZERO.                   
006000     03 WS-ANTAL-UPD-WDK611      PIC 9(8)   VALUE ZERO.                   
006100     03 WS-ANTAL-UPD-WDK629      PIC 9(8)   VALUE ZERO.                   
006200     EJECT                                                                
006300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006400 01  FILLER REDEFINES DAGENS-DATUM.                                       
006500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006800     EJECT                                                                
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL POSTSUM                                          
007600*                                                                         
007700*01  -COPY W0005   -PRE  POSTSUM-                                         
007800     EJECT                                                                
007900 01  IN-AREA-START               PIC X(24)   VALUE                        
008000                                             'IN-AREA-START'.             
008100     SKIP2                                                                
008200                                                                          
008300*01  AREA -COPY W27236     -PRE IN-                                       
008400*                                                                         
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008700     SKIP3                                                                
008800 01  NYCKLAR-TILL-DLI.                                                    
008900     03  W-IDARTNR-X.                                                     
009000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009100     03  W-KDSEGKEY-X.                                                    
009200         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
009300     SKIP2                                                                
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FINNS                       VALUE '  '.                  
009700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010000     88  IMS-EJ-OK                           VALUE 'XD'.                  
010100     SKIP2                                                                
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(64).                               
010600 01  SSA2                        PIC X(64).                               
010700 01  SSA3                        PIC X(64).                               
010800     EJECT                                                                
010900*    --- IMS FUNKTIONSKODER                                               
011000*01  -COPY W0003                                                          
011100     EJECT                                                                
011200*    ---  DLI INPUT-OUTPUT AREA                                           
011300                                                                          
011400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
011500 01  DLI-IO-WDK601.                                                       
011600*    03  -COPY WDK601                                                     
011700     EJECT                                                                
011800                                                                          
011900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
012000 01  DLI-IO-WDK611.                                                       
012100*    03  -COPY WDK611                                                     
012200     EJECT                                                                
012300                                                                          
012400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
012500 01  DLI-IO-WDK629.                                                       
012600*    03  -COPY WDK629                                                     
012700     EJECT                                                                
012800                                                                          
012900     EJECT                                                                
013000 LINKAGE SECTION.                                                         
013100                                                                          
013200*01  -COPY W0009   -PRE MSG-                                              
013300                                                                          
013400*01  -COPY W0008  -PRE WDK6-                                              
013500     05  FILLER                  PIC X.                                   
013600     EJECT                                                                
013700 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
013800 MAIN SECTION.                                                            
013900     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
014000                                                                          
014100     SKIP2                                                                
014200     PERFORM A-INIT                                                       
014300     PERFORM S01-LAES-W27236                                              
014400     PERFORM UNTIL END-OF-W27236                                          
014500                                                                          
014600       IF CHKP-ANT > CHKP-MAX                                             
014700         PERFORM X-TAG-CHECKPOINT                                         
014800       END-IF                                                             
014900                                                                          
015000       PERFORM B-BEHANDLA-POSTER                                          
015100                                                                          
015200       PERFORM S01-LAES-W27236                                            
015300     END-PERFORM                                                          
015400                                                                          
015500                                                                          
015600     PERFORM Z-FINIT                                                      
015700                                                                          
015800     MOVE ZERO TO RETURN-CODE                                             
015900     GOBACK                                                               
016000     .                                                                    
016100     EJECT                                                                
016200 A-INIT SECTION.                                                          
016300     SKIP2                                                                
016400                                                                          
016500     PERFORM IMS-RESTART                                                  
016600                                                                          
016700     OPEN INPUT W27236                                                    
016800     .                                                                    
016900     EJECT                                                                
017000 B-BEHANDLA-POSTER SECTION.                                               
017100                                                                          
017200     MOVE IN-IDARTNR TO W-IDARTNR                                         
017300                                                                          
017400     IF IN-FLTILLK = JA                                                   
017500       PERFORM IMS-GHU-WDK611                                             
017600       ADD  IN-KVPB-SEP      TO CLAG-KVPB-SEP                             
017700       MOVE IN-TIPBDAT       TO CLAG-TIPBDAT                              
017800       PERFORM IMS-REPL-WDK611                                            
017900       ADD +1                TO CHKP-ANT                                  
018000       ADD 1                 TO WS-ANTAL-UPD-WDK611                       
018100**                                                                        
018200       PERFORM IMS-GHNP-WDK629                                            
018300       IF CREF-IDPERSON-BUY = 0                                           
018400         MOVE IN-IDPERSON-BUY TO CREF-IDPERSON-BUY                        
018410       END-IF                                                             
018500       MOVE IN-FLWILSON      TO CREF-FLWILSON                             
018600       IF CREF-FLREFBEO = 'J'                                             
018700         MOVE 'N'            TO CREF-FLREFBEO                             
018800       END-IF                                                             
018900       IF CREF-KDREFSTA = 'P'                                             
019000         MOVE 'A'               TO CREF-KDREFSTA                          
019100         MOVE IN-DAGENS-DATUM   TO CREF-TIREFSTA                          
019200       END-IF                                                             
019300       IF CREF-FLREFILL = 'J'                                             
019400         MOVE IN-FLREFILL    TO CREF-FLREFILL                             
019500       END-IF                                                             
019501       IF IN-FLFLYG = 'J' OR 'S'                                          
019502         IF CREF-FLFLYG = 'N'                                             
019510           MOVE IN-FLFLYG    TO CREF-FLFLYG                               
019520         END-IF                                                           
019530       END-IF                                                             
019600       PERFORM IMS-REPL-WDK629                                            
019700       ADD +1                TO CHKP-ANT                                  
019800       ADD 1                 TO WS-ANTAL-UPD-WDK629                       
019900     ELSE                                                                 
020000       PERFORM IMS-GHU-WDK629                                             
020100       MOVE IN-FLPB-FLYTT    TO CREF-FLPB-FLYTT                           
020200       MOVE IN-TIREFSTO      TO CREF-TIREFSTO                             
020300       MOVE IN-FLREFBEO      TO CREF-FLREFBEO                             
020400       PERFORM IMS-REPL-WDK629                                            
020500       ADD +1                TO CHKP-ANT                                  
020600       ADD 1                 TO WS-ANTAL-UPD-WDK629                       
020700     END-IF                                                               
020800     .                                                                    
020900     EJECT                                                                
021000 Z-FINIT SECTION.                                                         
021100                                                                          
021200     CLOSE W27236                                                         
021300     DISPLAY 'ANTAL W27236    : ' WS-ANTAL-W27236                         
021400     DISPLAY 'ANTAL REPL K611 : ' WS-ANTAL-UPD-WDK611                     
021500     DISPLAY 'ANTAL REPL K629 : ' WS-ANTAL-UPD-WDK629                     
021600     .                                                                    
021700     EJECT                                                                
021800 S01-LAES-W27236  SECTION.                                                
021900     SKIP2                                                                
022000     READ W27236 INTO IN-AREA                                             
022100     AT END                                                               
022200        SET END-OF-W27236 TO TRUE                                         
022300                                                                          
022400     NOT AT END                                                           
022500        ADD 1                TO WS-ANTAL-W27236                           
022600     END-READ                                                             
022700     .                                                                    
022800     EJECT                                                                
022900                                                                          
023000 X-TAG-CHECKPOINT   SECTION.                                              
023100                                                                          
023200* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
023300* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
023400     PERFORM IMS-CHECKPOINT                                               
023500     MOVE ZERO TO CHKP-ANT                                                
023600* --- LÄS OM DATABAS OM DET BEHÖVS                                        
023700     .                                                                    
023800     EJECT                                                                
023900* --- IMS SEKTIONER ---                                                   
024000                                                                          
024100     EJECT                                                                
024200 IMS-GHU-WDK611 SECTION.                                                  
024300                                                                          
024400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
024500          DELIMITED BY SIZE INTO SSA1                                     
024600     MOVE 'WDK611  '         TO SSA2                                      
024700     MOVE '  ' TO GODK-STATUSKODER                                        
024800     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
024900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025000     PERFORM IMS-STATUSKONTROLL                                           
025100     .                                                                    
025200     EJECT                                                                
025300 IMS-REPL-WDK611 SECTION.                                                 
025400                                                                          
025500     MOVE '  ' TO GODK-STATUSKODER                                        
025600     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
025700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025800     PERFORM IMS-STATUSKONTROLL                                           
025900     .                                                                    
026000     EJECT                                                                
026100 IMS-GHNP-WDK629 SECTION.                                                 
026200                                                                          
026300     MOVE 'WDK629  '         TO SSA1                                      
026400     MOVE '  ' TO GODK-STATUSKODER                                        
026500     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK629 SSA1                  
026600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026700     PERFORM IMS-STATUSKONTROLL                                           
026800     .                                                                    
026900     EJECT                                                                
027000 IMS-GHU-WDK629 SECTION.                                                  
027100                                                                          
027200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
027300          DELIMITED BY SIZE INTO SSA1                                     
027400     MOVE 'WDK611  '         TO SSA2                                      
027500     MOVE 'WDK629  '         TO SSA3                                      
027600     MOVE '  ' TO GODK-STATUSKODER                                        
027700     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
027800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027900     PERFORM IMS-STATUSKONTROLL                                           
028000     .                                                                    
028100     EJECT                                                                
028200 IMS-REPL-WDK629 SECTION.                                                 
028300                                                                          
028400     MOVE '  ' TO GODK-STATUSKODER                                        
028500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
028600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
028700     PERFORM IMS-STATUSKONTROLL                                           
028800     .                                                                    
028900     EJECT                                                                
029000 IMS-RESTART SECTION.                                                     
029100     SKIP2                                                                
029200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
029300     MOVE '  ' TO GODK-STATUSKODER                                        
029400     CALL CBLTDLI USING XRST MSG-PCB                                      
029500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
029600                        CHKP-AREA-LENGTH CHKP-AREA                        
029700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029800     PERFORM IMS-STATUSKONTROLL                                           
029900     .                                                                    
030000     EJECT                                                                
030100 IMS-CHECKPOINT SECTION.                                                  
030200     SKIP2                                                                
030300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
030400     MOVE '  XD' TO GODK-STATUSKODER                                      
030500     CALL CBLTDLI USING CHKP MSG-PCB                                      
030600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
030700                        CHKP-AREA-LENGTH CHKP-AREA                        
030800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030900     PERFORM IMS-STATUSKONTROLL                                           
031000                                                                          
031100     IF IMS-EJ-OK                                                         
031200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
031300       DISPLAY FELTEXT                                                    
031400       CALL FELLOG                                                        
031500     END-IF                                                               
031600     .                                                                    
031700     EJECT                                                                
031800 IMS-STATUSKONTROLL SECTION.                                              
031900     SKIP2                                                                
032000     SET STATUS-IX TO 1                                                   
032100     SEARCH GODK-STATUS                                                   
032200       AT END                                                             
032300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032400           DELIMITED BY SIZE INTO FELTEXT                                 
032500         DISPLAY FELTEXT                                                  
032600         CALL FELLOG                                                      
032700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032800         CONTINUE                                                         
032900     END-SEARCH                                                           
033000     .                                                                    

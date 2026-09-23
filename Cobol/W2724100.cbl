000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2724100.                                                
000300 AUTHOR.         ARUP DATTA.                                              
000400 DATE-WRITTEN.   16/12/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        UPDATE OF WDK6 FOR ACTIVATION OF                                 
001000*        REFILL APRTS ON CDC                                              
001100*        PROGRAM UPDATES WDK6                                             
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*     --- FILE WITH ARTICLES MARKED FOR ACTIVATION                        
002200     SELECT W27240                     ASSIGN TO W27241D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W27240                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W27240      -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W2724100'.            
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
005300 77  W27240-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W27240                       VALUE 'J'.                   
005500                                                                          
005600     EJECT                                                                
005700 01  ARBETSAREOR.                                                         
005800                                                                          
005900     03 WS-ANTAL-W27240          PIC 9(8)   VALUE ZERO.                   
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
008300*01  AREA -COPY W27240     -PRE IN-                                       
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
014300     PERFORM S01-READ-W27240                                              
014400     PERFORM UNTIL END-OF-W27240                                          
014500                                                                          
014600       IF CHKP-ANT > CHKP-MAX                                             
014700         PERFORM X-TAG-CHECKPOINT                                         
014800       END-IF                                                             
014900                                                                          
015000       PERFORM B-PROCESS-LINE                                             
015100                                                                          
015200       PERFORM S01-READ-W27240                                            
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
016700     OPEN INPUT W27240                                                    
016800     .                                                                    
016900     EJECT                                                                
017000 B-PROCESS-LINE    SECTION.                                               
017100                                                                          
017200     MOVE IN-IDARTNR             TO W-IDARTNR                             
017300                                                                          
017400     PERFORM IMS-GHU-WDK629                                               
017500     IF SEGMENT-FINNS                                                     
017600                                                                          
017700        MOVE IN-TIREFSTA         TO CREF-TIREFSTA                         
017800        MOVE 'A'                 TO CREF-KDREFSTA                         
017900*---                                                                      
018000*--- IF CREF-FLREFBEO = S, THEN IT WILL REMAIN SO                         
018100*---                                                                      
018200                                                                          
018300        IF CREF-FLREFBEO = JA                                             
018400           MOVE NEJ              TO CREF-FLREFBEO                         
018500        END-IF                                                            
018600                                                                          
018700        IF CREF-FLPB-FLYTT = 'N'                                          
018800           PERFORM IMS-REPL-WDK629                                        
018900           ADD +1                TO CHKP-ANT                              
019000                                    WS-ANTAL-UPD-WDK629                   
019100        END-IF                                                            
019200     END-IF                                                               
019300                                                                          
019400     .                                                                    
019500     EJECT                                                                
019600 Z-FINIT SECTION.                                                         
019700                                                                          
019800     CLOSE W27240                                                         
019900     DISPLAY 'ANTAL W27240    : ' WS-ANTAL-W27240                         
020000     DISPLAY 'ANTAL REPL K629 : ' WS-ANTAL-UPD-WDK629                     
020100     .                                                                    
020200     EJECT                                                                
020300 S01-READ-W27240  SECTION.                                                
020400     SKIP2                                                                
020500     READ W27240 INTO IN-AREA                                             
020600     AT END                                                               
020700        SET END-OF-W27240 TO TRUE                                         
020800                                                                          
020900     NOT AT END                                                           
021000        ADD 1                TO WS-ANTAL-W27240                           
021100     END-READ                                                             
021200     .                                                                    
021300     EJECT                                                                
021400                                                                          
021500 X-TAG-CHECKPOINT   SECTION.                                              
021600                                                                          
021700* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
021800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
021900     PERFORM IMS-CHECKPOINT                                               
022000     MOVE ZERO TO CHKP-ANT                                                
022100* --- LÄS OM DATABAS OM DET BEHÖVS                                        
022200     .                                                                    
022300     EJECT                                                                
022400* --- IMS SEKTIONER ---                                                   
022500                                                                          
022600     EJECT                                                                
022700 IMS-GHU-WDK629 SECTION.                                                  
022800                                                                          
022900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
023000          DELIMITED BY SIZE INTO SSA1                                     
023100     MOVE 'WDK611  '          TO SSA2                                     
023200     MOVE 'WDK629  '          TO SSA3                                     
023300     MOVE '  ' TO GODK-STATUSKODER                                        
023400     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
023500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
023600     PERFORM IMS-STATUSKONTROLL                                           
023700     .                                                                    
023800     EJECT                                                                
023900 IMS-REPL-WDK629 SECTION.                                                 
024000                                                                          
024100     MOVE '  ' TO GODK-STATUSKODER                                        
024200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
024300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
024400     PERFORM IMS-STATUSKONTROLL                                           
024500     .                                                                    
024600     EJECT                                                                
024700 IMS-RESTART SECTION.                                                     
024800     SKIP2                                                                
024900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
025000     MOVE '  ' TO GODK-STATUSKODER                                        
025100     CALL CBLTDLI USING XRST MSG-PCB                                      
025200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025300                        CHKP-AREA-LENGTH CHKP-AREA                        
025400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025500     PERFORM IMS-STATUSKONTROLL                                           
025600     .                                                                    
025700     EJECT                                                                
025800 IMS-CHECKPOINT SECTION.                                                  
025900     SKIP2                                                                
026000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026100     MOVE '  XD' TO GODK-STATUSKODER                                      
026200     CALL CBLTDLI USING CHKP MSG-PCB                                      
026300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026400                        CHKP-AREA-LENGTH CHKP-AREA                        
026500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026600     PERFORM IMS-STATUSKONTROLL                                           
026700                                                                          
026800     IF IMS-EJ-OK                                                         
026900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
027000       DISPLAY FELTEXT                                                    
027100       CALL FELLOG                                                        
027200     END-IF                                                               
027300     .                                                                    
027400     EJECT                                                                
027500 IMS-STATUSKONTROLL SECTION.                                              
027600     SKIP2                                                                
027700     SET STATUS-IX TO 1                                                   
027800     SEARCH GODK-STATUS                                                   
027900       AT END                                                             
028000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
028100           DELIMITED BY SIZE INTO FELTEXT                                 
028200         DISPLAY FELTEXT                                                  
028300         CALL FELLOG                                                      
028400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028500         CONTINUE                                                         
028600     END-SEARCH                                                           
028700     .                                                                    

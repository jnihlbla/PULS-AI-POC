000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2722100.                                                
000300 AUTHOR.         JOHAN NIHLBLAD.                                          
000400 DATE-WRITTEN.   15/10/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR REFILPUNKTER                                          
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
002200     SELECT W27220                     ASSIGN TO W27221D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W27220                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W27220      -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W2722100'.            
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
005300 77  W27220-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W27220                       VALUE 'J'.                   
005500                                                                          
005600     EJECT                                                                
005700 01  ARBETSAREOR.                                                         
005800                                                                          
005900     03 WS-ANTAL-W27220          PIC 9(8)   VALUE ZERO.                   
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
008300*01  AREA -COPY W27220     -PRE IN-                                       
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
014300     PERFORM S01-LAES-W27220                                              
014400     PERFORM UNTIL END-OF-W27220                                          
014500                                                                          
014600       IF CHKP-ANT > CHKP-MAX                                             
014700         PERFORM X-TAG-CHECKPOINT                                         
014800       END-IF                                                             
014900                                                                          
015000       PERFORM B-BEHANDLA-POSTER                                          
015100                                                                          
015200       PERFORM S01-LAES-W27220                                            
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
016700     OPEN INPUT W27220                                                    
016800     .                                                                    
016900     EJECT                                                                
017000 B-BEHANDLA-POSTER SECTION.                                               
017100                                                                          
017200     MOVE IN-IDARTNR TO W-IDARTNR                                         
017300                                                                          
017400     PERFORM IMS-GHU-WDK611                                               
017500     MOVE IN-KVSLAGER        TO CLAG-KVSLAGER                             
017600     MOVE IN-KVREFBER        TO CLAG-KVQ                                  
017700     PERFORM IMS-REPL-WDK611                                              
017800     ADD +1                  TO CHKP-ANT                                  
017900     ADD 1                   TO WS-ANTAL-UPD-WDK611                       
018000**                                                                        
018100     PERFORM IMS-GHNP-K629                                                
018200     MOVE IN-KVREFOVL        TO CREF-KVREFOVL                             
018300     MOVE IN-KVREFPKT        TO CREF-KVREFPKT                             
018400     PERFORM IMS-REPL-WDK629                                              
018600     ADD +1                  TO CHKP-ANT                                  
018700     ADD 1                   TO WS-ANTAL-UPD-WDK629                       
018800     .                                                                    
018900     EJECT                                                                
019000 Z-FINIT SECTION.                                                         
019100                                                                          
019200     CLOSE W27220                                                         
019300     DISPLAY 'ANTAL W27220    : ' WS-ANTAL-W27220                         
019400     DISPLAY 'ANTAL REPL K611 : ' WS-ANTAL-UPD-WDK611                     
019500     DISPLAY 'ANTAL REPL K629 : ' WS-ANTAL-UPD-WDK629                     
019600     .                                                                    
019700     EJECT                                                                
019800 S01-LAES-W27220  SECTION.                                                
019900     SKIP2                                                                
020000     READ W27220 INTO IN-AREA                                             
020100     AT END                                                               
020200        SET END-OF-W27220 TO TRUE                                         
020300                                                                          
020400     NOT AT END                                                           
020500        ADD 1                TO WS-ANTAL-W27220                           
020600     END-READ                                                             
020700     .                                                                    
020800     EJECT                                                                
020900                                                                          
021000 X-TAG-CHECKPOINT   SECTION.                                              
021100                                                                          
021200* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
021300* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
021400     PERFORM IMS-CHECKPOINT                                               
021500     MOVE ZERO TO CHKP-ANT                                                
021600* --- LÄS OM DATABAS OM DET BEHÖVS                                        
021700     .                                                                    
021800     EJECT                                                                
021900* --- IMS SEKTIONER ---                                                   
022000                                                                          
022100     EJECT                                                                
022200 IMS-GHU-WDK611 SECTION.                                                  
022300                                                                          
022400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
022500          DELIMITED BY SIZE INTO SSA1                                     
022600     MOVE 'WDK611  '         TO SSA2                                      
022700     MOVE '  ' TO GODK-STATUSKODER                                        
022800     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
022900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
023000     PERFORM IMS-STATUSKONTROLL                                           
023100     .                                                                    
023200     EJECT                                                                
023300 IMS-REPL-WDK611 SECTION.                                                 
023400                                                                          
023500     MOVE '  ' TO GODK-STATUSKODER                                        
023600     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
023700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
023800     PERFORM IMS-STATUSKONTROLL                                           
023900     .                                                                    
024000     EJECT                                                                
024100 IMS-GHNP-K629 SECTION.                                                   
024200                                                                          
024300     MOVE 'WDK629  '         TO SSA1                                      
024400     MOVE '  ' TO GODK-STATUSKODER                                        
024500     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK629 SSA1                  
024600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
024700     PERFORM IMS-STATUSKONTROLL                                           
024800     .                                                                    
024900     EJECT                                                                
025000 IMS-REPL-WDK629 SECTION.                                                 
025100                                                                          
025200     MOVE '  ' TO GODK-STATUSKODER                                        
025300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
025400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025500     PERFORM IMS-STATUSKONTROLL                                           
025600     .                                                                    
025700     EJECT                                                                
025800 IMS-RESTART SECTION.                                                     
025900     SKIP2                                                                
026000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026100     MOVE '  ' TO GODK-STATUSKODER                                        
026200     CALL CBLTDLI USING XRST MSG-PCB                                      
026300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026400                        CHKP-AREA-LENGTH CHKP-AREA                        
026500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026600     PERFORM IMS-STATUSKONTROLL                                           
026700     .                                                                    
026800     EJECT                                                                
026900 IMS-CHECKPOINT SECTION.                                                  
027000     SKIP2                                                                
027100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027200     MOVE '  XD' TO GODK-STATUSKODER                                      
027300     CALL CBLTDLI USING CHKP MSG-PCB                                      
027400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027500                        CHKP-AREA-LENGTH CHKP-AREA                        
027600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027700     PERFORM IMS-STATUSKONTROLL                                           
027800                                                                          
027900     IF IMS-EJ-OK                                                         
028000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
028100       DISPLAY FELTEXT                                                    
028200       CALL FELLOG                                                        
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 IMS-STATUSKONTROLL SECTION.                                              
028700     SKIP2                                                                
028800     SET STATUS-IX TO 1                                                   
028900     SEARCH GODK-STATUS                                                   
029000       AT END                                                             
029100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
029200           DELIMITED BY SIZE INTO FELTEXT                                 
029300         DISPLAY FELTEXT                                                  
029400         CALL FELLOG                                                      
029500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
029600         CONTINUE                                                         
029700     END-SEARCH                                                           
029800     .                                                                    

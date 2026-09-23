000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W2724900.                                                
000300 AUTHOR.         JOHAN NIHLBLAD.                                          
000401 DATE-WRITTEN.   16/12/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000901*        UPPDATERAR VÄRDEN PÅ WDK6 VID PASSIVERING                        
001001*        AV REFILLARTIKEL PÅ CDC                                          
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
002201     SELECT W27242                     ASSIGN TO W27249D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002801 FD  W27242                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003201*01  -COPY W27242      -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003801 77  IDPGM                       PIC X(8)    VALUE 'W2724900'.            
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
005401     88  END-OF-W27242                       VALUE 'J'.                   
005500                                                                          
005600     EJECT                                                                
005700 01  ARBETSAREOR.                                                         
005800                                                                          
005901     03 WS-ANTAL-W27242          PIC 9(8)   VALUE ZERO.                   
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
008300*01  AREA -COPY W27242     -PRE IN-                                       
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
014301     PERFORM S01-LAES-W27242                                              
014401     PERFORM UNTIL END-OF-W27242                                          
014500                                                                          
014600       IF CHKP-ANT > CHKP-MAX                                             
014700         PERFORM X-TAG-CHECKPOINT                                         
014800       END-IF                                                             
014900                                                                          
015000       PERFORM B-BEHANDLA-POSTER                                          
015100                                                                          
015201       PERFORM S01-LAES-W27242                                            
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
016701     OPEN INPUT W27242                                                    
016800     .                                                                    
016900     EJECT                                                                
017000 B-BEHANDLA-POSTER SECTION.                                               
017100                                                                          
017200     MOVE IN-IDARTNR TO W-IDARTNR                                         
017300                                                                          
017400     PERFORM IMS-GHU-WDK611                                               
017502     MOVE +0           TO CLAG-KVPB-SEP                                   
017602                          CLAG-TIPBDAT                                    
017800     PERFORM IMS-REPL-WDK611                                              
017900     ADD +1                  TO CHKP-ANT                                  
018000     ADD 1                   TO WS-ANTAL-UPD-WDK611                       
018100**                                                                        
018200     PERFORM IMS-GHNP-K629                                                
018502     MOVE IN-TIREFSTA  TO CREF-TIREFSTA                                   
018602     MOVE 'P'          TO CREF-KDREFSTA                                   
018802     MOVE 'N'          TO CREF-FLREFNYO                                   
018902     MOVE +0           TO CREF-KVREFOVL                                   
019002                          CREF-KVREFPKT                                   
019102                          CREF-TIREFPAF                                   
019202                          CREF-TIREFPKT                                   
019302                          CREF-TIREFSTO                                   
019303     IF CREF-FLREFBEO = 'S'                                               
019304       CONTINUE                                                           
019305     ELSE                                                                 
019306       MOVE 'N'        TO CREF-FLREFBEO                                   
019307     END-IF                                                               
019400     PERFORM IMS-REPL-WDK629                                              
019500     ADD +1                  TO CHKP-ANT                                  
019600     ADD 1                   TO WS-ANTAL-UPD-WDK629                       
019700     .                                                                    
019800     EJECT                                                                
020400 Z-FINIT SECTION.                                                         
020500                                                                          
020601     CLOSE W27242                                                         
020701     DISPLAY 'ANTAL W27242    : ' WS-ANTAL-W27242                         
020800     DISPLAY 'ANTAL REPL K611 : ' WS-ANTAL-UPD-WDK611                     
020900     DISPLAY 'ANTAL REPL K629 : ' WS-ANTAL-UPD-WDK629                     
021000     .                                                                    
021100     EJECT                                                                
021201 S01-LAES-W27242  SECTION.                                                
021300     SKIP2                                                                
021401     READ W27242 INTO IN-AREA                                             
021500     AT END                                                               
021601        SET END-OF-W27242 TO TRUE                                         
021700                                                                          
021800     NOT AT END                                                           
021901        ADD 1                TO WS-ANTAL-W27242                           
022000     END-READ                                                             
022100     .                                                                    
022200     EJECT                                                                
022300                                                                          
022400 X-TAG-CHECKPOINT   SECTION.                                              
022500                                                                          
022600* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
022700* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
022800     PERFORM IMS-CHECKPOINT                                               
022900     MOVE ZERO TO CHKP-ANT                                                
023000* --- LÄS OM DATABAS OM DET BEHÖVS                                        
023100     .                                                                    
023200     EJECT                                                                
023300* --- IMS SEKTIONER ---                                                   
023400                                                                          
023500     EJECT                                                                
023600 IMS-GHU-WDK611 SECTION.                                                  
023700                                                                          
023800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
023900          DELIMITED BY SIZE INTO SSA1                                     
024000     MOVE 'WDK611  '         TO SSA2                                      
024100     MOVE '  ' TO GODK-STATUSKODER                                        
024200     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
024300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
024400     PERFORM IMS-STATUSKONTROLL                                           
024500     .                                                                    
024600     EJECT                                                                
024700 IMS-REPL-WDK611 SECTION.                                                 
024800                                                                          
024900     MOVE '  ' TO GODK-STATUSKODER                                        
025000     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
025100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025200     PERFORM IMS-STATUSKONTROLL                                           
025300     .                                                                    
025400     EJECT                                                                
025500 IMS-GHNP-K629 SECTION.                                                   
025600                                                                          
025700     MOVE 'WDK629  '         TO SSA1                                      
025800     MOVE '  ' TO GODK-STATUSKODER                                        
025900     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK629 SSA1                  
026000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026100     PERFORM IMS-STATUSKONTROLL                                           
026200     .                                                                    
026300     EJECT                                                                
026400 IMS-REPL-WDK629 SECTION.                                                 
026500                                                                          
026600     MOVE '  ' TO GODK-STATUSKODER                                        
026700     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
026800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100     EJECT                                                                
027200 IMS-RESTART SECTION.                                                     
027300     SKIP2                                                                
027400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027500     MOVE '  ' TO GODK-STATUSKODER                                        
027600     CALL CBLTDLI USING XRST MSG-PCB                                      
027700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027800                        CHKP-AREA-LENGTH CHKP-AREA                        
027900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028000     PERFORM IMS-STATUSKONTROLL                                           
028100     .                                                                    
028200     EJECT                                                                
028300 IMS-CHECKPOINT SECTION.                                                  
028400     SKIP2                                                                
028500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
028600     MOVE '  XD' TO GODK-STATUSKODER                                      
028700     CALL CBLTDLI USING CHKP MSG-PCB                                      
028800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
028900                        CHKP-AREA-LENGTH CHKP-AREA                        
029000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029100     PERFORM IMS-STATUSKONTROLL                                           
029200                                                                          
029300     IF IMS-EJ-OK                                                         
029400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
029500       DISPLAY FELTEXT                                                    
029600       CALL FELLOG                                                        
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 IMS-STATUSKONTROLL SECTION.                                              
030100     SKIP2                                                                
030200     SET STATUS-IX TO 1                                                   
030300     SEARCH GODK-STATUS                                                   
030400       AT END                                                             
030500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030600           DELIMITED BY SIZE INTO FELTEXT                                 
030700         DISPLAY FELTEXT                                                  
030800         CALL FELLOG                                                      
030900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031000         CONTINUE                                                         
032000     END-SEARCH                                                           
040000     .                                                                    

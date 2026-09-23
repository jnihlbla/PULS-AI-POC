000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2718000.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   99/09/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR BUYERKOD SAMT REFILLKÖPTABELL                         
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDK7                                       
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
002200     SELECT W27179                     ASSIGN TO W27180D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W27179                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W27179      -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W2718000'.            
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
005600 77  W27179-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W27179                       VALUE 'J'.                   
005800                                                                          
005900     EJECT                                                                
006000 01  ARBETSAREOR.                                                         
006100                                                                          
006200     03 W-IDARTNR-SPAR           PIC S9(9)  VALUE ZERO COMP-3.            
006300     03 WS-ANTAL-W27179          PIC 9(8)   VALUE ZERO.                   
006400     03 WS-ANTAL-UPD-WDK7        PIC 9(8)   VALUE ZERO.                   
006500     EJECT                                                                
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200*    --- VALID DC CODES                                                   
008300*                                                                         
008400*01  -COPY WWDC99                                                         
008500     EJECT                                                                
008600 01  IN-AREA-START               PIC X(24)   VALUE                        
008700                                             'IN-AREA-START'.             
008800     SKIP2                                                                
008900                                                                          
009000*01  AREA -COPY W27179     -PRE IN-                                       
009100*                                                                         
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009400     SKIP3                                                                
009500 01  NYCKLAR-TILL-DLI.                                                    
009600     03  W-IDARTNR-X.                                                     
009700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009800     03  W-KDSEGKEY-X.                                                    
009900         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
010000     03  W-IDDC-X.                                                        
010100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010200     SKIP2                                                                
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FINNS                       VALUE '  '.                  
010600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010900     88  IMS-EJ-OK                           VALUE 'XD'.                  
011000     SKIP2                                                                
011100 01  GODK-STATUSKODER.                                                    
011200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(64).                               
011500 01  SSA2                        PIC X(64).                               
011600     EJECT                                                                
011700*    --- IMS FUNKTIONSKODER                                               
011800*01  -COPY W0003                                                          
011900     EJECT                                                                
012000*    ---  DLI INPUT-OUTPUT AREA                                           
012100                                                                          
012200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
012300 01  DLI-IO-WDK701.                                                       
012400*    03  -COPY WDK701                                                     
012500     EJECT                                                                
012600                                                                          
012700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
012800 01  DLI-IO-WDK711.                                                       
012900*    03  -COPY WDK711                                                     
013000     EJECT                                                                
013100     EJECT                                                                
013200 LINKAGE SECTION.                                                         
013300                                                                          
013400*01  -COPY W0009   -PRE MSG-                                              
013500                                                                          
013600*01  -COPY W0008  -PRE WDK7-                                              
013700     05  FILLER                  PIC X.                                   
013800     EJECT                                                                
013900 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB.                              
014000 MAIN SECTION.                                                            
014100     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB.                              
014200                                                                          
014300     SKIP2                                                                
014400     PERFORM A-INIT                                                       
014500     PERFORM S01-LAES-W27179                                              
014600     PERFORM UNTIL END-OF-W27179                                          
014700                                                                          
014800       IF CHKP-ANT > CHKP-MAX                                             
014900         PERFORM X-TAG-CHECKPOINT                                         
015000       END-IF                                                             
015100                                                                          
015200       PERFORM B-BEHANDLA-POSTER                                          
015300                                                                          
015400       PERFORM S01-LAES-W27179                                            
015500     END-PERFORM                                                          
015600                                                                          
015700                                                                          
015800     PERFORM Z-FINIT                                                      
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500     SKIP2                                                                
016600                                                                          
016700     PERFORM IMS-RESTART                                                  
016800                                                                          
016900     OPEN INPUT W27179                                                    
017000     .                                                                    
017100     EJECT                                                                
017200 B-BEHANDLA-POSTER SECTION.                                               
017300                                                                          
017500     MOVE IN-IDARTNR TO W-IDARTNR                                         
017600     MOVE IN-IDDC    TO W-IDDC                                            
017700                        WS-IDDC                                           
017800                                                                          
017900     PERFORM IMS-GHU-K711                                                 
018000     IF IN-IDPERSON-BUY NOT = SLAG-IDPERSON-BUY                           
018200     OR IN-IDREFTAB NOT = SLAG-IDREFTAB                                   
018300                                                                          
018500        IF IN-FLBUYUPD = 'N'                                              
018800          MOVE IN-IDPERSON-BUY                                            
018900                             TO SLAG-IDPERSON-BUY                         
019100        END-IF                                                            
019200                                                                          
019210        IF IN-FLTABUPD = 'N'                                              
019300          MOVE IN-IDREFTAB   TO SLAG-IDREFTAB                             
019310        END-IF                                                            
019400                                                                          
019500        PERFORM IMS-REPL-WDK7                                             
019600        ADD +1               TO CHKP-ANT                                  
019700        ADD 1                TO WS-ANTAL-UPD-WDK7                         
019800     END-IF                                                               
019900*                                                                         
022000     .                                                                    
022100     EJECT                                                                
022200 Z-FINIT SECTION.                                                         
022300                                                                          
022400     CLOSE W27179                                                         
022500     DISPLAY 'ANTAL W27179    : ' WS-ANTAL-W27179                         
022600     DISPLAY 'ANTAL REPL WDK7 : ' WS-ANTAL-UPD-WDK7                       
022700     .                                                                    
022800     EJECT                                                                
022900 S01-LAES-W27179  SECTION.                                                
023000     SKIP2                                                                
023100     READ W27179 INTO IN-AREA                                             
023200     AT END                                                               
023300        SET END-OF-W27179 TO TRUE                                         
023400                                                                          
023500     NOT AT END                                                           
023600        ADD 1                TO WS-ANTAL-W27179                           
023700     END-READ                                                             
023800     .                                                                    
023900     EJECT                                                                
024000                                                                          
024100 X-TAG-CHECKPOINT   SECTION.                                              
024200                                                                          
024300* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
024400* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
024500     PERFORM IMS-CHECKPOINT                                               
024600     MOVE ZERO TO CHKP-ANT                                                
024700* --- LÄS OM DATABAS OM DET BEHÖVS                                        
024800     .                                                                    
024900     EJECT                                                                
025000* --- IMS SEKTIONER ---                                                   
025100                                                                          
025200     EJECT                                                                
025300 IMS-GU-WDK701 SECTION.                                                   
025400                                                                          
025500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X  ')'                        
025600          DELIMITED BY SIZE INTO SSA1                                     
025700     MOVE 'GE  ' TO GODK-STATUSKODER                                      
025800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
025900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
026000     PERFORM IMS-STATUSKONTROLL                                           
026100     .                                                                    
026200     EJECT                                                                
026300 IMS-GHNP-WDK711 SECTION.                                                 
026400                                                                          
026500     MOVE 'WDK711 ' TO SSA1                                               
026600     MOVE '  GE' TO GODK-STATUSKODER                                      
026700     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1                  
026800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100     EJECT                                                                
027200 IMS-GHU-K711 SECTION.                                                    
027300                                                                          
027400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
027500          DELIMITED BY SIZE INTO SSA1                                     
027600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
027700          DELIMITED BY SIZE INTO SSA2                                     
027800     MOVE '  ' TO GODK-STATUSKODER                                        
027900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
028000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
028100     PERFORM IMS-STATUSKONTROLL                                           
028200     .                                                                    
028300     EJECT                                                                
028400 IMS-REPL-WDK7 SECTION.                                                   
028500                                                                          
028600     MOVE '  ' TO GODK-STATUSKODER                                        
028700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
028800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
028900     PERFORM IMS-STATUSKONTROLL                                           
029000     .                                                                    
029100     EJECT                                                                
029200 IMS-RESTART SECTION.                                                     
029300     SKIP2                                                                
029400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
029500     MOVE '  ' TO GODK-STATUSKODER                                        
029600     CALL CBLTDLI USING XRST MSG-PCB                                      
029700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
029800                        CHKP-AREA-LENGTH CHKP-AREA                        
029900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030000     PERFORM IMS-STATUSKONTROLL                                           
030100     .                                                                    
030200     EJECT                                                                
030300 IMS-CHECKPOINT SECTION.                                                  
030400     SKIP2                                                                
030500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
030600     MOVE '  XD' TO GODK-STATUSKODER                                      
030700     CALL CBLTDLI USING CHKP MSG-PCB                                      
030800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
030900                        CHKP-AREA-LENGTH CHKP-AREA                        
031000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031100     PERFORM IMS-STATUSKONTROLL                                           
031200                                                                          
031300     IF IMS-EJ-OK                                                         
031400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
031500       DISPLAY FELTEXT                                                    
031600       CALL FELLOG                                                        
031700     END-IF                                                               
031800     .                                                                    
031900     EJECT                                                                
032000 IMS-STATUSKONTROLL SECTION.                                              
032100     SKIP2                                                                
032200     SET STATUS-IX TO 1                                                   
032300     SEARCH GODK-STATUS                                                   
032400       AT END                                                             
032500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032600           DELIMITED BY SIZE INTO FELTEXT                                 
032700         DISPLAY FELTEXT                                                  
032800         CALL FELLOG                                                      
032900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033000         CONTINUE                                                         
033100     END-SEARCH                                                           
033200     .                                                                    

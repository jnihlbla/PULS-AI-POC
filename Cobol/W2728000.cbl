000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2728000.                                                
000300 AUTHOR.         JOHAN NIHLBLAD.                                          
000400 DATE-WRITTEN.   15/10/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR BUYERKOD SAMT REFILLKÖPTABELL                         
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
002200     SELECT W27279                     ASSIGN TO W27280D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W27279                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W27179      -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W2728000'.            
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
005300 77  W27279-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W27279                       VALUE 'J'.                   
005500                                                                          
005600     EJECT                                                                
005700 01  ARBETSAREOR.                                                         
005800                                                                          
005900     03 WS-ANTAL-W27279          PIC 9(8)   VALUE ZERO.                   
006000     03 WS-ANTAL-UPD-WDK6        PIC 9(8)   VALUE ZERO.                   
006100     EJECT                                                                
006200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES DAGENS-DATUM.                                       
006400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006700     EJECT                                                                
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900*                                                                         
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL POSTSUM                                          
007500*                                                                         
007600*01  -COPY W0005   -PRE  POSTSUM-                                         
007700     EJECT                                                                
007800 01  IN-AREA-START               PIC X(24)   VALUE                        
007900                                             'IN-AREA-START'.             
008000     SKIP2                                                                
008100                                                                          
008200*01  AREA -COPY W27179     -PRE IN-                                       
008300*                                                                         
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008600     SKIP3                                                                
008700 01  NYCKLAR-TILL-DLI.                                                    
008800     03  W-IDARTNR-X.                                                     
008900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009000     03  W-KDSEGKEY-X.                                                    
009100         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
009200     SKIP2                                                                
009300*    --- STATUS-KOD FRÅN IMS                                              
009400 01  STATUS-WS                   PIC XX.                                  
009500     88  SEGMENT-FINNS                       VALUE '  '.                  
009600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009900     88  IMS-EJ-OK                           VALUE 'XD'.                  
010000     SKIP2                                                                
010100 01  GODK-STATUSKODER.                                                    
010200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010300     SKIP3                                                                
010400 01  SSA1                        PIC X(64).                               
010500 01  SSA2                        PIC X(64).                               
010600 01  SSA3                        PIC X(64).                               
010700     EJECT                                                                
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000     EJECT                                                                
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200                                                                          
011300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
011400 01  DLI-IO-WDK601.                                                       
011500*    03  -COPY WDK601                                                     
011600     EJECT                                                                
011700                                                                          
011800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
011900 01  DLI-IO-WDK611.                                                       
012000*    03  -COPY WDK611                                                     
012100     EJECT                                                                
012200                                                                          
012300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
012400 01  DLI-IO-WDK629.                                                       
012500*    03  -COPY WDK629                                                     
012600     EJECT                                                                
012700                                                                          
012800     EJECT                                                                
012900 LINKAGE SECTION.                                                         
013000                                                                          
013100*01  -COPY W0009   -PRE MSG-                                              
013200                                                                          
013300*01  -COPY W0008  -PRE WDK6-                                              
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
013700 MAIN SECTION.                                                            
013800     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
013900                                                                          
014000     SKIP2                                                                
014100     PERFORM A-INIT                                                       
014200     PERFORM S01-LAES-W27279                                              
014300     PERFORM UNTIL END-OF-W27279                                          
014400                                                                          
014500       IF CHKP-ANT > CHKP-MAX                                             
014600         PERFORM X-TAG-CHECKPOINT                                         
014700       END-IF                                                             
014800                                                                          
014900       PERFORM B-BEHANDLA-POSTER                                          
015000                                                                          
015100       PERFORM S01-LAES-W27279                                            
015200     END-PERFORM                                                          
015300                                                                          
015400                                                                          
015500     PERFORM Z-FINIT                                                      
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT SECTION.                                                          
016200     SKIP2                                                                
016300                                                                          
016400     PERFORM IMS-RESTART                                                  
016500                                                                          
016600     OPEN INPUT W27279                                                    
016700     .                                                                    
016800     EJECT                                                                
016900 B-BEHANDLA-POSTER SECTION.                                               
017000                                                                          
017100     MOVE IN-IDARTNR TO W-IDARTNR                                         
017200                                                                          
017300     PERFORM IMS-GHU-K629                                                 
017400     IF SEGMENT-FINNS                                                     
017500       IF IN-IDPERSON-BUY NOT = CREF-IDPERSON-BUY                         
017700       OR IN-IDREFTAB NOT = CREF-IDREFTAB                                 
017800                                                                          
018420          IF IN-FLBUYUPD = 'N'                                            
018430            MOVE IN-IDPERSON-BUY                                          
018440                             TO CREF-IDPERSON-BUY                         
018450          END-IF                                                          
018470                                                                          
018480          IF IN-FLTABUPD = 'N'                                            
018490            MOVE IN-IDREFTAB TO CREF-IDREFTAB                             
018491          END-IF                                                          
018492                                                                          
018500                                                                          
018600          PERFORM IMS-REPL-WDK6                                           
018700          ADD +1             TO CHKP-ANT                                  
018800          ADD 1              TO WS-ANTAL-UPD-WDK6                         
018900       END-IF                                                             
018910     END-IF                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 Z-FINIT SECTION.                                                         
019300                                                                          
019400     CLOSE W27279                                                         
019500     DISPLAY 'ANTAL W27279    : ' WS-ANTAL-W27279                         
019600     DISPLAY 'ANTAL REPL WDK6 : ' WS-ANTAL-UPD-WDK6                       
019700     .                                                                    
019800     EJECT                                                                
019900 S01-LAES-W27279  SECTION.                                                
020000     SKIP2                                                                
020100     READ W27279 INTO IN-AREA                                             
020200     AT END                                                               
020300        SET END-OF-W27279 TO TRUE                                         
020400                                                                          
020500     NOT AT END                                                           
020600        ADD 1                TO WS-ANTAL-W27279                           
020700     END-READ                                                             
020800     .                                                                    
020900     EJECT                                                                
021000                                                                          
021100 X-TAG-CHECKPOINT   SECTION.                                              
021200                                                                          
021300* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
021400* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
021500     PERFORM IMS-CHECKPOINT                                               
021600     MOVE ZERO TO CHKP-ANT                                                
021700* --- LÄS OM DATABAS OM DET BEHÖVS                                        
021800     .                                                                    
021900     EJECT                                                                
022000* --- IMS SEKTIONER ---                                                   
022100                                                                          
022200     EJECT                                                                
022300 IMS-GHU-K629 SECTION.                                                    
022400                                                                          
022500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
022600          DELIMITED BY SIZE INTO SSA1                                     
022700     MOVE 'WDK611  '         TO SSA2                                      
022800     MOVE 'WDK629  '         TO SSA3                                      
022900     MOVE '  GE' TO GODK-STATUSKODER                                      
023000     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
023100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
023200     PERFORM IMS-STATUSKONTROLL                                           
023300     .                                                                    
023400     EJECT                                                                
023500 IMS-REPL-WDK6 SECTION.                                                   
023600                                                                          
023700     MOVE '  ' TO GODK-STATUSKODER                                        
023800     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
023900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
024000     PERFORM IMS-STATUSKONTROLL                                           
024100     .                                                                    
024200     EJECT                                                                
024300 IMS-RESTART SECTION.                                                     
024400     SKIP2                                                                
024500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024600     MOVE '  ' TO GODK-STATUSKODER                                        
024700     CALL CBLTDLI USING XRST MSG-PCB                                      
024800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024900                        CHKP-AREA-LENGTH CHKP-AREA                        
025000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025100     PERFORM IMS-STATUSKONTROLL                                           
025200     .                                                                    
025300     EJECT                                                                
025400 IMS-CHECKPOINT SECTION.                                                  
025500     SKIP2                                                                
025600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
025700     MOVE '  XD' TO GODK-STATUSKODER                                      
025800     CALL CBLTDLI USING CHKP MSG-PCB                                      
025900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026000                        CHKP-AREA-LENGTH CHKP-AREA                        
026100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026200     PERFORM IMS-STATUSKONTROLL                                           
026300                                                                          
026400     IF IMS-EJ-OK                                                         
026500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
026600       DISPLAY FELTEXT                                                    
026700       CALL FELLOG                                                        
026800     END-IF                                                               
026900     .                                                                    
027000     EJECT                                                                
027100 IMS-STATUSKONTROLL SECTION.                                              
027200     SKIP2                                                                
027300     SET STATUS-IX TO 1                                                   
027400     SEARCH GODK-STATUS                                                   
027500       AT END                                                             
027600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
027700           DELIMITED BY SIZE INTO FELTEXT                                 
027800         DISPLAY FELTEXT                                                  
027900         CALL FELLOG                                                      
028000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028100         CONTINUE                                                         
028200     END-SEARCH                                                           
028300     .                                                                    

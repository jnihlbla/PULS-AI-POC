000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2713C00.                                                
000400*AUTHOR.         ANNELIE ENGLUND.                                         
000500*DATE-WRITTEN.   94/12/14.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR FÖRÄNDRAD PROGNOS FÖR REFILL -OI PÅ WDK7              
001100*        INGET ÅTERSTARSREGISTER BEHÖVS, VID                              
001200*        ABEND KOLLAS OM PROGNOSEN(KVPBREOI) PÅ WDK7                      
001300*        FÖRÄNDRATS I FÖRHÅLLANDE TILL FILEN                              
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- PROGNOS SOM SKA FÖRÄNDRAS                                  
003000     SELECT W27134                     ASSIGN TO W2713CD1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W27134                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W27134      -L.                                                
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W2713C00'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
005000     SKIP2                                                                
005100 01  CHKP-VAR.                                                            
005200 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005300 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005400 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005500 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005600 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005700 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005800                                                                          
005900                                                                          
006000                                                                          
006100 01  FELTEXT.                                                             
006200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400                                                                          
006500 77  W27134-EOF-SW               PIC X       VALUE 'N'.                   
006600     88  END-OF-W27134                       VALUE 'J'.                   
006700                                                                          
006800 77  K7-SW                       PIC X       VALUE 'J'.                   
006900     88  K7-FINNS                            VALUE 'J'.                   
007000     EJECT                                                                
007100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007200 01  FILLER REDEFINES DAGENS-DATUM.                                       
007300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007600     EJECT                                                                
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800*                                                                         
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL POSTSUM                                          
008400*                                                                         
008500*01  -COPY W0005   -PRE  POSTSUM-                                         
008600     EJECT                                                                
008700 01  IN-AREA-START               PIC X(24)   VALUE                        
008800                                             'IN-AREA-START'.             
008900     SKIP2                                                                
009000                                                                          
009100*01  AREA -COPY W27134     -PRE IN-                                       
009200*                                                                         
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009500     SKIP3                                                                
009600 01  NYCKLAR-TILL-DLI.                                                    
009700     03  W-IDARTNR-X.                                                     
009800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009900     03  W-IDDC-X.                                                        
010000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010100     SKIP2                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FINNS                       VALUE '  '.                  
010500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010800     88  IMS-EJ-OK                           VALUE 'XD'.                  
010900     SKIP2                                                                
011000 01  GODK-STATUSKODER.                                                    
011100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011200     SKIP3                                                                
011300 01  SSA1                        PIC X(64).                               
011400 01  SSA2                        PIC X(64).                               
011500     EJECT                                                                
011600*    --- IMS FUNKTIONSKODER                                               
011700*01  -COPY W0003                                                          
011800     EJECT                                                                
011900*    ---  DLI INPUT-OUTPUT AREA                                           
012000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012100     SKIP3                                                                
012200 01  DLI-IO-AREA.                                                         
012300     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
012400     SKIP3                                                                
012500     03  WLARTS01 REDEFINES IO-AREA.                                      
012600*        05  -COPY WDK701                                                 
012700     SKIP3                                                                
012800     03  WLARTS11 REDEFINES IO-AREA.                                      
012900*        05  -COPY WDK711                                                 
013000     EJECT                                                                
013100 LINKAGE SECTION.                                                         
013200                                                                          
013300*01  -COPY W0009   -PRE MSG-                                              
013400     EJECT                                                                
013500*01  -COPY W0008  -PRE ARTS-                                              
013600     05  FILLER                  PIC X.                                   
013700     EJECT                                                                
013800 PROCEDURE DIVISION  USING MSG-PCB ARTS-PCB.                              
013900     ENTRY 'DLITCBL' USING MSG-PCB ARTS-PCB.                              
014000                                                                          
014100     PERFORM A-INIT                                                       
014200     PERFORM S01-LAES-W27134                                              
014300     PERFORM UNTIL END-OF-W27134                                          
014400                                                                          
014500       PERFORM B-BEHANDLA-POSTER                                          
014600                                                                          
014700       PERFORM S01-LAES-W27134                                            
014800                                                                          
014900     END-PERFORM                                                          
015000     PERFORM Z-FINIT                                                      
015100                                                                          
015200     MOVE ZERO TO RETURN-CODE                                             
015300     GOBACK                                                               
015400     .                                                                    
015500     EJECT                                                                
015600 A-INIT SECTION.                                                          
015700     SKIP2                                                                
015800                                                                          
015900     ACCEPT DAGENS-DATUM FROM DATE                                        
016000                                                                          
016100     PERFORM IMS-RESTART                                                  
016200                                                                          
016300     OPEN INPUT W27134                                                    
016400                                                                          
016500                                                                          
016600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016700     .                                                                    
016800     EJECT                                                                
016900 B-BEHANDLA-POSTER SECTION.                                               
017000                                                                          
017100     MOVE IN-IDARTNR TO W-IDARTNR                                         
017200     MOVE IN-IDDC    TO W-IDDC                                            
017300     IF IN-IDARTNR = WS-IDARTNR                                           
017400       CONTINUE                                                           
017500     ELSE                                                                 
017600       IF CHKP-ANT > CHKP-MAX                                             
017700         PERFORM X-TAG-CHECKPOINT                                         
017800       END-IF                                                             
017900       PERFORM IMS-GU-ARTS-WDK711                                         
018000       MOVE W-IDARTNR TO WS-IDARTNR                                       
018100     END-IF                                                               
018200     PERFORM IMS-GHNP-ARTS-WDK711                                         
018300                                                                          
018400     IF IN-KDERS < 20                                                     
018500        IF SLAG-KVPBREOI = IN-KVPBREOI                                    
018700          CONTINUE                                                        
018800        ELSE                                                              
018900          MOVE IN-KVPBREOI TO SLAG-KVPBREOI                               
019000          MOVE IN-RETREND-REOI  TO SLAG-RETREND-REOI                      
019100          PERFORM IMS-REPL-ARTS-WDK711                                    
019200          ADD +1 TO CHKP-ANT                                              
019300        END-IF                                                            
019400     ELSE                                                                 
019500        MOVE ZERO TO    SLAG-KVPBREOI                                     
019610                        SLAG-KVREFPKT                                     
019700                        SLAG-KVREFBER                                     
019800                        SLAG-KVREFOVL                                     
019900        PERFORM IMS-REPL-ARTS-WDK711                                      
020000        ADD +1 TO CHKP-ANT                                                
020100     END-IF                                                               
020200     .                                                                    
020300     EJECT                                                                
020400 Z-FINIT SECTION.                                                         
020500                                                                          
020600     CLOSE W27134                                                         
020700     SKIP2                                                                
020800     MOVE 'S' TO POSTSUM-OPKOD                                            
020900     CALL POSTSUM USING POSTSUM-PARM                                      
021000                                                                          
021100     .                                                                    
021200     EJECT                                                                
021300 S01-LAES-W27134  SECTION.                                                
021400     SKIP2                                                                
021500     READ W27134 INTO IN-AREA                                             
021600     AT END                                                               
021700        SET END-OF-W27134 TO TRUE                                         
021800                                                                          
021900     NOT AT END                                                           
022000        MOVE 'W27134' TO POSTSUM-FDNAMN                                   
022100        MOVE 'W2713CD1' TO POSTSUM-DDNAMN2                                
022200        CALL POSTSUM USING POSTSUM-PARM                                   
022300     END-READ                                                             
022400     .                                                                    
022500     EJECT                                                                
022600 X-TAG-CHECKPOINT   SECTION.                                              
022700                                                                          
022800* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
022900* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
023000     PERFORM IMS-CHECKPOINT                                               
023100     MOVE ZERO TO CHKP-ANT                                                
023200* --- LÄS OM DATABAS OM DET BEHÖVS                                        
023300     .                                                                    
023400     EJECT                                                                
023500* --- IMS SEKTIONER ---                                                   
023600     SKIP3                                                                
023700     EJECT                                                                
023800 IMS-RESTART SECTION.                                                     
023900     SKIP2                                                                
024000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024100     MOVE '  ' TO GODK-STATUSKODER                                        
024200     CALL CBLTDLI USING XRST MSG-PCB                                      
024300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024400                        CHKP-AREA-LENGTH CHKP-AREA                        
024500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024600     PERFORM IMS-STATUSKONTROLL                                           
024700     .                                                                    
024800     EJECT                                                                
024900 IMS-CHECKPOINT SECTION.                                                  
025000     SKIP2                                                                
025100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
025200     MOVE '  XD' TO GODK-STATUSKODER                                      
025300     CALL CBLTDLI USING CHKP MSG-PCB                                      
025400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025500                        CHKP-AREA-LENGTH CHKP-AREA                        
025600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025700     PERFORM IMS-STATUSKONTROLL                                           
025800                                                                          
025900     IF IMS-EJ-OK                                                         
026000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
026100       DISPLAY FELTEXT                                                    
026200       CALL FELLOG                                                        
026300     END-IF                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 IMS-GU-ARTS-WDK711 SECTION.                                              
026700                                                                          
026800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
026900          DELIMITED BY SIZE INTO SSA1                                     
027000     MOVE '  ' TO GODK-STATUSKODER                                        
027100     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1                      
027200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
027300     PERFORM IMS-STATUSKONTROLL                                           
027400     .                                                                    
027500     EJECT                                                                
027600 IMS-GHNP-ARTS-WDK711 SECTION.                                            
027700                                                                          
027800     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
027900          DELIMITED BY SIZE INTO SSA1                                     
028000     MOVE '  ' TO GODK-STATUSKODER                                        
028100     CALL CBLTDLI USING GHNP ARTS-PCB DLI-IO-AREA SSA1                    
028200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
028300     PERFORM IMS-STATUSKONTROLL                                           
028400     .                                                                    
028500     SKIP3                                                                
028600 IMS-REPL-ARTS-WDK711 SECTION.                                            
028700                                                                          
028800     MOVE '  ' TO GODK-STATUSKODER                                        
028900     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA                         
029000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
029100     PERFORM IMS-STATUSKONTROLL                                           
029200     .                                                                    
029300     EJECT                                                                
029400 IMS-STATUSKONTROLL SECTION.                                              
029500     SKIP2                                                                
029600     SET STATUS-IX TO 1                                                   
029700     SEARCH GODK-STATUS                                                   
029800       AT END                                                             
029900         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
030000         DISPLAY FELTEXT                                                  
030100         CALL FELLOG                                                      
030200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030300         CONTINUE                                                         
030400     END-SEARCH                                                           
030500     .                                                                    

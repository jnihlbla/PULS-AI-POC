000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2713300.                                                
000400*AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000500*DATE-WRITTEN.   97/01/23.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR FÖRÄNDRADE SÄSONGSINDEX PÅ WDK7                       
001100*        INGET ÅTERSTARSREGISTER BEHÖVS, VID                              
001200*        ABEND KOLLAS OM SASONGEN PÅ WDK7 ?????????                       
001300*        FÖRÄNDRATS I FÖRHÅLLANDE TILL FILEN. VID FÖRÄNDRAD               
001400*        SÄSONG SLÄCKS RÖRELSEINDIKATORFLAGGAN PÅ WDL7.                   
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
001700*        PROGRAMMET UPPDATERAR WLOIGA (WDL7)                              
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- SASONGSINDEX                                               
003200     SELECT W27133                     ASSIGN TO W27133D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W27133                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST -COPY W27132  -PRE IN-    -L.                                   
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004600                                                                          
004700*    -- CHECKED BY WY2000                                                 
004800 77  IDPGM                       PIC X(8)    VALUE 'W2713300'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
005200 77  WS-ANTAL-WDK711             PIC S9(9)   VALUE ZERO COMP-3.           
005300     SKIP2                                                                
005400 01  CHKP-VAR.                                                            
005500 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005600 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005700 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005800 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005900 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
006000 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
006100                                                                          
006200                                                                          
006300                                                                          
006400 01  FELTEXT.                                                             
006500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006700                                                                          
006800 77  W27133-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W27133                       VALUE 'J'.                   
007000                                                                          
007100 77  K7-SW                       PIC X       VALUE 'J'.                   
007200     88  K7-FINNS                            VALUE 'J'.                   
007300     EJECT                                                                
007400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007500 01  FILLER REDEFINES DAGENS-DATUM.                                       
007600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007900       EJECT                                                              
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100*                                                                         
008200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL POSTSUM                                          
008700*                                                                         
008800*01  -COPY W0005   -PRE  POSTSUM-                                         
008900     EJECT                                                                
009000 01  IN-AREA-START               PIC X(24)   VALUE                        
009100                                             'IN-AREA-START'.             
009200     SKIP2                                                                
009300                                                                          
009400*01  AREA -COPY W27132     -PRE IN-                                       
009500*                                                                         
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009800     SKIP3                                                                
009900 01  NYCKLAR-TILL-DLI.                                                    
010000     03  W-IDARTNR-X.                                                     
010100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010200     03  W-IDDC-X.                                                        
010300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010400     SKIP2                                                                
010500*    --- STATUS-KOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011100     88  IMS-EJ-OK                           VALUE 'XD'.                  
011200     SKIP2                                                                
011300 01  GODK-STATUSKODER.                                                    
011400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700 01  SSA2                        PIC X(64).                               
011800     EJECT                                                                
011900*    --- IMS FUNKTIONSKODER                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA                                           
012300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012400     SKIP3                                                                
012500 01  DLI-IO-AREA.                                                         
012600     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
012700     SKIP3                                                                
012800     03  WLARTS01 REDEFINES IO-AREA.                                      
012900*        05  -COPY WDK701  -PRE ARTS-                                     
013000     SKIP3                                                                
013100     03  WLARTS11 REDEFINES IO-AREA.                                      
013200*        05  -COPY WDK711  -PRE ARTS-                                     
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500                                                                          
013600*01  -COPY W0009   -PRE MSG-                                              
013700     EJECT                                                                
013800*01  -COPY W0008  -PRE ARTS-                                              
013900     05  FILLER                  PIC X.                                   
014000*01  -COPY W0008  -PRE OIGA-                                              
014100     05  FILLER                  PIC X.                                   
014200     EJECT                                                                
014300 PROCEDURE DIVISION  USING MSG-PCB ARTS-PCB OIGA-PCB.                     
014400     ENTRY 'DLITCBL' USING MSG-PCB ARTS-PCB OIGA-PCB.                     
014500                                                                          
014600     PERFORM A-INIT                                                       
014700     PERFORM S01-LAES-W27133                                              
014800     PERFORM UNTIL END-OF-W27133                                          
014900                                                                          
015000       PERFORM B-BEHANDLA-POSTER                                          
015100       PERFORM S01-LAES-W27133                                            
015200                                                                          
015300     END-PERFORM                                                          
015400     PERFORM Z-FINIT                                                      
015500                                                                          
015600     MOVE ZERO TO RETURN-CODE                                             
015700     GOBACK                                                               
015800     .                                                                    
015900     EJECT                                                                
016000 A-INIT SECTION.                                                          
016100     SKIP2                                                                
016200                                                                          
016300     ACCEPT DAGENS-DATUM FROM DATE                                        
016400                                                                          
016500     PERFORM IMS-RESTART                                                  
016600                                                                          
016700     OPEN INPUT W27133                                                    
016800                                                                          
016900                                                                          
017000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017100     .                                                                    
017200     EJECT                                                                
017300 B-BEHANDLA-POSTER SECTION.                                               
017400                                                                          
017500     MOVE IN-IDARTNR TO W-IDARTNR                                         
017600     MOVE IN-IDDC    TO W-IDDC                                            
017700     IF IN-IDARTNR = WS-IDARTNR                                           
017800       CONTINUE                                                           
017900     ELSE                                                                 
018000       IF CHKP-ANT > CHKP-MAX                                             
018100         PERFORM X-TAG-CHECKPOINT                                         
018200       END-IF                                                             
018300       PERFORM IMS-GU-ARTS-WDK711                                         
018400       MOVE W-IDARTNR TO WS-IDARTNR                                       
018500     END-IF                                                               
018600     PERFORM IMS-GHNP-ARTS-WDK711                                         
018700                                                                          
018800     IF  ARTS-SLAG-RESEASON (1)  = IN-RESEASON (1)                        
018900     AND ARTS-SLAG-RESEASON (2)  = IN-RESEASON (2)                        
019000     AND ARTS-SLAG-RESEASON (3)  = IN-RESEASON (3)                        
019100     AND ARTS-SLAG-RESEASON (4)  = IN-RESEASON (4)                        
019200     AND ARTS-SLAG-RESEASON (5)  = IN-RESEASON (5)                        
019300     AND ARTS-SLAG-RESEASON (6)  = IN-RESEASON (6)                        
019400     AND ARTS-SLAG-RESEASON (7)  = IN-RESEASON (7)                        
019500     AND ARTS-SLAG-RESEASON (8)  = IN-RESEASON (8)                        
019600     AND ARTS-SLAG-RESEASON (9)  = IN-RESEASON (9)                        
019700     AND ARTS-SLAG-RESEASON (10) = IN-RESEASON (10)                       
019800     AND ARTS-SLAG-RESEASON (11) = IN-RESEASON (11)                       
019900     AND ARTS-SLAG-RESEASON (12) = IN-RESEASON (12)                       
020000       IF ARTS-SLAG-TIMANSEA > ZERO                                       
020100       OR ARTS-SLAG-DASPSEA  > ZERO                                       
020200         MOVE ZERO           TO ARTS-SLAG-DASPSEA                         
020300                                ARTS-SLAG-TIMANSEA                        
020400         IF ARTS-SLAG-FLREFNYO = JA                                       
020500            MOVE NEJ         TO ARTS-SLAG-FLREFNYO                        
020600         END-IF                                                           
020700         PERFORM IMS-REPL-ARTS-WDK711                                     
020800                                                                          
020900         ADD +1 TO CHKP-ANT                                               
021000       END-IF                                                             
021100     ELSE                                                                 
021200                                                                          
021300       IF     (ARTS-SLAG-RESEASON (1) = 1.00                              
021400       AND     ARTS-SLAG-RESEASON (2) = 1.00                              
021500       AND     ARTS-SLAG-RESEASON (3) = 1.00                              
021600       AND     ARTS-SLAG-RESEASON (4) = 1.00                              
021700       AND     ARTS-SLAG-RESEASON (5) = 1.00                              
021800       AND     ARTS-SLAG-RESEASON (6) = 1.00                              
021900       AND     ARTS-SLAG-RESEASON (7) = 1.00                              
022000       AND     ARTS-SLAG-RESEASON (8) = 1.00                              
022100       AND     ARTS-SLAG-RESEASON (9) = 1.00                              
022200       AND     ARTS-SLAG-RESEASON (10) = 1.00                             
022300       AND     ARTS-SLAG-RESEASON (11) = 1.00                             
022400       AND     ARTS-SLAG-RESEASON (12) = 1.00)                            
022500       AND    (IN-RESEASON (1) < 0.80                                     
022600       OR      IN-RESEASON (2) < 0.80                                     
022700       OR      IN-RESEASON (3) < 0.80                                     
022800       OR      IN-RESEASON (4) < 0.80                                     
022900       OR      IN-RESEASON (5) < 0.80                                     
023000       OR      IN-RESEASON (6) < 0.80                                     
023100       OR      IN-RESEASON (7) < 0.80                                     
023200       OR      IN-RESEASON (8) < 0.80                                     
023300       OR      IN-RESEASON (9) < 0.80                                     
023400       OR      IN-RESEASON (10) < 0.80                                    
023500       OR      IN-RESEASON (11) < 0.80                                    
023600       OR      IN-RESEASON (12) < 0.80                                    
023700       OR      IN-RESEASON (1) > 1.20                                     
023800       OR      IN-RESEASON (2) > 1.20                                     
023900       OR      IN-RESEASON (3) > 1.20                                     
024000       OR      IN-RESEASON (4) > 1.20                                     
024100       OR      IN-RESEASON (5) > 1.20                                     
024200       OR      IN-RESEASON (6) > 1.20                                     
024300       OR      IN-RESEASON (7) > 1.20                                     
024400       OR      IN-RESEASON (8) > 1.20                                     
024500       OR      IN-RESEASON (9) > 1.20                                     
024600       OR      IN-RESEASON (10) > 1.20                                    
024700       OR      IN-RESEASON (11) > 1.20                                    
024800       OR      IN-RESEASON (12) > 1.20)                                   
024900         IF ARTS-SLAG-FLREFBEO = JA                                       
025000           MOVE NEJ          TO ARTS-SLAG-FLREFBEO                        
025100         END-IF                                                           
025200       END-IF                                                             
025300                                                                          
025400       MOVE IN-RESEASON (1)  TO ARTS-SLAG-RESEASON (1)                    
025500       MOVE IN-RESEASON (2)  TO ARTS-SLAG-RESEASON (2)                    
025600       MOVE IN-RESEASON (3)  TO ARTS-SLAG-RESEASON (3)                    
025700       MOVE IN-RESEASON (4)  TO ARTS-SLAG-RESEASON (4)                    
025800       MOVE IN-RESEASON (5)  TO ARTS-SLAG-RESEASON (5)                    
025900       MOVE IN-RESEASON (6)  TO ARTS-SLAG-RESEASON (6)                    
026000       MOVE IN-RESEASON (7)  TO ARTS-SLAG-RESEASON (7)                    
026100       MOVE IN-RESEASON (8)  TO ARTS-SLAG-RESEASON (8)                    
026200       MOVE IN-RESEASON (9)  TO ARTS-SLAG-RESEASON (9)                    
026300       MOVE IN-RESEASON (10) TO ARTS-SLAG-RESEASON (10)                   
026400       MOVE IN-RESEASON (11) TO ARTS-SLAG-RESEASON (11)                   
026500       MOVE IN-RESEASON (12) TO ARTS-SLAG-RESEASON (12)                   
026600       MOVE ZERO             TO ARTS-SLAG-DASPSEA                         
026700                                ARTS-SLAG-TIMANSEA                        
026800                                                                          
026900* --   VID ÄNDRAD SÄSONG VILL MAN SLÅ AV EVENTUELL PÅSLAGEN               
027000* --   RÖRELSEINDIKATOR-FLAGGA FÖR ATT FÅ UPP ARTIKELN SOM                
027100* --   KÖPFÖRSLAG IGEN.                                                   
027200       IF ARTS-SLAG-FLREFNYO = JA                                         
027300          MOVE NEJ         TO ARTS-SLAG-FLREFNYO                          
027400       END-IF                                                             
027500                                                                          
027600******                                                                    
027700*         EN ARTIKEL SOM SÄTTS TILL SÄSONG SKA FLAGGAS                    
027800*         SÅ ATT DEN INTE REFILLBEORDRAS AUTOMATISKT                      
027900******                                                                    
028000                                                                          
028100       PERFORM IMS-REPL-ARTS-WDK711                                       
028200                                                                          
028300       ADD +1 TO CHKP-ANT                                                 
028400     END-IF                                                               
028500     .                                                                    
028600     EJECT                                                                
028700 Z-FINIT SECTION.                                                         
028800                                                                          
028900     CLOSE W27133                                                         
029000     SKIP2                                                                
029100     MOVE 'S' TO POSTSUM-OPKOD                                            
029200     CALL POSTSUM USING POSTSUM-PARM                                      
029300                                                                          
029400     .                                                                    
029500     EJECT                                                                
029600 S01-LAES-W27133  SECTION.                                                
029700     SKIP2                                                                
029800     READ W27133 INTO IN-AREA                                             
029900     AT END                                                               
030000        SET END-OF-W27133 TO TRUE                                         
030100                                                                          
030200     NOT AT END                                                           
030300        MOVE 'W27133' TO POSTSUM-FDNAMN                                   
030400        MOVE 'W27133D1' TO POSTSUM-DDNAMN2                                
030500        CALL POSTSUM USING POSTSUM-PARM                                   
030600     END-READ                                                             
030700     .                                                                    
030800                                                                          
030900 X-TAG-CHECKPOINT   SECTION.                                              
031000                                                                          
031100* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
031200* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
031300     PERFORM IMS-CHECKPOINT                                               
031400     MOVE ZERO TO CHKP-ANT                                                
031500* --- LÄS OM DATABAS OM DET BEHÖVS                                        
031600     .                                                                    
031700     EJECT                                                                
031800* --- IMS SEKTIONER ---                                                   
031900     SKIP3                                                                
032000     EJECT                                                                
032100 IMS-RESTART SECTION.                                                     
032200     SKIP2                                                                
032300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
032400     MOVE '  ' TO GODK-STATUSKODER                                        
032500     CALL CBLTDLI USING XRST MSG-PCB                                      
032600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
032700                        CHKP-AREA-LENGTH CHKP-AREA                        
032800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032900     PERFORM IMS-STATUSKONTROLL                                           
033000     .                                                                    
033100     EJECT                                                                
033200 IMS-CHECKPOINT SECTION.                                                  
033300     SKIP2                                                                
033400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
033500     MOVE '  XD' TO GODK-STATUSKODER                                      
033600     CALL CBLTDLI USING CHKP MSG-PCB                                      
033700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
033800                        CHKP-AREA-LENGTH CHKP-AREA                        
033900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034000     PERFORM IMS-STATUSKONTROLL                                           
034100                                                                          
034200     IF IMS-EJ-OK                                                         
034300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
034400       DISPLAY FELTEXT                                                    
034500       CALL FELLOG                                                        
034600     END-IF                                                               
034700     .                                                                    
034800     EJECT                                                                
034900 IMS-GU-ARTS-WDK711 SECTION.                                              
035000                                                                          
035100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
035200          DELIMITED BY SIZE INTO SSA1                                     
035300     MOVE '  ' TO GODK-STATUSKODER                                        
035400     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1                      
035500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
035600     PERFORM IMS-STATUSKONTROLL                                           
035700     .                                                                    
035800     EJECT                                                                
035900 IMS-GHNP-ARTS-WDK711 SECTION.                                            
036000                                                                          
036100     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
036200          DELIMITED BY SIZE INTO SSA1                                     
036300     MOVE '  ' TO GODK-STATUSKODER                                        
036400     CALL CBLTDLI USING GHNP ARTS-PCB DLI-IO-AREA SSA1                    
036500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
036600     PERFORM IMS-STATUSKONTROLL                                           
036700     .                                                                    
036800     SKIP3                                                                
036900 IMS-REPL-ARTS-WDK711 SECTION.                                            
037000                                                                          
037100     MOVE '  ' TO GODK-STATUSKODER                                        
037200     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA                         
037300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
037400     PERFORM IMS-STATUSKONTROLL                                           
037500     .                                                                    
037600     EJECT                                                                
037700 IMS-STATUSKONTROLL SECTION.                                              
037800     SKIP2                                                                
037900     SET STATUS-IX TO 1                                                   
038000     SEARCH GODK-STATUS                                                   
038100       AT END                                                             
038200         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
038300         DISPLAY FELTEXT                                                  
038400         CALL FELLOG                                                      
038500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
038600         CONTINUE                                                         
038700     END-SEARCH                                                           
038800     .                                                                    

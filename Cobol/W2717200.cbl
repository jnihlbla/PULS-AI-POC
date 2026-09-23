000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2717200.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   FEB 1998.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                                                                         
001000*        PROGRAMMET SKRIVER UT EN LISTA ÖVER 'TOPP 100                    
001100*        RESTORDER' PER DC I ORDERLINES ORDNING                           
001200*        FÖR FÖRRA VECKAN                                                 
001300*                                                                         
001400*                                                                         
001500                                                                          
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          ---                                                            
002400     SELECT W27172                     ASSIGN TO W27172D1.                
002500     SKIP2                                                                
002600*          ---                                                            
002700     SELECT W271UT-ALL                 ASSIGN TO W27172D2.                
002800                                                                          
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400                                                                          
003500 FD  W27172                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  POST  -COPY W27162    -PRE IN-    -L.                                
004000                                                                          
004100 FD  W271UT-ALL                                                           
004200     RECORDING       V                                                    
004300     BLOCK CONTAINS  0 RECORDS.                                           
004400 01  W27172-REC            PIC X(124).                                    
004500                                                                          
004600                                                                          
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W2717200'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 01  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005500                                                                          
005600 77  W27172-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W27172                       VALUE 'J'.                   
005800                                                                          
005900                                                                          
006000 01  ARBETSAREOR.                                                         
006200                                                                          
006300     03 WS-RED-DATUM.                                                     
006400       05  WS-RED-MM             PIC X(2)   VALUE SPACE.                  
006500       05  FILLER                PIC X      VALUE '/'.                    
006600       05  WS-RED-DD             PIC X(2)   VALUE SPACE.                  
006700       05  FILLER                PIC X      VALUE '/'.                    
006800       05  WS-RED-AA             PIC X(2)   VALUE SPACE.                  
006900     03 WS-ANTAL-ARTIKLAR-ALL    PIC 9(8)   VALUE ZERO.                   
007000     03 WS-ANTAL-ORDRAD-ALL      PIC 9(8)   VALUE ZERO.                   
007100     03 WS-ANTAL-QTY-ALL         PIC 9(8)   VALUE ZERO.                   
007300                                                                          
007400                                                                          
007500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007600 01  FILLER REDEFINES DAGENS-DATUM.                                       
007700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008000                                                                          
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400                                                                          
008500     EJECT                                                                
008600 01  W27172-AREA-START           PIC X(24)   VALUE                        
008700                                 'W27172-AREA-START  '.                   
008800                                                                          
008900*01  AREA -COPY W27162     -PRE IN-                                       
009000                                                                          
009100     EJECT                                                                
009200 01  W271UT-AREA-START           PIC X(24)   VALUE                        
009300                                 'W271UT-AREA-START  '.                   
009400                                                                          
009500 01  W001-DAP.                                                            
009600     03  FILLER                  PIC X(165)  VALUE SPACE.                 
009700                                                                          
009800 01  UT-AREOR.                                                            
009900     03 UT-RPT-PRINT-LINES.                                               
010000       05 UT-HEADING-1.                                                   
010100         10 FILLER               PIC X(1)  VALUE SPACE.                   
010200         10 FILLER               PIC X(10) VALUE 'VOLVO CAR'.             
010300         10 FILLER               PIC X(17) VALUE 'AFTERSALES'.            
010400         10 FILLER               PIC X(11) VALUE SPACE.                   
010500         10 UT-LIST-NAME         PIC X(12) VALUE 'W27172-001'.            
010600         10 FILLER               PIC X(12) VALUE SPACE.                   
010700         10 FILLER               PIC X(13) VALUE 'DC-WAREHOUSE'.          
010800         10 UT-IDDC              PIC X(2)  VALUE SPACE.                   
010900         10 FILLER               PIC X(5)  VALUE SPACE.                   
011000         10 FILLER               PIC X(7)  VALUE 'TOP 100'.               
011100         10 FILLER               PIC X(11) VALUE SPACE.                   
011200         10 UT-REPORT-DATE       PIC X(8)  VALUE SPACE.                   
011300         10 FILLER               PIC X(1)  VALUE SPACE.                   
011400         10 FILLER               PIC X(4)  VALUE 'PAGE'.                  
011500         10 FILLER               PIC X(1)  VALUE SPACE.                   
011600                                                                          
011700       05 UT-HEADING-2.                                                   
011800         10 FILLER               PIC X(39) VALUE SPACE.                   
011900         10 FILLER               PIC X(12) VALUE 'BACKORDERED'.           
012000         10 FILLER               PIC X(9)  VALUE 'ITEMS BY'.              
012100         10 FILLER               PIC X(11) VALUE 'ORDERLINES'.            
012200         10 FILLER               PIC X(9)  VALUE 'LAST WEEK'.             
012300         10 FILLER               PIC X(10) VALUE SPACE.                   
012400         10 UT-BUYER             PIC X(10) VALUE 'ALL BUYERS'.            
012500       05 UT-HEADING-3.                                                   
012600         10 FILLER               PIC X(5)  VALUE SPACE.                   
012700         10 FILLER               PIC X(5)  VALUE 'BUYER'.                 
012800         10 FILLER               PIC X(4)  VALUE SPACE.                   
012900         10 FILLER               PIC X(4)  VALUE 'PART'.                  
013000         10 FILLER               PIC X(27) VALUE SPACE.                   
013100         10 FILLER               PIC X(4)  VALUE 'FUNC'.                  
013200         10 FILLER               PIC X(1)  VALUE SPACE.                   
013300         10 FILLER               PIC X(6)   VALUE 'FREEZE'.               
013400         10 FILLER               PIC X(1)  VALUE SPACE.                   
013500         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
013600         10 FILLER               PIC X(9)  VALUE SPACE.                   
013700         10 FILLER               PIC X(7)  VALUE 'AVERAGE'.               
013800         10 FILLER               PIC X(6)  VALUE SPACE.                   
013900         10 FILLER               PIC X(4)  VALUE 'FORE'.                  
014000         10 FILLER               PIC X(5)  VALUE SPACE.                   
014100         10 FILLER               PIC X(2)  VALUE 'ON'.                    
014200         10 FILLER               PIC X(6)  VALUE SPACE.                   
014300         10 FILLER               PIC X(2)  VALUE 'ON'.                    
014400         10 FILLER               PIC X(13) VALUE SPACE.                   
014500         10 FILLER               PIC X(3)  VALUE 'BAL'.                   
014600       05 UT-HEADING-4.                                                   
014700         10 FILLER               PIC X(1)  VALUE SPACE.                   
014800         10 FILLER               PIC X(4)  VALUE 'RANK'.                  
014900         10 FILLER               PIC X(1)  VALUE SPACE.                   
015000         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
015100         10 FILLER               PIC X(3)  VALUE SPACE.                   
015200         10 FILLER               PIC X(6)  VALUE 'NUMBER'.                
015300         10 FILLER               PIC X(1)  VALUE SPACE.                   
015400         10 FILLER               PIC X(11) VALUE 'DESCRIPTION'.           
015500         10 FILLER               PIC X(10) VALUE SPACE.                   
015600         10 FILLER               PIC X(3)  VALUE 'S/C'.                   
015700         10 FILLER               PIC X(2)  VALUE SPACE.                   
015800         10 FILLER               PIC X(3)  VALUE 'GRP'.                   
015900         10 FILLER               PIC X(2)  VALUE SPACE.                   
016000         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
016100         10 FILLER               PIC X(2)  VALUE SPACE.                   
016200         10 FILLER               PIC X(5)  VALUE 'LINES'.                 
016300         10 FILLER               PIC X(2)  VALUE SPACE.                   
016400         10 FILLER               PIC X(3)  VALUE 'QTY'.                   
016500         10 FILLER               PIC X(6)  VALUE SPACE.                   
016600         10 FILLER               PIC X(4)  VALUE 'COST'.                  
016700         10 FILLER               PIC X(7)  VALUE SPACE.                   
016800         10 FILLER               PIC X(4)  VALUE 'CAST'.                  
016900         10 FILLER               PIC X(4)  VALUE SPACE.                   
017000         10 FILLER               PIC X(4)  VALUE 'HAND'.                  
017100         10 FILLER               PIC X(3)  VALUE SPACE.                   
017200         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
017300         10 FILLER               PIC X(5)  VALUE SPACE.                   
017400         10 FILLER               PIC X(2)  VALUE 'AK'.                    
017500         10 FILLER               PIC X(5)  VALUE SPACE.                   
017600         10 FILLER               PIC X(3)  VALUE 'CDC'.                   
017700       05 UT-RAD-1.                                                       
017800         10 FILLER               PIC X     VALUE SPACE.                   
017900         10 UT-RANK              PIC Z(3)9 VALUE ZERO.                    
018000         10 FILLER               PIC X(1)  VALUE SPACE.                   
018100         10 UT-IDPERSON-BUY      PIC ZZ9   VALUE ZERO.                    
018200         10 FILLER               PIC X     VALUE SPACE.                   
018300         10 UT-IDARTNR           PIC Z(8)9 VALUE ZERO.                    
018400         10 FILLER               PIC X(1)  VALUE SPACE.                   
018500         10 UT-BEART             PIC X(19) VALUE SPACE.                   
018600         10 FILLER               PIC X(2)  VALUE SPACE.                   
018700         10 UT-KDERS             PIC Z9    VALUE ZERO.                    
018800         10 FILLER               PIC X(1)  VALUE SPACE.                   
018900         10 UT-IDFKNGRP          PIC Z(4)9 VALUE ZERO.                    
019000         10 FILLER               PIC X(3)  VALUE SPACE.                   
019100         10 UT-FREEZECODE        PIC X     VALUE SPACE.                   
019200         10 FILLER               PIC X(1)  VALUE SPACE.                   
019300         10 UT-ANTAL-ORDRAD      PIC Z(5)9 VALUE ZERO.                    
019400         10 FILLER               PIC X(1)  VALUE SPACE.                   
019500         10 UT-ANTAL-QTY         PIC Z(5)9 VALUE ZERO.                    
019600         10 FILLER               PIC X(1)  VALUE SPACE.                   
019700         10 UT-PRAVCOST          PIC Z(6)9.9(2)                           
019800                                           VALUE ZERO.                    
019900         10 FILLER               PIC X(1)  VALUE SPACE.                   
020000         10 UT-KVPB-REF          PIC Z(5)9.9(2)                           
020100                                           VALUE ZERO.                    
020200         10 FILLER               PIC X(1)  VALUE SPACE.                   
020300         10 UT-ONHAND            PIC Z(6)9 VALUE ZERO.                    
020400         10 FILLER               PIC X(1)  VALUE SPACE.                   
020500         10 UT-KVBEART           PIC Z(6)9 VALUE ZERO.                    
020600         10 FILLER               PIC X(1)  VALUE SPACE.                   
020700         10 UT-KVAKS-SDC         PIC Z(5)9 VALUE ZERO.                    
020800         10 FILLER               PIC X(1)  VALUE SPACE.                   
020900         10 UT-AVAIL-CDC         PIC Z(6)9 VALUE ZERO.                    
021000       05 UT-RAD-TOT.                                                     
021100         10 FILLER               PIC X(48) VALUE SPACE.                   
021200         10 FILLER               PIC X(6)  VALUE 'TOTAL'.                 
021300         10 UT-ANTAL-ORDRAD-TOT  PIC Z(5)9 VALUE ZERO.                    
021400         10 FILLER               PIC X(1)  VALUE SPACE.                   
021500         10 UT-ANTAL-QTY-TOT     PIC Z(5)9 VALUE ZERO.                    
021600                                                                          
021700     EJECT                                                                
021800 PROCEDURE DIVISION.                                                      
021900 MAIN SECTION.                                                            
022000                                                                          
022100     PERFORM A-INIT                                                       
022200     PERFORM B-SKAPA-POSTER                                               
022300     PERFORM Z-FINIT                                                      
022400                                                                          
022500     MOVE ZERO TO RETURN-CODE                                             
022600     GOBACK                                                               
022700     .                                                                    
022800                                                                          
022900     EJECT                                                                
023000 A-INIT SECTION.                                                          
023100                                                                          
023200     OPEN INPUT  W27172                                                   
023300                                                                          
023400     OPEN OUTPUT W271UT-ALL                                               
023500                                                                          
023600     ACCEPT DAGENS-DATUM     FROM DATE                                    
023700     MOVE DAGENS-DATUM-AAR   TO WS-RED-AA                                 
023800     MOVE DAGENS-DATUM-MAANAD                                             
023900                             TO WS-RED-MM                                 
024000     MOVE DAGENS-DATUM-DAG   TO WS-RED-DD                                 
024100     MOVE WS-RED-DATUM       TO UT-REPORT-DATE                            
024400     MOVE ZERO               TO WS-ANTAL-ARTIKLAR-ALL                     
024500                                WS-ANTAL-ORDRAD-ALL                       
024600                                WS-ANTAL-QTY-ALL                          
024700     .                                                                    
024800                                                                          
024900     EJECT                                                                
025000 B-SKAPA-POSTER SECTION.                                                  
025100     PERFORM S01-LAES-W27172                                              
025200                                                                          
025300     PERFORM UNTIL END-OF-W27172                                          
025400       IF IN-IDDC NOT = WS-IDDC                                           
025410          PERFORM S02-SKRIV-DAP1                                          
025420          PERFORM S03-SKRIV-DAP2                                          
025421          MOVE IN-IDDC       TO UT-IDDC                                   
025430          WRITE W27172-REC FROM UT-HEADING-1                              
025431          WRITE W27172-REC FROM UT-HEADING-2                              
025432          WRITE W27172-REC FROM UT-HEADING-3                              
025433          WRITE W27172-REC FROM UT-HEADING-4                              
025442          MOVE IN-IDDC TO WS-IDDC                                         
025443          MOVE ZERO          TO WS-ANTAL-ARTIKLAR-ALL                     
025450       END-IF                                                             
025500       MOVE IN-IDPERSON-BUY  TO UT-IDPERSON-BUY                           
025600       MOVE IN-IDARTNR       TO UT-IDARTNR                                
025700       MOVE IN-BEART         TO UT-BEART                                  
025800       MOVE IN-IDFKNGRP      TO UT-IDFKNGRP                               
025900       MOVE IN-FREEZECODE    TO UT-FREEZECODE                             
026000       MOVE IN-ANTAL-ORDRAD  TO UT-ANTAL-ORDRAD                           
026100       MOVE IN-ANTAL-QTY     TO UT-ANTAL-QTY                              
026200       MOVE IN-PRAVCOST      TO UT-PRAVCOST                               
026300       MOVE IN-KVPB-REF      TO UT-KVPB-REF                               
026400       MOVE IN-ONHAND        TO UT-ONHAND                                 
026500       MOVE IN-KVBEART       TO UT-KVBEART                                
026600       MOVE IN-KVAKS-SDC     TO UT-KVAKS-SDC                              
026700       MOVE IN-KDERS         TO UT-KDERS                                  
026800       MOVE IN-AVAIL-CDC     TO UT-AVAIL-CDC                              
026900       IF WS-ANTAL-ARTIKLAR-ALL < 100                                     
027000         ADD 1               TO WS-ANTAL-ARTIKLAR-ALL                     
027100         MOVE WS-ANTAL-ARTIKLAR-ALL                                       
027200                             TO UT-RANK                                   
027300         ADD IN-ANTAL-ORDRAD TO WS-ANTAL-ORDRAD-ALL                       
027400         ADD IN-ANTAL-QTY    TO WS-ANTAL-QTY-ALL                          
027600         MOVE UT-RAD-1       TO W001-DAP                                  
027700         WRITE W27172-REC FROM W001-DAP                                   
027800       END-IF                                                             
027900       PERFORM S01-LAES-W27172                                            
028000     END-PERFORM                                                          
028100                                                                          
028200**************************************************'                       
028300*                                                                         
028400*    SKRIV EN TOTALRAD FÖR SAMTLIGA LISTOR                                
028500*                                                                         
028600**************************************************'                       
028700                                                                          
028800     MOVE SPACE              TO W001-DAP                                  
028900     WRITE W27172-REC FROM W001-DAP                                       
029000     MOVE WS-ANTAL-ORDRAD-ALL                                             
029100                             TO UT-ANTAL-ORDRAD-TOT                       
029200     MOVE WS-ANTAL-QTY-ALL   TO UT-ANTAL-QTY-TOT                          
029400     MOVE UT-RAD-TOT         TO W001-DAP                                  
029500     WRITE W27172-REC FROM W001-DAP                                       
029600     .                                                                    
029700                                                                          
029800     EJECT                                                                
032500 Z-FINIT SECTION.                                                         
032600     CLOSE W27172                                                         
032700           W271UT-ALL                                                     
032800     .                                                                    
032900                                                                          
033000     EJECT                                                                
033100 S01-LAES-W27172  SECTION.                                                
033200     READ W27172 INTO IN-AREA                                             
033300     AT END                                                               
033400        SET END-OF-W27172 TO TRUE                                         
033500                                                                          
033600     END-READ                                                             
033700     .                                                                    
033800 S02-SKRIV-DAP1 SECTION.                                                  
033900                                                                          
034000     MOVE ' ¤DAPW27172' TO W001-DAP                                       
034100     WRITE W27172-REC    FROM W001-DAP                                    
034200                                                                          
034300     MOVE SPACE TO W001-DAP                                               
034400     .                                                                    
034500                                                                          
034600 S03-SKRIV-DAP2 SECTION.                                                  
034700                                                                          
034800     STRING ' ¤DAP' IN-IDDC                                               
034900            DELIMITED BY SIZE INTO W001-DAP                               
035000     WRITE W27172-REC    FROM W001-DAP                                    
035100                                                                          
035200     MOVE SPACE TO W001-DAP                                               
035300     .                                                                    
035400     EJECT                                                                

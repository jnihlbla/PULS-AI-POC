000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2716300.                                                
000300 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000400 DATE-WRITTEN.   MAJ 1997.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET SKRIVER UT EN LISTA ÖVER 'TOPP 200                    
000900*        RESTORDER' PER DC I ÄLDSTAORDNING                                
001000                                                                          
001100                                                                          
001200 ENVIRONMENT DIVISION.                                                    
001300                                                                          
001400 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700                                                                          
001800*          ---                                                            
001900     SELECT W27163                     ASSIGN TO W27163D1.                
002000*          ---                                                            
002100     SELECT W271UT-ALL                 ASSIGN TO W27163D2.                
002200                                                                          
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500                                                                          
002600 FILE SECTION.                                                            
002700                                                                          
002800                                                                          
002900 FD  W27163                                                               
003000     RECORDING F                                                          
003100     LABEL RECORD STANDARD                                                
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400 01  IN-POST                 PIC X(80).                                   
003500                                                                          
003600 FD  W271UT-ALL                                                           
003700     RECORDING  V                                                         
003800     BLOCK CONTAINS  0 RECORDS.                                           
003900 01  ORDER-REPORT-REC-ALL    PIC X(124).                                  
004000                                                                          
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W2716300'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 01  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
004800                                                                          
004900 77  W27163-EOF-SW               PIC X       VALUE 'N'.                   
005000     88  END-OF-W27163                       VALUE 'J'.                   
005100                                                                          
005200     EJECT                                                                
005300 01  ARBETSAREOR.                                                         
005400                                                                          
006400     03 WS-RED-DATUM.                                                     
006500       05  WS-RED-MM             PIC X(2)   VALUE SPACE.                  
006600       05  FILLER                PIC X      VALUE '/'.                    
006700       05  WS-RED-DD             PIC X(2)   VALUE SPACE.                  
006800       05  FILLER                PIC X      VALUE '/'.                    
006900       05  WS-RED-AA             PIC X(2)   VALUE SPACE.                  
008100     03 WS-DARODAT               PIC 9(8)   VALUE ZERO.                   
008400     03 WS-ANTAL-ARTIKLAR-ALL    PIC 9(8)   VALUE ZERO.                   
008700                                                                          
008800                                                                          
008900                                                                          
009000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009100 01  FILLER REDEFINES DAGENS-DATUM.                                       
009200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009500                                                                          
009600     EJECT                                                                
009700 01  W27163-AREA-START           PIC X(24)   VALUE                        
009800                                 'W27163-AREA-START  '.                   
009900                                                                          
010000*01  AREA -COPY W27162     -PRE IN-                                       
010100                                                                          
010200     EJECT                                                                
010300 01  W271UT-AREA-START           PIC X(24)   VALUE                        
010400                                 'W271UT-AREA-START  '.                   
010500                                                                          
010510 01  W001-DAP.                                                            
010520     03  FILLER                  PIC X(165)  VALUE SPACE.                 
010600                                                                          
010700 01  UT-AREOR.                                                            
010800     03 UT-RPT-PRINT-LINES.                                               
010900       05 UT-HEADING-1.                                                   
011000         10 FILLER               PIC X(1)  VALUE SPACE.                   
011100         10 FILLER               PIC X(10) VALUE 'VOLVO CAR'.             
011200         10 FILLER               PIC X(17) VALUE 'AFTERSALES'.            
011300         10 FILLER               PIC X(11) VALUE SPACE.                   
011400         10 UT-LIST-NAME         PIC X(12) VALUE SPACE.                   
011500         10 FILLER               PIC X(12) VALUE SPACE.                   
011600         10 FILLER               PIC X(13) VALUE 'DC-WAREHOUSE'.          
011700         10 UT-IDDC              PIC X(2)  VALUE SPACE.                   
011800         10 FILLER               PIC X(5)  VALUE SPACE.                   
011900         10 FILLER               PIC X(7)  VALUE 'TOP 200'.               
012000         10 FILLER               PIC X(11) VALUE SPACE.                   
012100         10 UT-REPORT-DATE       PIC X(8)  VALUE SPACE.                   
012600       05 UT-HEADING-2.                                                   
012700         10 FILLER               PIC X(39) VALUE SPACE.                   
012800         10 FILLER               PIC X(12) VALUE 'BACKORDERED'.           
012900         10 FILLER               PIC X(9)  VALUE 'ITEMS BY'.              
013000         10 FILLER               PIC X(10) VALUE 'ORDER DATE'.            
013100         10 FILLER               PIC X(10) VALUE SPACE.                   
013200         10 UT-BUYER             PIC X(10) VALUE 'ALL-BUYERS'.            
013300       05 UT-HEADING-3.                                                   
013400         10 FILLER               PIC X(5)  VALUE SPACE.                   
013500         10 FILLER               PIC X(5)  VALUE 'BUYER'.                 
013600         10 FILLER               PIC X(4)  VALUE SPACE.                   
013700         10 FILLER               PIC X(4)  VALUE 'PART'.                  
013800         10 FILLER               PIC X(27) VALUE SPACE.                   
013900         10 FILLER               PIC X(4)  VALUE 'FUNC'.                  
014000         10 FILLER               PIC X(1)  VALUE SPACE.                   
014100         10 FILLER               PIC X(6)   VALUE 'FREEZE'.               
014200         10 FILLER               PIC X(1)  VALUE SPACE.                   
014300         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
014400         10 FILLER               PIC X(9)  VALUE SPACE.                   
014500         10 FILLER               PIC X(7)  VALUE 'AVERAGE'.               
014600         10 FILLER               PIC X(6)  VALUE SPACE.                   
014700         10 FILLER               PIC X(4)  VALUE 'FORE'.                  
014800         10 FILLER               PIC X(5)  VALUE SPACE.                   
014900         10 FILLER               PIC X(2)  VALUE 'ON'.                    
015000         10 FILLER               PIC X(5)  VALUE SPACE.                   
015100         10 FILLER               PIC X(2)  VALUE 'ON'.                    
015200         10 FILLER               PIC X(4)  VALUE SPACE.                   
015300         10 FILLER               PIC X(4)  VALUE 'BACK'.                  
015400         10 FILLER               PIC X(6)  VALUE SPACE.                   
015500         10 FILLER               PIC X(3)  VALUE 'BAL'.                   
015600       05 UT-HEADING-4.                                                   
015700         10 FILLER               PIC X(1)  VALUE SPACE.                   
015800         10 FILLER               PIC X(4)  VALUE 'RANK'.                  
015900         10 FILLER               PIC X(1)  VALUE SPACE.                   
016000         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
016100         10 FILLER               PIC X(3)  VALUE SPACE.                   
016200         10 FILLER               PIC X(6)  VALUE 'NUMBER'.                
016300         10 FILLER               PIC X(1)  VALUE SPACE.                   
016400         10 FILLER               PIC X(11) VALUE 'DESCRIPTION'.           
016500         10 FILLER               PIC X(10) VALUE SPACE.                   
016600         10 FILLER               PIC X(3)  VALUE 'S/C'.                   
016700         10 FILLER               PIC X(2)  VALUE SPACE.                   
016800         10 FILLER               PIC X(3)  VALUE 'GRP'.                   
016900         10 FILLER               PIC X(2)  VALUE SPACE.                   
017000         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
017100         10 FILLER               PIC X(2)  VALUE SPACE.                   
017200         10 FILLER               PIC X(5)  VALUE 'LINES'.                 
017300         10 FILLER               PIC X(2)  VALUE SPACE.                   
017400         10 FILLER               PIC X(3)  VALUE 'QTY'.                   
017500         10 FILLER               PIC X(6)  VALUE SPACE.                   
017600         10 FILLER               PIC X(4)  VALUE 'COST'.                  
017700         10 FILLER               PIC X(7)  VALUE SPACE.                   
017800         10 FILLER               PIC X(4)  VALUE 'CAST'.                  
017900         10 FILLER               PIC X(4)  VALUE SPACE.                   
018000         10 FILLER               PIC X(4)  VALUE 'HAND'.                  
018100         10 FILLER               PIC X(2)  VALUE SPACE.                   
018200         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
018300         10 FILLER               PIC X(1)  VALUE SPACE.                   
018400         10 FILLER               PIC X(7)  VALUE 'ORDERED'.               
018500         10 FILLER               PIC X(5)  VALUE SPACE.                   
018600         10 FILLER               PIC X(3)  VALUE 'CDC'.                   
018700       05 UT-RAD-1.                                                       
018800         10 FILLER               PIC X     VALUE SPACE.                   
018900         10 UT-RANK              PIC Z(3)9 VALUE ZERO.                    
019000         10 FILLER               PIC X(1)  VALUE SPACE.                   
019100         10 UT-IDPERSON-BUY      PIC ZZ9   VALUE ZERO.                    
019200         10 FILLER               PIC X     VALUE SPACE.                   
019300         10 UT-IDARTNR           PIC Z(8)9 VALUE ZERO.                    
019400         10 FILLER               PIC X(1)  VALUE SPACE.                   
019500         10 UT-BEART             PIC X(19) VALUE SPACE.                   
019600         10 FILLER               PIC X(2)  VALUE SPACE.                   
019700         10 UT-KDERS             PIC Z9    VALUE ZERO.                    
019800         10 FILLER               PIC X(1)  VALUE SPACE.                   
019900         10 UT-IDFKNGRP          PIC Z(4)9 VALUE ZERO.                    
020000         10 FILLER               PIC X(3)  VALUE SPACE.                   
020100         10 UT-FREEZECODE        PIC X     VALUE SPACE.                   
020200         10 FILLER               PIC X(1)  VALUE SPACE.                   
020300         10 UT-ANTAL-ORDRAD      PIC Z(5)9 VALUE ZERO.                    
020400         10 FILLER               PIC X(1)  VALUE SPACE.                   
020500         10 UT-ANTAL-QTY         PIC Z(5)9 VALUE ZERO.                    
020600         10 FILLER               PIC X(1)  VALUE SPACE.                   
020700         10 UT-PRAVCOST          PIC Z(6)9.9(2)                           
020800                                           VALUE ZERO.                    
020900         10 FILLER               PIC X(1)  VALUE SPACE.                   
021000         10 UT-KVPB-REF          PIC Z(5)9.9(2)                           
021100                                           VALUE ZERO.                    
021200         10 FILLER               PIC X(1)  VALUE SPACE.                   
021300         10 UT-ONHAND            PIC Z(6)9 VALUE ZERO.                    
021400         10 FILLER               PIC X(1)  VALUE SPACE.                   
021500         10 UT-KVBEART           PIC Z(5)9 VALUE ZERO.                    
021600         10 FILLER               PIC X(2)  VALUE SPACE.                   
021700         10 UT-TIRODAT           PIC 9(6)  VALUE ZERO.                    
021800         10 FILLER               PIC X(1)  VALUE SPACE.                   
021900         10 UT-AVAIL-CDC         PIC Z(6)9 VALUE ZERO.                    
022000     EJECT                                                                
022100 PROCEDURE DIVISION.                                                      
022200 MAIN SECTION.                                                            
022300                                                                          
022400     PERFORM A-INIT                                                       
022500     PERFORM B-SKAPA-POSTER                                               
022600     PERFORM Z-FINIT                                                      
022700                                                                          
022800     MOVE ZERO TO RETURN-CODE                                             
022900     GOBACK                                                               
023000     .                                                                    
023100     EJECT                                                                
023200                                                                          
023300                                                                          
023400 A-INIT SECTION.                                                          
023500                                                                          
023600     OPEN INPUT  W27163                                                   
023700                                                                          
023800     OPEN OUTPUT W271UT-ALL                                               
023900                                                                          
024000     ACCEPT DAGENS-DATUM     FROM DATE                                    
024100     MOVE DAGENS-DATUM-AAR   TO WS-RED-AA                                 
024200     MOVE DAGENS-DATUM-MAANAD                                             
024300                             TO WS-RED-MM                                 
024400     MOVE DAGENS-DATUM-DAG   TO WS-RED-DD                                 
024500     MOVE WS-RED-DATUM       TO UT-REPORT-DATE                            
024700                                                                          
024800     MOVE ZERO               TO WS-ANTAL-ARTIKLAR-ALL                     
024900     .                                                                    
025000     EJECT                                                                
025100 B-SKAPA-POSTER SECTION.                                                  
025200     PERFORM S01-LAES-W27163                                              
025300                                                                          
025400     PERFORM UNTIL END-OF-W27163                                          
025410       IF IN-IDDC NOT = WS-IDDC                                           
025420          PERFORM S02-SKRIV-DAP1                                          
025430          PERFORM S03-SKRIV-DAP2                                          
025440          MOVE IN-IDDC       TO UT-IDDC                                   
025450          WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-1                    
025460          WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-2                    
025470          WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-3                    
025480          WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-4                    
025490          MOVE IN-IDDC TO WS-IDDC                                         
025491          MOVE ZERO    TO WS-ANTAL-ARTIKLAR-ALL                           
025492       END-IF                                                             
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
026600       MOVE IN-DARODAT       TO WS-DARODAT                                
026700       MOVE WS-DARODAT (3:6) TO UT-TIRODAT                                
026800       MOVE IN-KDERS         TO UT-KDERS                                  
026900       MOVE IN-AVAIL-CDC     TO UT-AVAIL-CDC                              
027000       IF WS-ANTAL-ARTIKLAR-ALL < 200                                     
027100         ADD 1               TO WS-ANTAL-ARTIKLAR-ALL                     
027200         MOVE WS-ANTAL-ARTIKLAR-ALL                                       
027210                             TO UT-RANK                                   
027220         MOVE UT-RAD-1       TO ORDER-REPORT-REC-ALL                      
027230         WRITE ORDER-REPORT-REC-ALL                                       
027300       END-IF                                                             
027400       PERFORM S01-LAES-W27163                                            
027500     END-PERFORM                                                          
027600     .                                                                    
027700     EJECT                                                                
027800                                                                          
027900                                                                          
030800 Z-FINIT SECTION.                                                         
030900     CLOSE W27163                                                         
031000           W271UT-ALL                                                     
031100     .                                                                    
031200                                                                          
031300     EJECT                                                                
031400 S01-LAES-W27163  SECTION.                                                
031500     READ W27163 INTO IN-AREA                                             
031600     AT END                                                               
031700        SET END-OF-W27163 TO TRUE                                         
031800                                                                          
031900     END-READ                                                             
032000     .                                                                    
032100                                                                          
032200                                                                          
032300 S02-SKRIV-DAP1 SECTION.                                                  
032400                                                                          
032500     MOVE ' ¤DAPW27163' TO W001-DAP                                       
032600     WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                             
032700                                                                          
032800     MOVE SPACE TO W001-DAP                                               
032900     .                                                                    
033000                                                                          
033100 S03-SKRIV-DAP2 SECTION.                                                  
033200                                                                          
033300     STRING ' ¤DAP' IN-IDDC                                               
033400            DELIMITED BY SIZE INTO W001-DAP                               
033500     WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                             
033600                                                                          
033700     MOVE SPACE TO W001-DAP                                               
033800     .                                                                    

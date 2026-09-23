000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2716500.                                                
000300 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000400 DATE-WRITTEN.   MAJ 1997.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET SKRIVER UT EN LISTA ÖVER 'TOPP 200                    
000900*        RESTORDER' PER DC I ORDERLINES ORDNING                           
001000*                                                                         
001100                                                                          
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800                                                                          
001900     SELECT W27165                     ASSIGN TO W27165D1.                
002000                                                                          
002100     SELECT W271UT-ALL                 ASSIGN TO W27165D2.                
002200                                                                          
002300                                                                          
002400 DATA DIVISION.                                                           
002500                                                                          
002600 FILE SECTION.                                                            
002700                                                                          
002800                                                                          
002900 FD  W27165                                                               
003000     RECORDING MODE F                                                     
003100     LABEL RECORD STANDARD                                                
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400 01  IN-POST                 PIC X(80).                                   
003500                                                                          
003600 FD  W271UT-ALL                                                           
003700     RECORDING MODE V                                                     
003800     LABEL RECORD STANDARD                                                
003900     BLOCK CONTAINS  0 RECORDS.                                           
004000 01  ORDER-REPORT-REC-ALL    PIC X(124).                                  
004100                                                                          
004200                                                                          
004400 WORKING-STORAGE SECTION.                                                 
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W2716500'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004810 01  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
004900                                                                          
005000 77  W27165-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W27165                       VALUE 'J'.                   
005200                                                                          
005400 01  ARBETSAREOR.                                                         
005500                                                                          
005600     03 WS-RED-DATUM.                                                     
005700       05  WS-RED-MM             PIC X(2)   VALUE SPACE.                  
005800       05  FILLER                PIC X      VALUE '/'.                    
005900       05  WS-RED-DD             PIC X(2)   VALUE SPACE.                  
006000       05  FILLER                PIC X      VALUE '/'.                    
006100       05  WS-RED-AA             PIC X(2)   VALUE SPACE.                  
006200     03 WS-ANTAL-ARTIKLAR-ALL    PIC 9(8)   VALUE ZERO.                   
006300                                                                          
006400                                                                          
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000                                                                          
007100                                                                          
007300 01  W27165-AREA-START           PIC X(24)   VALUE                        
007400                                 'W27165-AREA-START  '.                   
007500                                                                          
007600*01  AREA -COPY W27162     -PRE IN-                                       
007700                                                                          
007900 01  W271UT-AREA-START           PIC X(24)   VALUE                        
008000                                 'W271UT-AREA-START  '.                   
008100                                                                          
008200 01  W001-DAP.                                                            
008300     03  FILLER                  PIC X(165)  VALUE SPACE.                 
008400                                                                          
008500 01  UT-AREOR.                                                            
008600     03 UT-RPT-PRINT-LINES.                                               
008700       05 UT-HEADING-1.                                                   
008800         10 FILLER               PIC X(1)  VALUE SPACE.                   
008900         10 FILLER               PIC X(10) VALUE 'VOLVO CAR'.             
009000         10 FILLER               PIC X(17) VALUE 'AFTERSALES'.            
009100         10 FILLER               PIC X(11) VALUE SPACE.                   
009200         10 UT-LIST-NAME         PIC X(12) VALUE SPACE.                   
009300         10 FILLER               PIC X(12) VALUE SPACE.                   
009400         10 FILLER               PIC X(13) VALUE 'DC-WAREHOUSE'.          
009500         10 UT-IDDC              PIC X(2)  VALUE SPACE.                   
009600         10 FILLER               PIC X(5)  VALUE SPACE.                   
009700         10 FILLER               PIC X(7)  VALUE 'TOP 200'.               
009800         10 FILLER               PIC X(11) VALUE SPACE.                   
009900         10 UT-REPORT-DATE       PIC X(8)  VALUE SPACE.                   
010000       05 UT-HEADING-2.                                                   
010100         10 FILLER               PIC X(39) VALUE SPACE.                   
010200         10 FILLER               PIC X(12) VALUE 'BACKORDERED'.           
010300         10 FILLER               PIC X(9)  VALUE 'ITEMS BY'.              
010400         10 FILLER               PIC X(10) VALUE 'ORDERLINES'.            
010500         10 FILLER               PIC X(10) VALUE SPACE.                   
010600         10 UT-BUYER             PIC X(10) VALUE 'ALL BUYERS'.            
010700       05 UT-HEADING-3.                                                   
010800         10 FILLER               PIC X(5)  VALUE SPACE.                   
010900         10 FILLER               PIC X(5)  VALUE 'BUYER'.                 
011000         10 FILLER               PIC X(4)  VALUE SPACE.                   
011100         10 FILLER               PIC X(4)  VALUE 'PART'.                  
011200         10 FILLER               PIC X(27) VALUE SPACE.                   
011300         10 FILLER               PIC X(4)  VALUE 'FUNC'.                  
011400         10 FILLER               PIC X(1)  VALUE SPACE.                   
011500         10 FILLER               PIC X(6)   VALUE 'FREEZE'.               
011600         10 FILLER               PIC X(1)  VALUE SPACE.                   
011700         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
011800         10 FILLER               PIC X(9)  VALUE SPACE.                   
011900         10 FILLER               PIC X(7)  VALUE 'AVERAGE'.               
012000         10 FILLER               PIC X(6)  VALUE SPACE.                   
012100         10 FILLER               PIC X(4)  VALUE 'FORE'.                  
012200         10 FILLER               PIC X(5)  VALUE SPACE.                   
012300         10 FILLER               PIC X(2)  VALUE 'ON'.                    
012400         10 FILLER               PIC X(6)  VALUE SPACE.                   
012500         10 FILLER               PIC X(2)  VALUE 'ON'.                    
012600         10 FILLER               PIC X(13) VALUE SPACE.                   
012700         10 FILLER               PIC X(3)  VALUE 'BAL'.                   
012800       05 UT-HEADING-4.                                                   
012900         10 FILLER               PIC X(1)  VALUE SPACE.                   
013000         10 FILLER               PIC X(4)  VALUE 'RANK'.                  
013100         10 FILLER               PIC X(1)  VALUE SPACE.                   
013200         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
013300         10 FILLER               PIC X(3)  VALUE SPACE.                   
013400         10 FILLER               PIC X(6)  VALUE 'NUMBER'.                
013500         10 FILLER               PIC X(1)  VALUE SPACE.                   
013600         10 FILLER               PIC X(11) VALUE 'DESCRIPTION'.           
013700         10 FILLER               PIC X(10) VALUE SPACE.                   
013800         10 FILLER               PIC X(3)  VALUE 'S/C'.                   
013900         10 FILLER               PIC X(2)  VALUE SPACE.                   
014000         10 FILLER               PIC X(3)  VALUE 'GRP'.                   
014100         10 FILLER               PIC X(2)  VALUE SPACE.                   
014200         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
014300         10 FILLER               PIC X(2)  VALUE SPACE.                   
014400         10 FILLER               PIC X(5)  VALUE 'LINES'.                 
014500         10 FILLER               PIC X(2)  VALUE SPACE.                   
014600         10 FILLER               PIC X(3)  VALUE 'QTY'.                   
014700         10 FILLER               PIC X(6)  VALUE SPACE.                   
014800         10 FILLER               PIC X(4)  VALUE 'COST'.                  
014900         10 FILLER               PIC X(7)  VALUE SPACE.                   
015000         10 FILLER               PIC X(4)  VALUE 'CAST'.                  
015100         10 FILLER               PIC X(4)  VALUE SPACE.                   
015200         10 FILLER               PIC X(4)  VALUE 'HAND'.                  
015300         10 FILLER               PIC X(3)  VALUE SPACE.                   
015400         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
015500         10 FILLER               PIC X(5)  VALUE SPACE.                   
015600         10 FILLER               PIC X(2)  VALUE 'AK'.                    
015700         10 FILLER               PIC X(5)  VALUE SPACE.                   
015800         10 FILLER               PIC X(3)  VALUE 'CDC'.                   
015900       05 UT-RAD-1.                                                       
016000         10 FILLER               PIC X     VALUE SPACE.                   
016100         10 UT-RANK              PIC Z(3)9 VALUE ZERO.                    
016200         10 FILLER               PIC X(1)  VALUE SPACE.                   
016300         10 UT-IDPERSON-BUY      PIC ZZ9   VALUE ZERO.                    
016400         10 FILLER               PIC X     VALUE SPACE.                   
016500         10 UT-IDARTNR           PIC Z(8)9 VALUE ZERO.                    
016600         10 FILLER               PIC X(1)  VALUE SPACE.                   
016700         10 UT-BEART             PIC X(19) VALUE SPACE.                   
016800         10 FILLER               PIC X(2)  VALUE SPACE.                   
016900         10 UT-KDERS             PIC Z9    VALUE ZERO.                    
017000         10 FILLER               PIC X(1)  VALUE SPACE.                   
017100         10 UT-IDFKNGRP          PIC Z(4)9 VALUE ZERO.                    
017200         10 FILLER               PIC X(3)  VALUE SPACE.                   
017300         10 UT-FREEZECODE        PIC X     VALUE SPACE.                   
017400         10 FILLER               PIC X(1)  VALUE SPACE.                   
017500         10 UT-ANTAL-ORDRAD      PIC Z(5)9 VALUE ZERO.                    
017600         10 FILLER               PIC X(1)  VALUE SPACE.                   
017700         10 UT-ANTAL-QTY         PIC Z(5)9 VALUE ZERO.                    
017800         10 FILLER               PIC X(1)  VALUE SPACE.                   
017900         10 UT-PRAVCOST          PIC Z(6)9.9(2)                           
018000                                           VALUE ZERO.                    
018100         10 FILLER               PIC X(1)  VALUE SPACE.                   
018200         10 UT-KVPB-REF          PIC Z(5)9.9(2)                           
018300                                           VALUE ZERO.                    
018400         10 FILLER               PIC X(1)  VALUE SPACE.                   
018500         10 UT-ONHAND            PIC Z(6)9 VALUE ZERO.                    
018600         10 FILLER               PIC X(1)  VALUE SPACE.                   
018700         10 UT-KVBEART           PIC Z(6)9 VALUE ZERO.                    
018800         10 FILLER               PIC X(1)  VALUE SPACE.                   
018900         10 UT-KVAKS-SDC         PIC Z(5)9 VALUE ZERO.                    
019000         10 FILLER               PIC X(1)  VALUE SPACE.                   
019100         10 UT-AVAIL-CDC         PIC Z(6)9 VALUE ZERO.                    
019200                                                                          
019400 PROCEDURE DIVISION.                                                      
019500 MAIN SECTION.                                                            
019600                                                                          
019700     PERFORM A-INIT                                                       
019800     PERFORM B-SKAPA-POSTER                                               
019900     PERFORM Z-FINIT                                                      
020000                                                                          
020100     MOVE ZERO TO RETURN-CODE                                             
020200     GOBACK                                                               
020300     .                                                                    
020500                                                                          
020600                                                                          
020700 A-INIT SECTION.                                                          
020800                                                                          
020900     OPEN INPUT  W27165                                                   
021000                                                                          
021100     OPEN OUTPUT W271UT-ALL                                               
021200                                                                          
021300     ACCEPT DAGENS-DATUM     FROM DATE                                    
021400     MOVE DAGENS-DATUM-AAR   TO WS-RED-AA                                 
021500     MOVE DAGENS-DATUM-MAANAD                                             
021600                             TO WS-RED-MM                                 
021700     MOVE DAGENS-DATUM-DAG   TO WS-RED-DD                                 
021800     MOVE WS-RED-DATUM       TO UT-REPORT-DATE                            
021900     MOVE ZERO               TO WS-ANTAL-ARTIKLAR-ALL                     
022000     .                                                                    
022100                                                                          
022200 B-SKAPA-POSTER SECTION.                                                  
022300     PERFORM S01-LAES-W27165                                              
022400                                                                          
022500     PERFORM UNTIL END-OF-W27165                                          
022600       IF IN-IDDC NOT = WS-IDDC                                           
022610          PERFORM S02-SKRIV-DAP1                                          
022620          PERFORM S03-SKRIV-DAP2                                          
022630          MOVE IN-IDDC       TO UT-IDDC                                   
022640          WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-1                    
022641          WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-2                    
022642          WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-3                    
022643          WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-4                    
022680          MOVE IN-IDDC TO WS-IDDC                                         
022681          MOVE ZERO    TO WS-ANTAL-ARTIKLAR-ALL                           
022690       END-IF                                                             
022700       MOVE IN-IDPERSON-BUY  TO UT-IDPERSON-BUY                           
022800       MOVE IN-IDARTNR       TO UT-IDARTNR                                
022900       MOVE IN-BEART         TO UT-BEART                                  
023000       MOVE IN-IDFKNGRP      TO UT-IDFKNGRP                               
023100       MOVE IN-FREEZECODE    TO UT-FREEZECODE                             
023200       MOVE IN-ANTAL-ORDRAD  TO UT-ANTAL-ORDRAD                           
023300       MOVE IN-ANTAL-QTY     TO UT-ANTAL-QTY                              
023400       MOVE IN-PRAVCOST      TO UT-PRAVCOST                               
023500       MOVE IN-KVPB-REF      TO UT-KVPB-REF                               
023600       MOVE IN-ONHAND        TO UT-ONHAND                                 
023700       MOVE IN-KVBEART       TO UT-KVBEART                                
023800       MOVE IN-KVAKS-SDC     TO UT-KVAKS-SDC                              
023900       MOVE IN-KDERS         TO UT-KDERS                                  
024000       MOVE IN-AVAIL-CDC     TO UT-AVAIL-CDC                              
024100       IF WS-ANTAL-ARTIKLAR-ALL < 200                                     
024200         ADD 1               TO WS-ANTAL-ARTIKLAR-ALL                     
024300         MOVE WS-ANTAL-ARTIKLAR-ALL                                       
024310                             TO UT-RANK                                   
024320         MOVE UT-RAD-1       TO ORDER-REPORT-REC-ALL                      
024330         WRITE ORDER-REPORT-REC-ALL                                       
024400       END-IF                                                             
024500       PERFORM S01-LAES-W27165                                            
024600     END-PERFORM                                                          
024700     .                                                                    
024800                                                                          
028000 Z-FINIT SECTION.                                                         
028010                                                                          
028100     CLOSE W27165                                                         
028200           W271UT-ALL                                                     
028201     .                                                                    
028202                                                                          
028210 S01-LAES-W27165  SECTION.                                                
028220     READ W27165 INTO IN-AREA                                             
028230     AT END                                                               
028240        SET END-OF-W27165 TO TRUE                                         
028250     END-READ                                                             
028260     .                                                                    
028281                                                                          
028290 S02-SKRIV-DAP1 SECTION.                                                  
028291                                                                          
028292     MOVE ' ¤DAPW27165' TO W001-DAP                                       
028293     WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                             
028294                                                                          
028295     MOVE SPACE TO W001-DAP                                               
028296     .                                                                    
028297                                                                          
028298 S03-SKRIV-DAP2 SECTION.                                                  
028299                                                                          
028300     STRING ' ¤DAP' IN-IDDC                                               
028310            DELIMITED BY SIZE INTO W001-DAP                               
028320     WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                             
028330                                                                          
028340     MOVE SPACE TO W001-DAP                                               
028400     .                                                                    

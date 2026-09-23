000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2717100.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   FEB 1998.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                                                                         
001000*        PROGRAMMET SKRIVER UT EN LISTA ÖVER 'TOPP 100                    
001100*        RESTORDER' FÖR NDC/LAND I ORDERLINES ORDNING                     
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
002400     SELECT W27171                     ASSIGN TO W27171D1.                
002500     SKIP2                                                                
002600*          ---                                                            
002700     SELECT W271UT-ALL                 ASSIGN TO W27171D2.                
002800                                                                          
002900     SELECT W271UT-80                  ASSIGN TO W27171D3.                
003000                                                                          
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600                                                                          
003700 FD  W27171                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  POST  -COPY W27162    -PRE IN-    -L.                                
004200                                                                          
004300 FD  W271UT-ALL                                                           
004400     RECORDING       V                                                    
004500     BLOCK CONTAINS  0 RECORDS.                                           
004600 01  ORDER-REPORT-REC-ALL    PIC X(124).                                  
004700                                                                          
004800 FD  W271UT-80                                                            
004900     RECORDING       V                                                    
005000     BLOCK CONTAINS  0 RECORDS.                                           
005100 01  ORDER-REPORT-REC-80     PIC X(124).                                  
005200                                                                          
005300                                                                          
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800 77  IDPGM                       PIC X(8)    VALUE 'W2717100'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100                                                                          
006200 77  W27171-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-W27171                       VALUE 'J'.                   
006410                                                                          
006420 01  SUBPROGRAM.                                                          
006440     03  WISOLAND                PIC X(8)    VALUE 'WISOLAND'.            
006450                                                                          
006460*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
006470*                                                                         
006480 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
006490     SKIP3                                                                
006491*01 -COPY WISOLAND                                                        
006492                                                                          
006493 01  WS-IDDC                 PIC X(2)    VALUE SPACE.                     
006494                                                                          
006495 01  WS-IDLAND-CURR          PIC X(2)    VALUE SPACE.                     
006499                                                                          
006500 01  ARBETSAREOR.                                                         
007000     03 WS-RED-DATUM.                                                     
007100       05  WS-RED-MM             PIC X(2)   VALUE SPACE.                  
007200       05  FILLER                PIC X      VALUE '/'.                    
007300       05  WS-RED-DD             PIC X(2)   VALUE SPACE.                  
007400       05  FILLER                PIC X      VALUE '/'.                    
007500       05  WS-RED-AA             PIC X(2)   VALUE SPACE.                  
007600     03 WS-ANTAL-ARTIKLAR-ALL    PIC 9(8)   VALUE ZERO.                   
007700     03 WS-ANTAL-ARTIKLAR-80     PIC 9(8)   VALUE ZERO.                   
007800     03 WS-ANTAL-ORDRAD-ALL      PIC 9(8)   VALUE ZERO.                   
007900     03 WS-ANTAL-QTY-ALL         PIC 9(8)   VALUE ZERO.                   
008000     03 WS-ANTAL-ORDRAD-80       PIC 9(8)   VALUE ZERO.                   
008100     03 WS-ANTAL-QTY-80          PIC 9(8)   VALUE ZERO.                   
008200     03 WS-RAD                   PIC X(120) VALUE SPACE.                  
008300                                                                          
008400                                                                          
008500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008600 01  FILLER REDEFINES DAGENS-DATUM.                                       
008700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009000                                                                          
009100                                                                          
009200 01  FELTEXT.                                                             
009300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009500                                                                          
009600     EJECT                                                                
009700 01  W27171-AREA-START           PIC X(24)   VALUE                        
009800                                 'W27171-AREA-START  '.                   
009900                                                                          
010000*01  AREA -COPY W27162     -PRE IN-                                       
010100                                                                          
010200     EJECT                                                                
010300 01  W271UT-AREA-START           PIC X(24)   VALUE                        
010400                                 'W271UT-AREA-START  '.                   
010500                                                                          
010510 01  W001-DAP.                                                            
010520     03  FILLER                  PIC X(165)  VALUE SPACE.                 
010530                                                                          
010600 01  UT-AREOR.                                                            
010700     03 UT-RPT-PRINT-LINES.                                               
010800       05 UT-HEADING-1.                                                   
010900         10 FILLER               PIC X(1)  VALUE SPACE.                   
011000         10 FILLER               PIC X(10) VALUE 'VOLVO CAR'.             
011100         10 FILLER               PIC X(17) VALUE 'AFTERSALES'.            
011200         10 FILLER               PIC X(11) VALUE SPACE.                   
011300         10 UT-LIST-NAME         PIC X(12) VALUE SPACE.                   
011400         10 FILLER               PIC X(12) VALUE SPACE.                   
011500         10 FILLER               PIC X(6)  VALUE 'TOTAL '.                
011510         10 UT-LIST-COUNTRY      PIC X(13) VALUE SPACE.                   
011600         10 FILLER               PIC X(2)  VALUE SPACE.                   
011700         10 FILLER               PIC X(7)  VALUE 'TOP 100'.               
011800         10 FILLER               PIC X(13) VALUE SPACE.                   
011900         10 UT-REPORT-DATE       PIC X(8)  VALUE SPACE.                   
012000         10 FILLER               PIC X(1)  VALUE SPACE.                   
012300                                                                          
012400       05 UT-HEADING-2.                                                   
012500         10 FILLER               PIC X(39) VALUE SPACE.                   
012600         10 FILLER               PIC X(12) VALUE 'BACKORDERED'.           
012700         10 FILLER               PIC X(9)  VALUE 'ITEMS BY'.              
012800         10 FILLER               PIC X(11) VALUE 'ORDERLINES'.            
012900         10 FILLER               PIC X(9)  VALUE 'LAST WEEK'.             
013000         10 FILLER               PIC X(10) VALUE SPACE.                   
013100         10 UT-BUYER             PIC X(10) VALUE SPACE.                   
013200       05 UT-HEADING-3.                                                   
013300         10 FILLER               PIC X(5)  VALUE SPACE.                   
013400         10 FILLER               PIC X(5)  VALUE 'BUYER'.                 
013500         10 FILLER               PIC X(4)  VALUE SPACE.                   
013600         10 FILLER               PIC X(4)  VALUE 'PART'.                  
013700         10 FILLER               PIC X(27) VALUE SPACE.                   
013800         10 FILLER               PIC X(4)  VALUE 'FUNC'.                  
013900         10 FILLER               PIC X(1)  VALUE SPACE.                   
014000         10 FILLER               PIC X(6)   VALUE 'FREEZE'.               
014100         10 FILLER               PIC X(1)  VALUE SPACE.                   
014200         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
014300         10 FILLER               PIC X(9)  VALUE SPACE.                   
014400         10 FILLER               PIC X(7)  VALUE 'AVERAGE'.               
014500         10 FILLER               PIC X(6)  VALUE SPACE.                   
014600         10 FILLER               PIC X(4)  VALUE 'FORE'.                  
014700         10 FILLER               PIC X(5)  VALUE SPACE.                   
014800         10 FILLER               PIC X(2)  VALUE 'ON'.                    
014900         10 FILLER               PIC X(6)  VALUE SPACE.                   
015000         10 FILLER               PIC X(2)  VALUE 'ON'.                    
015100         10 FILLER               PIC X(13) VALUE SPACE.                   
015200         10 FILLER               PIC X(3)  VALUE 'BAL'.                   
015300       05 UT-HEADING-4.                                                   
015400         10 FILLER               PIC X(1)  VALUE SPACE.                   
015500         10 FILLER               PIC X(4)  VALUE 'RANK'.                  
015600         10 FILLER               PIC X(1)  VALUE SPACE.                   
015700         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
015800         10 FILLER               PIC X(3)  VALUE SPACE.                   
015900         10 FILLER               PIC X(6)  VALUE 'NUMBER'.                
016000         10 FILLER               PIC X(1)  VALUE SPACE.                   
016100         10 FILLER               PIC X(11) VALUE 'DESCRIPTION'.           
016200         10 FILLER               PIC X(10) VALUE SPACE.                   
016300         10 FILLER               PIC X(3)  VALUE 'S/C'.                   
016400         10 FILLER               PIC X(2)  VALUE SPACE.                   
016500         10 FILLER               PIC X(3)  VALUE 'GRP'.                   
016600         10 FILLER               PIC X(2)  VALUE SPACE.                   
016700         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
016800         10 FILLER               PIC X(2)  VALUE SPACE.                   
016900         10 FILLER               PIC X(5)  VALUE 'LINES'.                 
017000         10 FILLER               PIC X(2)  VALUE SPACE.                   
017100         10 FILLER               PIC X(3)  VALUE 'QTY'.                   
017200         10 FILLER               PIC X(6)  VALUE SPACE.                   
017300         10 FILLER               PIC X(4)  VALUE 'COST'.                  
017400         10 FILLER               PIC X(7)  VALUE SPACE.                   
017500         10 FILLER               PIC X(4)  VALUE 'CAST'.                  
017600         10 FILLER               PIC X(4)  VALUE SPACE.                   
017700         10 FILLER               PIC X(4)  VALUE 'HAND'.                  
017800         10 FILLER               PIC X(3)  VALUE SPACE.                   
017900         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
018000         10 FILLER               PIC X(5)  VALUE SPACE.                   
018100         10 FILLER               PIC X(2)  VALUE 'AK'.                    
018200         10 FILLER               PIC X(5)  VALUE SPACE.                   
018300         10 FILLER               PIC X(3)  VALUE 'CDC'.                   
018400       05 UT-RAD-1.                                                       
018500         10 FILLER               PIC X     VALUE SPACE.                   
018600         10 UT-RANK              PIC Z(3)9 VALUE ZERO.                    
018700         10 FILLER               PIC X(1)  VALUE SPACE.                   
018800         10 UT-IDPERSON-BUY      PIC ZZ9   VALUE ZERO.                    
018900         10 FILLER               PIC X     VALUE SPACE.                   
019000         10 UT-IDARTNR           PIC Z(8)9 VALUE ZERO.                    
019100         10 FILLER               PIC X(1)  VALUE SPACE.                   
019200         10 UT-BEART             PIC X(19) VALUE SPACE.                   
019300         10 FILLER               PIC X(2)  VALUE SPACE.                   
019400         10 UT-KDERS             PIC Z9    VALUE ZERO.                    
019500         10 FILLER               PIC X(1)  VALUE SPACE.                   
019600         10 UT-IDFKNGRP          PIC Z(4)9 VALUE ZERO.                    
019700         10 FILLER               PIC X(3)  VALUE SPACE.                   
019800         10 UT-FREEZECODE        PIC X     VALUE SPACE.                   
019900         10 FILLER               PIC X(1)  VALUE SPACE.                   
020000         10 UT-ANTAL-ORDRAD      PIC Z(5)9 VALUE ZERO.                    
020100         10 FILLER               PIC X(1)  VALUE SPACE.                   
020200         10 UT-ANTAL-QTY         PIC Z(5)9 VALUE ZERO.                    
020300         10 FILLER               PIC X(1)  VALUE SPACE.                   
020400         10 UT-PRAVCOST          PIC Z(6)9.9(2)                           
020500                                           VALUE ZERO.                    
020600         10 FILLER               PIC X(1)  VALUE SPACE.                   
020700         10 UT-KVPB-REF          PIC Z(5)9.9(2)                           
020800                                           VALUE ZERO.                    
020900         10 FILLER               PIC X(1)  VALUE SPACE.                   
021000         10 UT-ONHAND            PIC Z(6)9 VALUE ZERO.                    
021100         10 FILLER               PIC X(1)  VALUE SPACE.                   
021200         10 UT-KVBEART           PIC Z(6)9 VALUE ZERO.                    
021300         10 FILLER               PIC X(1)  VALUE SPACE.                   
021400         10 UT-KVAKS-SDC         PIC Z(5)9 VALUE ZERO.                    
021500         10 FILLER               PIC X(1)  VALUE SPACE.                   
021600         10 UT-AVAIL-CDC         PIC Z(6)9 VALUE ZERO.                    
021700       05 UT-RAD-TOT.                                                     
021800         10 FILLER               PIC X(48) VALUE SPACE.                   
021900         10 FILLER               PIC X(6)  VALUE 'TOTAL'.                 
022000         10 UT-ANTAL-ORDRAD-TOT  PIC Z(5)9 VALUE ZERO.                    
022100         10 FILLER               PIC X(1)  VALUE SPACE.                   
022200         10 UT-ANTAL-QTY-TOT     PIC Z(5)9 VALUE ZERO.                    
022300                                                                          
022400     EJECT                                                                
022500 PROCEDURE DIVISION.                                                      
022600 MAIN SECTION.                                                            
022700                                                                          
022800     PERFORM A-INIT                                                       
022900     PERFORM B-SKAPA-POSTER                                               
023000     PERFORM Z-FINIT                                                      
023100                                                                          
023200     MOVE ZERO TO RETURN-CODE                                             
023300     GOBACK                                                               
023400     .                                                                    
023500                                                                          
023600     EJECT                                                                
023700 A-INIT SECTION.                                                          
023800                                                                          
023900     OPEN INPUT  W27171                                                   
024000                                                                          
024100     OPEN OUTPUT W271UT-ALL                                               
024200                 W271UT-80                                                
024300                                                                          
024400     ACCEPT DAGENS-DATUM     FROM DATE                                    
024500     MOVE DAGENS-DATUM-AAR   TO WS-RED-AA                                 
024600     MOVE DAGENS-DATUM-MAANAD                                             
024700                             TO WS-RED-MM                                 
024800     MOVE DAGENS-DATUM-DAG   TO WS-RED-DD                                 
024900     MOVE WS-RED-DATUM       TO UT-REPORT-DATE                            
025000     MOVE ZERO               TO                                           
025100                                                                          
025400                                WS-ANTAL-ARTIKLAR-ALL                     
025500                                WS-ANTAL-ARTIKLAR-80                      
025600                                WS-ANTAL-ORDRAD-ALL                       
025700                                WS-ANTAL-ORDRAD-80                        
025800                                WS-ANTAL-QTY-ALL                          
025900                                WS-ANTAL-QTY-80                           
026000     .                                                                    
026100                                                                          
026200     EJECT                                                                
026300 B-SKAPA-POSTER SECTION.                                                  
026310                                                                          
026400     PERFORM S01-LAES-W27171                                              
026550     MOVE SPACE        TO WS-IDDC                                         
026560                          WS-IDLAND-CURR                                  
026600                                                                          
026700     PERFORM UNTIL END-OF-W27171                                          
026710        IF IN-IDDC NOT = WS-IDDC                                          
026720           PERFORM S10-SOK-IDLAND                                         
026730        END-IF                                                            
026731                                                                          
026732        IF IN-IDLANDX2 NOT = WS-IDLAND-CURR                               
026740          IF WS-IDDC NOT = SPACE                                          
026741             PERFORM BC-AVSLUTA-LAND                                      
026742          END-IF                                                          
026743          MOVE IN-IDLANDX2 TO WS-IDLAND-CURR                              
026744          MOVE IN-IDDC     TO WS-IDDC                                     
026746          PERFORM BA-REPORT-HEADER-ALL                                    
026747          PERFORM BB-REPORT-HEADER-80                                     
026797                                                                          
026800          MOVE ZERO TO WS-ANTAL-ARTIKLAR-ALL                              
026801                       WS-ANTAL-ARTIKLAR-80                               
026802                       WS-ANTAL-ORDRAD-ALL                                
026803                       WS-ANTAL-ORDRAD-80                                 
026809        END-IF                                                            
026810                                                                          
026820        MOVE IN-IDPERSON-BUY   TO UT-IDPERSON-BUY                         
026900        MOVE IN-IDARTNR        TO UT-IDARTNR                              
027000        MOVE IN-BEART          TO UT-BEART                                
027100        MOVE IN-IDFKNGRP       TO UT-IDFKNGRP                             
027200        MOVE IN-FREEZECODE     TO UT-FREEZECODE                           
027300        MOVE IN-ANTAL-ORDRAD   TO UT-ANTAL-ORDRAD                         
027400        MOVE IN-ANTAL-QTY      TO UT-ANTAL-QTY                            
027500        MOVE IN-PRAVCOST       TO UT-PRAVCOST                             
027600        MOVE IN-KVPB-REF       TO UT-KVPB-REF                             
027700        MOVE IN-ONHAND         TO UT-ONHAND                               
027800        MOVE IN-KVBEART        TO UT-KVBEART                              
027900        MOVE IN-KVAKS-SDC      TO UT-KVAKS-SDC                            
028000        MOVE IN-KDERS          TO UT-KDERS                                
028100        MOVE IN-AVAIL-CDC      TO UT-AVAIL-CDC                            
028110                                                                          
028200        IF WS-ANTAL-ARTIKLAR-ALL < 100                                    
028300           ADD 1               TO WS-ANTAL-ARTIKLAR-ALL                   
028400           MOVE WS-ANTAL-ARTIKLAR-ALL                                     
028500                               TO UT-RANK                                 
028600           ADD IN-ANTAL-ORDRAD TO WS-ANTAL-ORDRAD-ALL                     
028700           ADD IN-ANTAL-QTY    TO WS-ANTAL-QTY-ALL                        
028800           MOVE SPACE          TO WS-RAD                                  
028900           MOVE UT-RAD-1       TO W001-DAP                                
029010           WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                       
029100        END-IF                                                            
029110                                                                          
029200        IF IN-IDPERSON-BUY > 79                                           
029300        AND WS-ANTAL-ARTIKLAR-80 < 100                                    
029400           ADD 1               TO WS-ANTAL-ARTIKLAR-80                    
029500           MOVE WS-ANTAL-ARTIKLAR-80                                      
029600                               TO UT-RANK                                 
029700           ADD IN-ANTAL-ORDRAD TO WS-ANTAL-ORDRAD-80                      
029800           ADD IN-ANTAL-QTY    TO WS-ANTAL-QTY-80                         
029900           MOVE SPACE          TO WS-RAD                                  
030000           MOVE UT-RAD-1       TO W001-DAP                                
030110           WRITE ORDER-REPORT-REC-80  FROM W001-DAP                       
030200        END-IF                                                            
030300        PERFORM S01-LAES-W27171                                           
030400     END-PERFORM                                                          
030401                                                                          
030402     PERFORM BC-AVSLUTA-LAND                                              
030410     .                                                                    
033100 BA-REPORT-HEADER-ALL SECTION.                                            
033200                                                                          
033300     PERFORM S02-SKRIV-DAP1-ALL                                           
033310     PERFORM S03-SKRIV-DAP2-ALL                                           
033400                                                                          
033600     MOVE 'W27171-ALL'     TO UT-LIST-NAME                                
033700     MOVE 'ALL BUYERS'     TO UT-BUYER                                    
033800                                                                          
034010     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-1                         
034020     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-2                         
034030     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-3                         
034040     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-4                         
035300     .                                                                    
035400                                                                          
035500     EJECT                                                                
035600 BB-REPORT-HEADER-80 SECTION.                                             
035700                                                                          
035800     PERFORM S04-SKRIV-DAP1-80                                            
035900     PERFORM S05-SKRIV-DAP2-80                                            
036000                                                                          
036100     MOVE 'W27171-080'     TO UT-LIST-NAME                                
036200     MOVE 'BUYER > 80'     TO UT-BUYER                                    
037110     WRITE ORDER-REPORT-REC-80 FROM UT-HEADING-1                          
037120     WRITE ORDER-REPORT-REC-80 FROM UT-HEADING-2                          
037130     WRITE ORDER-REPORT-REC-80 FROM UT-HEADING-3                          
037140     WRITE ORDER-REPORT-REC-80 FROM UT-HEADING-4                          
037800     .                                                                    
037900                                                                          
038000     EJECT                                                                
038010 BC-AVSLUTA-LAND SECTION.                                                 
038020                                                                          
038030**************************************************'                       
038040*                                                                         
038050*    SKRIV EN TOTALRAD FÖR SAMTLIGA LISTOR                                
038060*                                                                         
038070**************************************************'                       
038080                                                                          
038090     IF WS-ANTAL-ARTIKLAR-ALL = 0                                         
038091       CONTINUE                                                           
038092     ELSE                                                                 
038093       MOVE SPACE            TO W001-DAP                                  
038094       WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                           
038095       MOVE WS-ANTAL-ORDRAD-ALL                                           
038096                               TO UT-ANTAL-ORDRAD-TOT                     
038097       MOVE WS-ANTAL-QTY-ALL TO UT-ANTAL-QTY-TOT                          
038098       MOVE SPACE            TO WS-RAD                                    
038099       WRITE ORDER-REPORT-REC-ALL FROM UT-RAD-TOT                         
038100     END-IF                                                               
038101                                                                          
038102     MOVE SPACE              TO W001-DAP                                  
038103     WRITE ORDER-REPORT-REC-80  FROM W001-DAP                             
038104     MOVE WS-ANTAL-ORDRAD-80 TO UT-ANTAL-ORDRAD-TOT                       
038105     MOVE WS-ANTAL-QTY-80    TO UT-ANTAL-QTY-TOT                          
038106     MOVE SPACE              TO WS-RAD                                    
038107     WRITE ORDER-REPORT-REC-80  FROM UT-RAD-TOT                           
038108     .                                                                    
038109                                                                          
038110     EJECT                                                                
038120 Z-FINIT SECTION.                                                         
038200     CLOSE W27171                                                         
038300           W271UT-ALL                                                     
038400           W271UT-80                                                      
038500     .                                                                    
038600                                                                          
038700     EJECT                                                                
038800 S01-LAES-W27171  SECTION.                                                
038900     READ W27171 INTO IN-AREA                                             
039000     AT END                                                               
039100        SET END-OF-W27171 TO TRUE                                         
039200                                                                          
039300     END-READ                                                             
039400     .                                                                    
039500 S02-SKRIV-DAP1-ALL SECTION.                                              
039600                                                                          
039700     MOVE ' ¤DAPW27171-001' TO W001-DAP                                   
039810     WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                             
039900                                                                          
040000     MOVE SPACE TO W001-DAP                                               
040100     .                                                                    
040200                                                                          
040300 S03-SKRIV-DAP2-ALL SECTION.                                              
040400                                                                          
040500     STRING ' ¤DAP' WS-IDLAND-CURR                                        
040600            DELIMITED BY SIZE INTO W001-DAP                               
040710     WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                             
040800                                                                          
040900     MOVE SPACE TO W001-DAP                                               
041000     .                                                                    
041100 S04-SKRIV-DAP1-80 SECTION.                                               
041200                                                                          
041300     MOVE ' ¤DAPW27171-080' TO W001-DAP                                   
041410     WRITE ORDER-REPORT-REC-80 FROM W001-DAP                              
041500                                                                          
041600     MOVE SPACE TO W001-DAP                                               
041700     .                                                                    
041800 S05-SKRIV-DAP2-80 SECTION.                                               
041900                                                                          
042000     STRING ' ¤DAP' WS-IDLAND-CURR                                        
042100            DELIMITED BY SIZE INTO W001-DAP                               
042110     WRITE ORDER-REPORT-REC-80 FROM W001-DAP                              
042300                                                                          
042400     MOVE SPACE TO W001-DAP                                               
042500     .                                                                    
042600 S10-SOK-IDLAND SECTION.                                                  
042700                                                                          
043402     IF IN-IDLANDX2 = SPACE                                               
043403        MOVE SPACE              TO UT-LIST-COUNTRY                        
043404     ELSE                                                                 
043420        MOVE IN-IDLANDX2        TO LAND-IDLANDX2                          
043430        MOVE SPACE              TO LAND-IDLANDX3                          
043440        CALL WISOLAND USING LAND-WISOLAND                                 
043450        IF LAND-KDSVAR = SPACE                                            
043460           MOVE LAND-BELAND-ENG TO UT-LIST-COUNTRY                        
043470        ELSE                                                              
043480           MOVE SPACE           TO UT-LIST-COUNTRY                        
043490        END-IF                                                            
043491     END-IF                                                               
043500     .                                                                    

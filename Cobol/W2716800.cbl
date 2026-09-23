000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2716800.                                                
000300 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000400 DATE-WRITTEN.   NOV 1997.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                                                                         
001000*        PROGRAMMET SKRIVER UT EN LISTA ÖVER 'TOPP 200                    
001100*        RESTORDER' FÖR NDC I ORDERLINES ORDNING                          
001200*                                                                         
001300*                                                                         
001400*                                                                         
001500*                                                                         
001600                                                                          
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          ---                                                            
002500     SELECT W27168                     ASSIGN TO W27168D1.                
002600     SKIP2                                                                
002700*          ---                                                            
002800     SELECT W271UT-ALL                 ASSIGN TO W27168D2.                
002900                                                                          
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800                                                                          
003900 FILE SECTION.                                                            
004000                                                                          
004100                                                                          
004200 FD  W27168                                                               
004300     RECORDING MODE F                                                     
004400     RECORD CONTAINS 80 CHARACTERS                                        
004500     LABEL RECORD STANDARD                                                
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800 01  IN-POST                 PIC X(80).                                   
004900                                                                          
005000 FD  W271UT-ALL                                                           
007200     RECORDING       V                                                    
007300     BLOCK CONTAINS  0.                                                   
007400                                                                          
007500 01  ORDER-REPORT-REC-ALL        PIC X(205).                              
007600     EJECT                                                                
007700 WORKING-STORAGE SECTION.                                                 
007800                                                                          
007900*    -- CHECKED BY WY2000                                                 
008000 77  IDPGM                       PIC X(8)    VALUE 'W2716800'.            
008100 77  JA                          PIC X       VALUE 'J'.                   
008200 77  NEJ                         PIC X       VALUE 'N'.                   
008300                                                                          
008400 77  W27168-EOF-SW               PIC X       VALUE 'N'.                   
008500     88  END-OF-W27168                       VALUE 'J'.                   
008600                                                                          
008700 01  ARBETSAREOR.                                                         
008800     03 WS-PAGE-ALL              PIC  9(4)  VALUE ZERO.                   
009100     03 WS-RADANT-ALL            PIC  9(2)  VALUE ZERO.                   
009400     03 WS-RED-DATUM.                                                     
009500       05  WS-RED-MM             PIC X(2)   VALUE SPACE.                  
009600       05  FILLER                PIC X      VALUE '/'.                    
009700       05  WS-RED-DD             PIC X(2)   VALUE SPACE.                  
009800       05  FILLER                PIC X      VALUE '/'.                    
009900       05  WS-RED-AA             PIC X(2)   VALUE SPACE.                  
010000     03 WS-ANTAL-ARTIKLAR-ALL    PIC 9(8)   VALUE ZERO.                   
010100     03 WS-ANTAL-ARTIKLAR-81     PIC 9(8)   VALUE ZERO.                   
010200     03 WS-ANTAL-ARTIKLAR-82     PIC 9(8)   VALUE ZERO.                   
010300                                                                          
010400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010500 01  FILLER REDEFINES DAGENS-DATUM.                                       
010600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
010700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010900                                                                          
011000                                                                          
011100 01  FELTEXT.                                                             
011200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011400                                                                          
011500     EJECT                                                                
011600 01  DYNAMISKA-SUBPROGRAM.                                                
011700*                                                                         
011800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011810     03  WISOLAND                PIC X(8)    VALUE 'WISOLAND'.            
011900     EJECT                                                                
012000*    --- PARAMETRAR TILL POSTSUM                                          
012100*                                                                         
012200*01  -COPY W0005   -PRE  POSTSUM-                                         
012210                                                                          
012220*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
012230*                                                                         
012240 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
012250                                                                          
012260*01 -COPY WISOLAND                                                        
012270                                                                          
012280 01  WS-IDDC                 PIC X(2)    VALUE SPACE.                     
012290                                                                          
012291 01  WS-IDLAND-CURR          PIC X(2)    VALUE SPACE.                     
012295                                                                          
012300     EJECT                                                                
012400 01  W27168-AREA-START           PIC X(24)   VALUE                        
012500                                 'W27168-AREA-START  '.                   
012600                                                                          
012700*01  AREA -COPY W27162     -PRE IN-                                       
012800                                                                          
012900     EJECT                                                                
013000 01  W271UT-AREA-START           PIC X(24)   VALUE                        
013100                                 'W271UT-AREA-START  '.                   
013200 01  UT-AREA-START             PIC X(24)   VALUE                          
013300                                 'UT-AREA-START  '.                       
013310                                                                          
013320 01  W001-DAP.                                                            
013330     03  FILLER                  PIC X(200)  VALUE SPACE.                 
013700                                                                          
013800                                                                          
013900 01  UT-AREOR.                                                            
014000     03 UT-RPT-PRINT-LINES.                                               
014100       05 UT-HEADING-1.                                                   
014200         10 FILLER               PIC X(1)  VALUE SPACE.                   
014300         10 FILLER               PIC X(11) VALUE 'VOLVO CARS'.            
014400         10 FILLER               PIC X(17) VALUE                          
014500                                 'CUSTOMER SERVICES'.                     
014600         10 FILLER               PIC X(10) VALUE SPACE.                   
014700         10 UT-LIST-NAME         PIC X(12) VALUE SPACE.                   
014800         10 FILLER               PIC X(12) VALUE SPACE.                   
014900         10 UT-LIST-COUNTRY      PIC X(13) VALUE SPACE.                   
015000         10 FILLER               PIC X(5)  VALUE SPACE.                   
015100         10 FILLER               PIC X(7)  VALUE 'TOP 200'.               
015200         10 FILLER               PIC X(13) VALUE SPACE.                   
015300         10 UT-REPORT-DATE       PIC X(8)  VALUE SPACE.                   
015800       05 UT-HEADING-2.                                                   
015900         10 FILLER               PIC X(39) VALUE SPACE.                   
016000         10 FILLER               PIC X(12) VALUE 'BACKORDERED'.           
016100         10 FILLER               PIC X(9)  VALUE 'ITEMS BY'.              
016200         10 FILLER               PIC X(10) VALUE 'ORDERLINES'.            
016300         10 FILLER               PIC X(10) VALUE SPACE.                   
016400         10 UT-BUYER             PIC X(10) VALUE 'ALL BUYERS'.            
016500       05 UT-HEADING-3.                                                   
016600         10 FILLER               PIC X(5)  VALUE SPACE.                   
016700         10 FILLER               PIC X(5)  VALUE 'BUYER'.                 
016800         10 FILLER               PIC X(4)  VALUE SPACE.                   
016900         10 FILLER               PIC X(4)  VALUE 'PART'.                  
017000         10 FILLER               PIC X(27) VALUE SPACE.                   
017100         10 FILLER               PIC X(4)  VALUE 'FUNC'.                  
017200         10 FILLER               PIC X(1)  VALUE SPACE.                   
017300         10 FILLER               PIC X(6)   VALUE 'FREEZE'.               
017400         10 FILLER               PIC X(1)  VALUE SPACE.                   
017500         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
017600         10 FILLER               PIC X(9)  VALUE SPACE.                   
017700         10 FILLER               PIC X(7)  VALUE 'AVERAGE'.               
017800         10 FILLER               PIC X(6)  VALUE SPACE.                   
017900         10 FILLER               PIC X(4)  VALUE 'FORE'.                  
018000         10 FILLER               PIC X(5)  VALUE SPACE.                   
018100         10 FILLER               PIC X(2)  VALUE 'ON'.                    
018200         10 FILLER               PIC X(6)  VALUE SPACE.                   
018300         10 FILLER               PIC X(2)  VALUE 'ON'.                    
018400         10 FILLER               PIC X(13) VALUE SPACE.                   
018500         10 FILLER               PIC X(3)  VALUE 'BAL'.                   
018600       05 UT-HEADING-4.                                                   
018700         10 FILLER               PIC X(1)  VALUE SPACE.                   
018800         10 FILLER               PIC X(4)  VALUE 'RANK'.                  
018900         10 FILLER               PIC X(1)  VALUE SPACE.                   
019000         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
019100         10 FILLER               PIC X(3)  VALUE SPACE.                   
019200         10 FILLER               PIC X(6)  VALUE 'NUMBER'.                
019300         10 FILLER               PIC X(1)  VALUE SPACE.                   
019400         10 FILLER               PIC X(11) VALUE 'DESCRIPTION'.           
019500         10 FILLER               PIC X(10) VALUE SPACE.                   
019600         10 FILLER               PIC X(3)  VALUE 'S/C'.                   
019700         10 FILLER               PIC X(2)  VALUE SPACE.                   
019800         10 FILLER               PIC X(3)  VALUE 'GRP'.                   
019900         10 FILLER               PIC X(2)  VALUE SPACE.                   
020000         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
020100         10 FILLER               PIC X(2)  VALUE SPACE.                   
020200         10 FILLER               PIC X(5)  VALUE 'LINES'.                 
020300         10 FILLER               PIC X(2)  VALUE SPACE.                   
020400         10 FILLER               PIC X(3)  VALUE 'QTY'.                   
020500         10 FILLER               PIC X(6)  VALUE SPACE.                   
020600         10 FILLER               PIC X(4)  VALUE 'COST'.                  
020700         10 FILLER               PIC X(7)  VALUE SPACE.                   
020800         10 FILLER               PIC X(4)  VALUE 'CAST'.                  
020900         10 FILLER               PIC X(4)  VALUE SPACE.                   
021000         10 FILLER               PIC X(4)  VALUE 'HAND'.                  
021100         10 FILLER               PIC X(3)  VALUE SPACE.                   
021200         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
021300         10 FILLER               PIC X(5)  VALUE SPACE.                   
021400         10 FILLER               PIC X(2)  VALUE 'AK'.                    
021500         10 FILLER               PIC X(5)  VALUE SPACE.                   
021600         10 FILLER               PIC X(3)  VALUE 'CDC'.                   
021700       05 UT-RAD-1.                                                       
021800         10 FILLER               PIC X     VALUE SPACE.                   
021900         10 UT-RANK              PIC Z(3)9 VALUE ZERO.                    
022000         10 FILLER               PIC X(1)  VALUE SPACE.                   
022100         10 UT-IDPERSON-BUY      PIC ZZ9   VALUE ZERO.                    
022200         10 FILLER               PIC X     VALUE SPACE.                   
022300         10 UT-IDARTNR           PIC Z(8)9 VALUE ZERO.                    
022400         10 FILLER               PIC X(1)  VALUE SPACE.                   
022500         10 UT-BEART             PIC X(19) VALUE SPACE.                   
022600         10 FILLER               PIC X(2)  VALUE SPACE.                   
022700         10 UT-KDERS             PIC Z9    VALUE ZERO.                    
022800         10 FILLER               PIC X(1)  VALUE SPACE.                   
022900         10 UT-IDFKNGRP          PIC Z(4)9 VALUE ZERO.                    
023000         10 FILLER               PIC X(3)  VALUE SPACE.                   
023100         10 UT-FREEZECODE        PIC X     VALUE SPACE.                   
023200         10 FILLER               PIC X(1)  VALUE SPACE.                   
023300         10 UT-ANTAL-ORDRAD      PIC Z(5)9 VALUE ZERO.                    
023400         10 FILLER               PIC X(1)  VALUE SPACE.                   
023500         10 UT-ANTAL-QTY         PIC Z(5)9 VALUE ZERO.                    
023600         10 FILLER               PIC X(1)  VALUE SPACE.                   
023700         10 UT-PRAVCOST          PIC Z(6)9.9(2)                           
023800                                           VALUE ZERO.                    
023900         10 FILLER               PIC X(1)  VALUE SPACE.                   
024000         10 UT-KVPB-REF          PIC Z(5)9.9(2)                           
024100                                           VALUE ZERO.                    
024200         10 FILLER               PIC X(1)  VALUE SPACE.                   
024300         10 UT-ONHAND            PIC Z(6)9 VALUE ZERO.                    
024400         10 FILLER               PIC X(1)  VALUE SPACE.                   
024500         10 UT-KVBEART           PIC Z(6)9 VALUE ZERO.                    
024600         10 FILLER               PIC X(1)  VALUE SPACE.                   
024700         10 UT-KVAKS-SDC         PIC Z(5)9 VALUE ZERO.                    
024800         10 FILLER               PIC X(1)  VALUE SPACE.                   
024900         10 UT-AVAIL-CDC         PIC Z(6)9 VALUE ZERO.                    
025000                                                                          
025100     EJECT                                                                
025200 PROCEDURE DIVISION.                                                      
025300 MAIN SECTION.                                                            
025400                                                                          
025500     PERFORM A-INIT                                                       
025600     PERFORM B-SKAPA-POSTER                                               
025700     PERFORM Z-FINIT                                                      
025800                                                                          
025900     MOVE ZERO TO RETURN-CODE                                             
026000     GOBACK                                                               
026100     .                                                                    
026200                                                                          
026300     EJECT                                                                
026400 A-INIT SECTION.                                                          
026500                                                                          
026600     OPEN INPUT  W27168                                                   
026700                                                                          
026800     OPEN OUTPUT W271UT-ALL                                               
027200                                                                          
027300     ACCEPT DAGENS-DATUM     FROM DATE                                    
027400     MOVE DAGENS-DATUM-AAR   TO WS-RED-AA                                 
027500     MOVE DAGENS-DATUM-MAANAD                                             
027600                             TO WS-RED-MM                                 
027700     MOVE DAGENS-DATUM-DAG   TO WS-RED-DD                                 
027800     MOVE WS-RED-DATUM       TO UT-REPORT-DATE                            
027900     MOVE ZERO               TO WS-PAGE-ALL                               
028200                                WS-RADANT-ALL                             
028500     MOVE ZERO               TO WS-ANTAL-ARTIKLAR-ALL                     
028800     .                                                                    
028900                                                                          
029000     EJECT                                                                
029100 B-SKAPA-POSTER SECTION.                                                  
029101                                                                          
029200     PERFORM S01-LAES-W27168                                              
029201     MOVE SPACE        TO WS-IDDC                                         
029202                          WS-IDLAND-CURR                                  
029203                                                                          
029400     PERFORM UNTIL END-OF-W27168                                          
029401                                                                          
029410       IF IN-IDDC NOT = WS-IDDC                                           
029420          PERFORM S10-SOK-IDLAND                                          
029430       END-IF                                                             
029440                                                                          
029450       IF IN-IDLANDX2 NOT = WS-IDLAND-CURR                                
029490          MOVE IN-IDLANDX2 TO WS-IDLAND-CURR                              
029491          MOVE IN-IDDC     TO WS-IDDC                                     
029492          PERFORM BA-REPORT-HEADER-ALL                                    
029494                                                                          
029495          MOVE ZERO TO WS-ANTAL-ARTIKLAR-ALL                              
029499       END-IF                                                             
029500                                                                          
029510       MOVE IN-IDPERSON-BUY  TO UT-IDPERSON-BUY                           
029600       MOVE IN-IDARTNR       TO UT-IDARTNR                                
029700       MOVE IN-BEART         TO UT-BEART                                  
029800       MOVE IN-IDFKNGRP      TO UT-IDFKNGRP                               
029900       MOVE IN-FREEZECODE    TO UT-FREEZECODE                             
030000       MOVE IN-ANTAL-ORDRAD  TO UT-ANTAL-ORDRAD                           
030100       MOVE IN-ANTAL-QTY     TO UT-ANTAL-QTY                              
030200       MOVE IN-PRAVCOST      TO UT-PRAVCOST                               
030300       MOVE IN-KVPB-REF      TO UT-KVPB-REF                               
030400       MOVE IN-ONHAND        TO UT-ONHAND                                 
030500       MOVE IN-KVBEART       TO UT-KVBEART                                
030600       MOVE IN-KVAKS-SDC     TO UT-KVAKS-SDC                              
030700       MOVE IN-KDERS         TO UT-KDERS                                  
030800       MOVE IN-AVAIL-CDC     TO UT-AVAIL-CDC                              
030900                                                                          
031300       IF WS-ANTAL-ARTIKLAR-ALL < 200                                     
031400          ADD 1               TO WS-ANTAL-ARTIKLAR-ALL                    
031500          MOVE WS-ANTAL-ARTIKLAR-ALL                                      
031600                              TO UT-RANK                                  
032000          MOVE UT-RAD-1       TO W001-DAP                                 
032100          WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                        
032200       END-IF                                                             
032300       PERFORM S01-LAES-W27168                                            
032400     END-PERFORM                                                          
032500     .                                                                    
032600                                                                          
032700     EJECT                                                                
036700 BA-REPORT-HEADER-ALL SECTION.                                            
036800                                                                          
036900     PERFORM S02-SKRIV-DAP1-ALL                                           
037000     PERFORM S03-SKRIV-DAP2-ALL                                           
037100                                                                          
037200     MOVE 'W27171-ALL'     TO UT-LIST-NAME                                
037400                                                                          
037500     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-1                         
037600     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-2                         
037700     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-3                         
037800     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-4                         
037900     .                                                                    
038000                                                                          
038100     EJECT                                                                
043700 S01-LAES-W27168  SECTION.                                                
043800     READ W27168 INTO IN-AREA                                             
043900     AT END                                                               
044000        SET END-OF-W27168 TO TRUE                                         
044100                                                                          
044200     END-READ                                                             
044300     .                                                                    
044400 S02-SKRIV-DAP1-ALL SECTION.                                              
044500                                                                          
044600     MOVE ' ¤DAPW27168' TO W001-DAP                                       
044700     WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                             
044800                                                                          
044900     MOVE SPACE TO W001-DAP                                               
045000     .                                                                    
045100                                                                          
045200 S03-SKRIV-DAP2-ALL SECTION.                                              
045300                                                                          
045400     STRING ' ¤DAP' WS-IDLAND-CURR                                        
045500            DELIMITED BY SIZE INTO W001-DAP                               
045600     WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                             
045700                                                                          
045800     MOVE SPACE TO W001-DAP                                               
045900     .                                                                    
047500 S10-SOK-IDLAND SECTION.                                                  
047600                                                                          
048500     IF IN-IDLANDX2 = SPACE                                               
048600        MOVE SPACE              TO UT-LIST-COUNTRY                        
048700     ELSE                                                                 
048800        MOVE IN-IDLANDX2        TO LAND-IDLANDX2                          
048900        MOVE SPACE              TO LAND-IDLANDX3                          
049000        CALL WISOLAND USING LAND-WISOLAND                                 
049100        IF LAND-KDSVAR = SPACE                                            
049200           MOVE LAND-BELAND-ENG TO UT-LIST-COUNTRY                        
049300        ELSE                                                              
049400           MOVE SPACE           TO UT-LIST-COUNTRY                        
049500        END-IF                                                            
049600     END-IF                                                               
049700     .                                                                    
049800     EJECT                                                                
049900 Z-FINIT SECTION.                                                         
050000     CLOSE W27168                                                         
050100           W271UT-ALL                                                     
050200     .                                                                    
050300                                                                          

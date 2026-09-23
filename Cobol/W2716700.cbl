000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2716700.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   NOV 1997.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                                                                         
001000*        PROGRAMMET SKRIVER UT EN LISTA ÖVER 'TOPP 200                    
001100*        RESTORDER' FÖR ALLA NDC I ÄLDSTAORDNING                          
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
002500     SELECT W27167                     ASSIGN TO W27167D1.                
002600*          ---                                                            
002700     SELECT W271UT-ALL                 ASSIGN TO W27167D2.                
002800     SKIP2                                                                
003400                                                                          
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000                                                                          
004100 FD  W27167                                                               
004200     RECORDING F                                                          
004300     RECORD CONTAINS 80 CHARACTERS                                        
004400     LABEL RECORD STANDARD                                                
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700 01  IN-POST                 PIC X(80).                                   
004800                                                                          
004900 FD  W271UT-ALL                                                           
005000     RECORDING V                                                          
005100     BLOCK CONTAINS  0 RECORDS.                                           
005200 01  ORDER-REPORT-REC-ALL    PIC X(124).                                  
005300                                                                          
005400                                                                          
006700     EJECT                                                                
006800 WORKING-STORAGE SECTION.                                                 
006900                                                                          
007000*    -- CHECKED BY WY2000                                                 
007100 77  IDPGM                       PIC X(8)    VALUE 'W2716700'.            
007200 77  JA                          PIC X       VALUE 'J'.                   
007300 77  NEJ                         PIC X       VALUE 'N'.                   
007400                                                                          
007500 77  W27167-EOF-SW               PIC X       VALUE 'N'.                   
007600     88  END-OF-W27167                       VALUE 'J'.                   
007610                                                                          
007620 01  SUBPROGRAM.                                                          
007630     03  WISOLAND                PIC X(8)    VALUE 'WISOLAND'.            
007640                                                                          
007650*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
007660*                                                                         
007670 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
007680     SKIP3                                                                
007690*01 -COPY WISOLAND                                                        
007691                                                                          
007692 01  WS-IDDC                 PIC X(2)    VALUE SPACE.                     
007693                                                                          
007694 01  WS-IDLAND-CURR          PIC X(2)    VALUE SPACE.                     
007700                                                                          
007800 01  ARBETSAREOR.                                                         
008500     03 WS-RED-DATUM.                                                     
008600       05  WS-RED-MM             PIC X(2)   VALUE SPACE.                  
008700       05  FILLER                PIC X      VALUE '/'.                    
008800       05  WS-RED-DD             PIC X(2)   VALUE SPACE.                  
008900       05  FILLER                PIC X      VALUE '/'.                    
009000       05  WS-RED-AA             PIC X(2)   VALUE SPACE.                  
009100     03 WS-DARODAT               PIC 9(8)   VALUE ZERO.                   
009200     03 WS-ANTAL-ARTIKLAR-ALL    PIC 9(8)   VALUE ZERO.                   
009500                                                                          
009600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009700 01  FILLER REDEFINES DAGENS-DATUM.                                       
009800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010100                                                                          
010200 01  FELTEXT.                                                             
010300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010500                                                                          
010600     EJECT                                                                
010700 01  W27167-AREA-START           PIC X(24)   VALUE                        
010800                                 'W27167-AREA-START  '.                   
010900                                                                          
011000*01  AREA -COPY W27162     -PRE IN-                                       
011100                                                                          
011200     EJECT                                                                
011300 01  W271UT-AREA-START           PIC X(24)   VALUE                        
011400                                 'W271UT-AREA-START  '.                   
011500                                                                          
011520 01  W001-DAP.                                                            
011530     03  FILLER                  PIC X(165)  VALUE SPACE.                 
011540                                                                          
011600 01  UT-AREOR.                                                            
011700     03 UT-RPT-PRINT-LINES.                                               
011800       05 UT-HEADING-1.                                                   
011900         10 FILLER               PIC X(1)  VALUE SPACE.                   
012000         10 FILLER               PIC X(10) VALUE 'VOLVO CAR'.             
012100         10 FILLER               PIC X(17) VALUE 'AFTERSALES'.            
012200         10 FILLER               PIC X(11) VALUE SPACE.                   
012300         10 UT-LIST-NAME         PIC X(12) VALUE SPACE.                   
012400         10 FILLER               PIC X(12) VALUE SPACE.                   
012500         10 UT-LIST-COUNTRY      PIC X(13) VALUE SPACE.                   
012600         10 FILLER               PIC X(5)  VALUE SPACE.                   
012700         10 FILLER               PIC X(7)  VALUE 'TOP 200'.               
012800         10 FILLER               PIC X(13) VALUE SPACE.                   
012900         10 UT-REPORT-DATE       PIC X(8)  VALUE SPACE.                   
013400       05 UT-HEADING-2.                                                   
013500         10 FILLER               PIC X(39) VALUE SPACE.                   
013600         10 FILLER               PIC X(12) VALUE 'BACKORDERED'.           
013700         10 FILLER               PIC X(9)  VALUE 'ITEMS BY'.              
013800         10 FILLER               PIC X(10) VALUE 'ORDER DATE'.            
013900         10 FILLER               PIC X(10) VALUE SPACE.                   
014000         10 UT-BUYER             PIC X(10) VALUE SPACE.                   
014100       05 UT-HEADING-3.                                                   
014200         10 FILLER               PIC X(5)  VALUE SPACE.                   
014300         10 FILLER               PIC X(5)  VALUE 'BUYER'.                 
014400         10 FILLER               PIC X(7)  VALUE SPACE.                   
014500         10 FILLER               PIC X(4)  VALUE 'PART'.                  
014600         10 FILLER               PIC X(24) VALUE SPACE.                   
014700         10 FILLER               PIC X(4)  VALUE 'FUNC'.                  
014800         10 FILLER               PIC X(1)  VALUE SPACE.                   
014900         10 FILLER               PIC X(6)   VALUE 'FREEZE'.               
015000         10 FILLER               PIC X(1)  VALUE SPACE.                   
015100         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
015200         10 FILLER               PIC X(9)  VALUE SPACE.                   
015300         10 FILLER               PIC X(7)  VALUE 'AVERAGE'.               
015400         10 FILLER               PIC X(6)  VALUE SPACE.                   
015500         10 FILLER               PIC X(4)  VALUE 'FORE'.                  
015600         10 FILLER               PIC X(5)  VALUE SPACE.                   
015700         10 FILLER               PIC X(2)  VALUE 'ON'.                    
015800         10 FILLER               PIC X(5)  VALUE SPACE.                   
015900         10 FILLER               PIC X(2)  VALUE 'ON'.                    
016000         10 FILLER               PIC X(4)  VALUE SPACE.                   
016100         10 FILLER               PIC X(4)  VALUE 'BACK'.                  
016200         10 FILLER               PIC X(6)  VALUE SPACE.                   
016300         10 FILLER               PIC X(3)  VALUE 'BAL'.                   
016400       05 UT-HEADING-4.                                                   
016500         10 FILLER               PIC X(1)  VALUE SPACE.                   
016600         10 FILLER               PIC X(4)  VALUE 'RANK'.                  
016700         10 FILLER               PIC X(1)  VALUE SPACE.                   
016800         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
016900         10 FILLER               PIC X(1)  VALUE SPACE.                   
017000         10 FILLER               PIC X(2)  VALUE 'DC'.                    
017100         10 FILLER               PIC X(3)  VALUE SPACE.                   
017200         10 FILLER               PIC X(6)  VALUE 'NUMBER'.                
017300         10 FILLER               PIC X(1)  VALUE SPACE.                   
017400         10 FILLER               PIC X(11) VALUE 'DESCRIPTION'.           
017500         10 FILLER               PIC X(12) VALUE SPACE.                   
017600         10 FILLER               PIC X(3)  VALUE 'GRP'.                   
017700         10 FILLER               PIC X(2)  VALUE SPACE.                   
017800         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
017900         10 FILLER               PIC X(2)  VALUE SPACE.                   
018000         10 FILLER               PIC X(5)  VALUE 'LINES'.                 
018100         10 FILLER               PIC X(2)  VALUE SPACE.                   
018200         10 FILLER               PIC X(3)  VALUE 'QTY'.                   
018300         10 FILLER               PIC X(6)  VALUE SPACE.                   
018400         10 FILLER               PIC X(4)  VALUE 'COST'.                  
018500         10 FILLER               PIC X(7)  VALUE SPACE.                   
018600         10 FILLER               PIC X(4)  VALUE 'CAST'.                  
018700         10 FILLER               PIC X(4)  VALUE SPACE.                   
018800         10 FILLER               PIC X(4)  VALUE 'HAND'.                  
018900         10 FILLER               PIC X(2)  VALUE SPACE.                   
019000         10 FILLER               PIC X(5)  VALUE 'ORDER'.                 
019100         10 FILLER               PIC X(1)  VALUE SPACE.                   
019200         10 FILLER               PIC X(7)  VALUE 'ORDERED'.               
019300         10 FILLER               PIC X(5)  VALUE SPACE.                   
019400         10 FILLER               PIC X(3)  VALUE 'CDC'.                   
019500       05 UT-RAD-1.                                                       
019600         10 FILLER               PIC X     VALUE SPACE.                   
019700         10 UT-RANK              PIC Z(3)9 VALUE ZERO.                    
019800         10 FILLER               PIC X(1)  VALUE SPACE.                   
019900         10 UT-IDPERSON-BUY      PIC ZZ9   VALUE ZERO.                    
020000         10 FILLER               PIC X(2)  VALUE SPACE.                   
020100         10 UT-IDDC              PIC X(2)  VALUE SPACE.                   
020200         10 UT-IDARTNR           PIC Z(8)9 VALUE ZERO.                    
020300         10 FILLER               PIC X(1)  VALUE SPACE.                   
020400         10 UT-BEART             PIC X(19) VALUE SPACE.                   
020500         10 FILLER               PIC X(2)  VALUE SPACE.                   
020600         10 UT-IDFKNGRP          PIC Z(4)9 VALUE ZERO.                    
020700         10 FILLER               PIC X(3)  VALUE SPACE.                   
020800         10 UT-FREEZECODE        PIC X     VALUE SPACE.                   
020900         10 FILLER               PIC X(1)  VALUE SPACE.                   
021000         10 UT-ANTAL-ORDRAD      PIC Z(5)9 VALUE ZERO.                    
021100         10 FILLER               PIC X(1)  VALUE SPACE.                   
021200         10 UT-ANTAL-QTY         PIC Z(5)9 VALUE ZERO.                    
021300         10 FILLER               PIC X(1)  VALUE SPACE.                   
021400         10 UT-PRAVCOST          PIC Z(6)9.9(2)                           
021500                                           VALUE ZERO.                    
021600         10 FILLER               PIC X(1)  VALUE SPACE.                   
021700         10 UT-KVPB-REF          PIC Z(5)9.9(2)                           
021800                                           VALUE ZERO.                    
021900         10 FILLER               PIC X(1)  VALUE SPACE.                   
022000         10 UT-ONHAND            PIC Z(6)9 VALUE ZERO.                    
022100         10 FILLER               PIC X(1)  VALUE SPACE.                   
022200         10 UT-KVBEART           PIC Z(5)9 VALUE ZERO.                    
022300         10 FILLER               PIC X(2)  VALUE SPACE.                   
022400         10 UT-TIRODAT           PIC 9(6)  VALUE ZERO.                    
022500         10 FILLER               PIC X(1)  VALUE SPACE.                   
022600         10 UT-AVAIL-CDC         PIC Z(6)9 VALUE ZERO.                    
022700                                                                          
022800     EJECT                                                                
022900 PROCEDURE DIVISION.                                                      
023000 MAIN SECTION.                                                            
023100                                                                          
023200     PERFORM A-INIT                                                       
023300     PERFORM B-SKAPA-POSTER                                               
023400     PERFORM Z-FINIT                                                      
023500                                                                          
023600     MOVE ZERO TO RETURN-CODE                                             
023700     GOBACK                                                               
023800     .                                                                    
023900                                                                          
024000     EJECT                                                                
024100 A-INIT SECTION.                                                          
024200                                                                          
024300     OPEN INPUT  W27167                                                   
024400                                                                          
024500     OPEN OUTPUT W271UT-ALL                                               
024800                                                                          
024900     ACCEPT DAGENS-DATUM     FROM DATE                                    
025000     MOVE DAGENS-DATUM-AAR   TO WS-RED-AA                                 
025100     MOVE DAGENS-DATUM-MAANAD                                             
025200                             TO WS-RED-MM                                 
025300     MOVE DAGENS-DATUM-DAG   TO WS-RED-DD                                 
025400     MOVE WS-RED-DATUM       TO UT-REPORT-DATE                            
026100     MOVE ZERO               TO WS-ANTAL-ARTIKLAR-ALL                     
026400     .                                                                    
026500                                                                          
026600     EJECT                                                                
026700 B-SKAPA-POSTER SECTION.                                                  
026800     PERFORM S01-LAES-W27167                                              
026900     MOVE SPACE        TO WS-IDDC                                         
026910                          WS-IDLAND-CURR                                  
026920                                                                          
027000                                                                          
027100     PERFORM UNTIL END-OF-W27167                                          
027110        IF IN-IDDC NOT = WS-IDDC                                          
027120           PERFORM S10-SOK-IDLAND                                         
027130        END-IF                                                            
027140                                                                          
027150        IF IN-IDLANDX2 NOT = WS-IDLAND-CURR                               
027190           MOVE IN-IDLANDX2 TO WS-IDLAND-CURR                             
027191           MOVE IN-IDDC     TO WS-IDDC                                    
027192           PERFORM BA-REPORT-HEADER-ALL                                   
027195                                                                          
027196           MOVE ZERO TO WS-ANTAL-ARTIKLAR-ALL                             
027202        END-IF                                                            
027203                                                                          
027210        MOVE IN-IDPERSON-BUY   TO UT-IDPERSON-BUY                         
027300        MOVE IN-IDDC           TO UT-IDDC                                 
027400        MOVE IN-IDARTNR        TO UT-IDARTNR                              
027500        MOVE IN-BEART          TO UT-BEART                                
027600        MOVE IN-IDFKNGRP       TO UT-IDFKNGRP                             
027700        MOVE IN-FREEZECODE     TO UT-FREEZECODE                           
027800        MOVE IN-ANTAL-ORDRAD   TO UT-ANTAL-ORDRAD                         
027900        MOVE IN-ANTAL-QTY      TO UT-ANTAL-QTY                            
028000        MOVE IN-PRAVCOST       TO UT-PRAVCOST                             
028100        MOVE IN-KVPB-REF       TO UT-KVPB-REF                             
028200        MOVE IN-ONHAND         TO UT-ONHAND                               
028300        MOVE IN-KVBEART        TO UT-KVBEART                              
028400        MOVE IN-DARODAT        TO WS-DARODAT                              
028500        MOVE WS-DARODAT (3:6)  TO UT-TIRODAT                              
028600        MOVE IN-AVAIL-CDC      TO UT-AVAIL-CDC                            
028700        IF WS-ANTAL-ARTIKLAR-ALL < 200                                    
028930           ADD 1               TO WS-ANTAL-ARTIKLAR-ALL                   
028940           MOVE WS-ANTAL-ARTIKLAR-ALL                                     
028950                               TO UT-RANK                                 
028990           MOVE UT-RAD-1       TO W001-DAP                                
028991           WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                       
029000        END-IF                                                            
030100        PERFORM S01-LAES-W27167                                           
030200     END-PERFORM                                                          
030300     .                                                                    
030400                                                                          
030500     EJECT                                                                
030510 BA-REPORT-HEADER-ALL SECTION.                                            
030520                                                                          
030530     PERFORM S02-SKRIV-DAP1-ALL                                           
030540     PERFORM S03-SKRIV-DAP2-ALL                                           
030550                                                                          
030560     MOVE 'W27167-ALL'     TO UT-LIST-NAME                                
030570     MOVE 'ALL BUYERS'     TO UT-BUYER                                    
030580                                                                          
030590     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-1                         
030591     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-2                         
030592     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-3                         
030593     WRITE ORDER-REPORT-REC-ALL FROM UT-HEADING-4                         
030594     .                                                                    
030595                                                                          
030596     EJECT                                                                
039100 Z-FINIT SECTION.                                                         
039200     CLOSE W27167                                                         
039300           W271UT-ALL                                                     
039600     .                                                                    
039700                                                                          
039800     EJECT                                                                
039900 S01-LAES-W27167  SECTION.                                                
040000     READ W27167 INTO IN-AREA                                             
040100     AT END                                                               
040200        SET END-OF-W27167 TO TRUE                                         
040300                                                                          
040400     END-READ                                                             
040500     .                                                                    
040600 S02-SKRIV-DAP1-ALL SECTION.                                              
040700                                                                          
040800     MOVE ' ¤DAPW27167' TO W001-DAP                                       
040900     WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                             
041000                                                                          
041100     MOVE SPACE TO W001-DAP                                               
041200     .                                                                    
041300                                                                          
041400 S03-SKRIV-DAP2-ALL SECTION.                                              
041500                                                                          
041600     STRING ' ¤DAP' WS-IDLAND-CURR                                        
041700            DELIMITED BY SIZE INTO W001-DAP                               
041800     WRITE ORDER-REPORT-REC-ALL FROM W001-DAP                             
041900                                                                          
042000     MOVE SPACE TO W001-DAP                                               
042100     .                                                                    
043700 S10-SOK-IDLAND SECTION.                                                  
043800                                                                          
044700     IF IN-IDLANDX2 = SPACE                                               
044800        MOVE SPACE              TO UT-LIST-COUNTRY                        
044900     ELSE                                                                 
045000        MOVE IN-IDLANDX2        TO LAND-IDLANDX2                          
045100        MOVE SPACE              TO LAND-IDLANDX3                          
045200        CALL WISOLAND USING LAND-WISOLAND                                 
045300        IF LAND-KDSVAR = SPACE                                            
045400           MOVE LAND-BELAND-ENG TO UT-LIST-COUNTRY                        
045500        ELSE                                                              
045600           MOVE SPACE           TO UT-LIST-COUNTRY                        
045700        END-IF                                                            
045800     END-IF                                                               
045900     .                                                                    

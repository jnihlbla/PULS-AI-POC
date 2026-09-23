000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W5128700.                                                 
000300                                                                          
000400*    AUTHOR.    BARSHARANI BISHOYE.                                       
000500*    DATE:      NOVEMBER 2019                                             
000510*                                                                         
000600*    FUNKTION:                                                            
000700*               SKAPAR W51287-XX  FÖR KURANSGRUPPER                       
000800*               TURNOVERVALUE PER PRODUCTGROUP                            
000900*               TURNOVERVALUE PER PRODUCTGROUP AND TURNOVERGROUP          
001000*               SUMMA FÖR LÄNDER MED FLERA LAGER I LANDET                 
001100*                                                                         
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600                                                                          
001700     SELECT W51284  ASSIGN       TO W51287D1.                             
001800                                                                          
001900     SELECT W51287S ASSIGN       TO W51287D2.                             
002000     SELECT W51287K ASSIGN       TO W51287D3.                             
002100     EJECT                                                                
002200                                                                          
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W51284                                                               
002700     RECORDING F                                                          
002800     BLOCK CONTAINS 0.                                                    
002900                                                                          
003000*01  INTRANS     -COPY W51284    -L                                       
003100     SKIP2                                                                
003200                                                                          
003300 FD  W51287S                                                              
003400     RECORDING V                                                          
003500     BLOCK CONTAINS 0.                                                    
003600                                                                          
003700 01  W51287-S-REC            PIC X(121).                                  
003800     SKIP2                                                                
003900                                                                          
004000 FD  W51287K                                                              
004100     RECORDING V                                                          
004200     BLOCK CONTAINS 0.                                                    
004300                                                                          
004400 01  W51287-K-REC            PIC X(121).                                  
004500     EJECT                                                                
004600                                                                          
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP3                                                                
004900 77  IDPGM                   PIC X(8)      VALUE 'W5128700'.              
005000 77  JA                      PIC X         VALUE 'J'.                     
005100 77  NEJ                     PIC X         VALUE 'N'.                     
005200 77  EOF-W51284              PIC X         VALUE 'N'.                     
005300                                                                          
005400*   -COPY WWDC99                                                          
005500                                                                          
005600 01  WS-KDPSLLOC             PIC S9(2)     VALUE +0  COMP-3.              
005700 01  TOT-INKRES              PIC S9(11)V99 VALUE +0  COMP-3.              
005800 01  RAD-INKRES              PIC S9(11)V99 VALUE +0  COMP-3.              
005900                                                                          
006000 01  SUBPROGRAM.                                                          
006100     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
006200     03  WISOLAND            PIC X(8)    VALUE 'WISOLAND'.                
006300                                                                          
006400 01  W51284-TRANSID.                                                      
006500     03  FILLER              PIC X(6) VALUE 'W51284'.                     
006600     03  FILLER              PIC X(8) VALUE 'W51287D1'.                   
006700     03  FILLER              PIC X(4) VALUE '4311'.                       
006800                                                                          
006900     EJECT                                                                
007000*   -COPY W0005  -PRE POSTSUM-                                            
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
007300*                                                                         
007400 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
007500     SKIP3                                                                
007600*01 -COPY WISOLAND                                                        
007700                                                                          
007800 01  W-IDLAND-CURR           PIC X(2)    VALUE SPACE.                     
007900 01  W-IDLAND-X.                                                          
008000     03  W-IDLAND            PIC X(2)    VALUE SPACE.                     
008100*01  -COPY WWDCLAND                                                       
008200                                                                          
008300 01  FILLER                  PIC X(16)   VALUE 'W51284-AREA'.             
008400 01  INAREA.                                                              
008500*    03  -COPY W51284     -PRE IN-                                        
008600     EJECT                                                                
008700                                                                          
008800 01  W001-DAP.                                                            
008900     03  FILLER                  PIC X(165)  VALUE SPACE.                 
009000                                                                          
009100*FOR SHELFLIFE                                                            
009200 01  TEXT-AREA.                                                           
009300     03  MAIN-LINE1.                                                      
009400         05  FILLER          PIC X(7)    VALUE                            
009500             'W51287-'.                                                   
009600         05  RUB1-IDLAND     PIC XX.                                      
009700         05  FILLER          PIC X(1)    VALUE '	'.                       
009800         05  FILLER          PIC X(17)   VALUE                            
009900             'STOCK TURNOVER BY'.                                         
010000         05  FILLER          PIC X(1)    VALUE '	'.                       
010100         05  FILLER          PIC X(16)   VALUE                            
010200             'SHELF-LIFE METOD'.                                          
010300         05  FILLER          PIC X(1)    VALUE '	'.                       
010400         05  FILLER          PIC X(5)    VALUE 'DATE '.                   
010500         05  RUB1-YYYY       PIC 9999.                                    
010600         05  FILLER          PIC X(1)    VALUE '-'.                       
010700         05  RUB1-MM         PIC 99.                                      
010800         05  FILLER          PIC X(1)    VALUE '-'.                       
010900         05  RUB1-DD         PIC 99.                                      
011000         05  FILLER          PIC X(1)    VALUE '	'.                       
011100         05  FILLER          PIC X(9)    VALUE 'CURRENCY '.               
011200         05  RUB1-KDVALISO   PIC X(3).                                    
011300         05  FILLER          PIC X(1)    VALUE '	'.                       
011400         05  FILLER          PIC X(1)    VALUE '	'.                       
011500                                                                          
011600     03  FILLER-LINE.                                                     
011700         05  FILLER          PIC X(1)    VALUE '	'.                       
011800         05  FILLER          PIC X(1)    VALUE '	'.                       
011900         05  FILLER          PIC X(1)    VALUE '	'.                       
012000         05  FILLER          PIC X(1)    VALUE '	'.                       
012100         05  FILLER          PIC X(1)    VALUE '	'.                       
012200         05  FILLER          PIC X(1)    VALUE '	'.                       
012300                                                                          
012400     03  ROW-LINE1.                                                       
012500         05  FILLER          PIC X(14)   VALUE                            
012600            'TURNOVER GROUP'.                                             
012700         05  FILLER          PIC X(1)    VALUE '	'.                       
012800         05  FILLER          PIC X(13)   VALUE                            
012900            'PRODUCT GROUP'.                                              
013000         05  FILLER          PIC X(1)    VALUE '	'.                       
013100         05  FILLER          PIC X(11)   VALUE                            
013200            'STOCKVALUE'.                                                 
013300         05  FILLER          PIC X(1)    VALUE '	'.                       
013400         05  FILLER          PIC X(24)   VALUE                            
013500            '% OF STOCKVALUE IN P.GR'.                                    
013600         05  FILLER          PIC X(1)    VALUE '	'.                       
013700         05  FILLER          PIC X(12)   VALUE                            
013800            'OBSOLESCENCE'.                                               
013900         05  FILLER          PIC X(1)    VALUE '	'.                       
014000         05  FILLER          PIC X(1)    VALUE '	'.                       
014100                                                                          
014200     03  ROW.                                                             
014300         05  ROW-KGR         PIC 9.                                       
014400         05  FILLER          PIC X(1)    VALUE '	'.                       
014500         05  ROW-PKOD        PIC 99.                                      
014600         05  FILLER          PIC X(1)    VALUE '	'.                       
014700         05  ROW-LVALUE      PIC Z(9)9.99-.                               
014800         05  FILLER          PIC X(1)    VALUE '	'.                       
014900         05  ROW-PROC        PIC Z(2)9.9-.                                
015000         05  FILLER          PIC X(1)    VALUE '	'.                       
015100         05  ROW-INKRES      PIC Z(9)9.99-.                               
015200         05  FILLER          PIC X(1)    VALUE '	'.                       
015300         05  FILLER          PIC X(1)    VALUE '	'.                       
015400                                                                          
015500     03  ROW-TOTAL.                                                       
015600         05  FILLER          PIC X(5)    VALUE                            
015700            'TOTAL'.                                                      
015800         05  FILLER          PIC X(1)    VALUE '	'.                       
015900         05  ROWT-PKOD       PIC 99.                                      
016000         05  FILLER          PIC X(1)    VALUE '	'.                       
016100         05  ROWT-LVALUE     PIC Z(9)9.99-.                               
016200         05  FILLER          PIC X(1)    VALUE '	'.                       
016300         05  ROWT-PROC       PIC Z(2)9.9-.                                
016400         05  FILLER          PIC X(1)    VALUE '	'.                       
016500         05  ROWT-INKRES     PIC Z(9)9.99-.                               
016600         05  FILLER          PIC X(1)    VALUE '	'.                       
016700         05  FILLER          PIC X(1)    VALUE '	'.                       
016800                                                                          
016900     03  ROW-SUPERTOTAL.                                                  
017000         05  FILLER          PIC X(17)   VALUE                            
017100            'TOTAL IN TURNOVER'.                                          
017200         05  FILLER          PIC X(1)    VALUE '	'.                       
017300         05  ROWS-LVALUE     PIC Z(9)9.99-.                               
017400         05  FILLER          PIC X(1)    VALUE '	'.                       
017500         05  FILLER          PIC X(1)    VALUE '	'.                       
017600         05  FILLER          PIC X(1)    VALUE '	'.                       
017700         05  FILLER          PIC X(1)    VALUE '	'.                       
017800         05  FILLER          PIC X(1)    VALUE '	'.                       
017900     EJECT                                                                
018000                                                                          
018100*FOR KGR                                                                  
018200 01  TEXT-AREA2.                                                          
018300     03  ROW-LINE2.                                                       
018400         05  FILLER          PIC X(13)   VALUE                            
018500            'PRODUCT GROUP'.                                              
018600         05  FILLER          PIC X(1)    VALUE '	'.                       
018700         05  FILLER          PIC X(17)   VALUE                            
018800            'STOCKVALUE T.GR 1'.                                          
018900         05  FILLER          PIC X(1)    VALUE '	'.                       
019000         05  FILLER          PIC X(17)   VALUE                            
019100            'STOCKVALUE T.GR 2'.                                          
019200         05  FILLER          PIC X(1)    VALUE '	'.                       
019300         05  FILLER          PIC X(17)   VALUE                            
019400            'STOCKVALUE T.GR 3'.                                          
019500         05  FILLER          PIC X(1)    VALUE '	'.                       
019600         05  FILLER          PIC X(17)   VALUE                            
019700            'STOCKVALUE T.GR 4'.                                          
019800         05  FILLER          PIC X(1)    VALUE '	'.                       
019900         05  FILLER          PIC X(17)   VALUE                            
020000            'STOCKVALUE T.GR 5'.                                          
020100         05  FILLER          PIC X(1)    VALUE '	'.                       
020200         05  FILLER          PIC X(17)   VALUE                            
020300            'STOCKVALUE T.GR 6'.                                          
020400                                                                          
020500     03  ROW2.                                                            
020600         05  ROW2-PKOD       PIC 99.                                      
020700         05  FILLER          PIC X(1)    VALUE '	'.                       
020800         05  ROW2-LVALUE1    PIC Z(9)9.99-.                               
020900         05  FILLER          PIC X(1)    VALUE '	'.                       
021000         05  ROW2-LVALUE2    PIC Z(9)9.99-.                               
021100         05  FILLER          PIC X(1)    VALUE '	'.                       
021200         05  ROW2-LVALUE3    PIC Z(9)9.99-.                               
021300         05  FILLER          PIC X(1)    VALUE '	'.                       
021400         05  ROW2-LVALUE4    PIC Z(9)9.99-.                               
021500         05  FILLER          PIC X(1)    VALUE '	'.                       
021600         05  ROW2-LVALUE5    PIC Z(9)9.99-.                               
021700         05  FILLER          PIC X(1)    VALUE '	'.                       
021800         05  ROW2-LVALUE6    PIC Z(9)9.99-.                               
021900                                                                          
022000     03  ROW2-TOTAL.                                                      
022100         05  FILLER          PIC X(5)    VALUE                            
022200            'TOTAL'.                                                      
022300         05  FILLER          PIC X(1)    VALUE '	'.                       
022400         05  ROW2T-LVALUE1   PIC Z(9)9.99-.                               
022500         05  FILLER          PIC X(1)    VALUE '	'.                       
022600         05  ROW2T-LVALUE2   PIC Z(9)9.99-.                               
022700         05  FILLER          PIC X(1)    VALUE '	'.                       
022800         05  ROW2T-LVALUE3   PIC Z(9)9.99-.                               
022900         05  FILLER          PIC X(1)    VALUE '	'.                       
023000         05  ROW2T-LVALUE4   PIC Z(9)9.99-.                               
023100         05  FILLER          PIC X(1)    VALUE '	'.                       
023200         05  ROW2T-LVALUE5   PIC Z(9)9.99-.                               
023300         05  FILLER          PIC X(1)    VALUE '	'.                       
023400         05  ROW2T-LVALUE6   PIC Z(9)9.99-.                               
023500                                                                          
023600     03  ROW-OBSOL.                                                       
023700         05  FILLER          PIC X(12)   VALUE                            
023800            'OBSOLESCENCE'.                                               
023900         05  FILLER          PIC X(1)    VALUE '	'.                       
024000         05  ROW3T-LVALUE1   PIC Z(9)9.99-.                               
024100         05  FILLER          PIC X(1)    VALUE '	'.                       
024200         05  ROW3T-LVALUE2   PIC Z(9)9.99-.                               
024300         05  FILLER          PIC X(1)    VALUE '	'.                       
024400         05  ROW3T-LVALUE3   PIC Z(9)9.99-.                               
024500         05  FILLER          PIC X(1)    VALUE '	'.                       
024600         05  ROW3T-LVALUE4   PIC Z(9)9.99-.                               
024700         05  FILLER          PIC X(1)    VALUE '	'.                       
024800         05  ROW3T-LVALUE5   PIC Z(9)9.99-.                               
024900         05  FILLER          PIC X(1)    VALUE '	'.                       
025000         05  ROW3T-LVALUE6   PIC Z(9)9.99-.                               
025100                                                                          
025200     03  ROWT-OBSOL.                                                      
025300         05  FILLER          PIC X(9)   VALUE                             
025400            'TOTAL OBS'.                                                  
025500         05  FILLER          PIC X(1)    VALUE '	'.                       
025600         05  ROW4T-LVALUE1   PIC Z(9)9.99-.                               
025700         05  FILLER          PIC X(1)    VALUE '	'.                       
025800         05  ROW4T-PROC      PIC Z(2)9.9-.                                
025900         05  FILLER          PIC X(1)    VALUE '	'.                       
026000         05  FILLER          PIC X(10)   VALUE                            
026100            '% OF TOTAL'.                                                 
026200         05  FILLER          PIC X(1)    VALUE '	'.                       
026300         05  FILLER          PIC X(1)    VALUE '	'.                       
026400         05  FILLER          PIC X(1)    VALUE '	'.                       
026500     EJECT                                                                
026600                                                                          
026700 01  FILLER                  PIC X(16)   VALUE 'W-KURTAB-S '.             
026800 01  W-KURTAB-S.                                                          
026900     03  W-KUR1-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
027000     03  W-KUR2-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
027100     03  W-KUR3-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
027200     03  W-KUR4-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
027300     03  W-KUR5-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
027400     03  W-KUR6-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
027500     03  W-KURSUM-S          PIC S9(11)V99  COMP-3 VALUE ZERO.            
027600     03  W-KURSUM2-S         PIC S9(11)V99  COMP-3 VALUE ZERO.            
027700                                                                          
027800 01  FILLER                  PIC X(16)   VALUE 'W-KURTAB   '.             
027900 01  W-KURTAB.                                                            
028000     03  W-KUR1              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028100     03  W-KUR2              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028200     03  W-KUR3              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028300     03  W-KUR4              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028400     03  W-KUR5              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028500     03  W-KUR6              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028600     03  W-KURSUM            PIC S9(11)V99  COMP-3 VALUE ZERO.            
028700     03  W-KURSUM2           PIC S9(11)V99  COMP-3 VALUE ZERO.            
028800                                                                          
028900 01  FILLER                  PIC X(16)   VALUE 'WS-KURTAB  '.             
029000 01  WS-KURTAB.                                                           
029100     03  WS-KUR1             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029200     03  WS-KUR2             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029300     03  WS-KUR3             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029400     03  WS-KUR4             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029500     03  WS-KUR5             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029600     03  WS-KUR6             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029700     03  WS-KURSUM           PIC S9(11)V99  COMP-3 VALUE ZERO.            
029800                                                                          
029900 01  FILLER                  PIC X(16)   VALUE 'WO-KURTAB  '.             
030000 01  WO-KURTAB.                                                           
030100     03  WO-KUR1             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030200     03  WO-KUR2             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030300     03  WO-KUR3             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030400     03  WO-KUR4             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030500     03  WO-KUR5             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030600     03  WO-KUR6             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030700     03  WO-KURSUM           PIC S9(11)V99  COMP-3 VALUE ZERO.            
030800                                                                          
030900 PROCEDURE DIVISION.                                                      
031000                                                                          
031100 MAIN SECTION.                                                            
031200                                                                          
031300     PERFORM A-INIT                                                       
031400                                                                          
031500     PERFORM S01-READ-W51284-POST                                         
031600     IF EOF-W51284 = JA                                                   
031700        CONTINUE                                                          
031800     ELSE                                                                 
031900        PERFORM B-CREATE-SH-O-KGR-POST                                    
032000     END-IF                                                               
032100                                                                          
032200     PERFORM Z-END                                                        
032300                                                                          
032400     MOVE ZERO TO RETURN-CODE                                             
032500     GOBACK                                                               
032600     .                                                                    
032700     EJECT                                                                
032800                                                                          
032900 A-INIT SECTION.                                                          
033000     OPEN INPUT  W51284                                                   
033100          OUTPUT W51287S W51287K                                          
033200                                                                          
033300     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
033400                                                                          
033500     MOVE FUNCTION CURRENT-DATE(1:4) TO RUB1-YYYY                         
033600     MOVE FUNCTION CURRENT-DATE(5:2) TO RUB1-MM                           
033700     MOVE FUNCTION CURRENT-DATE(7:2) TO RUB1-DD                           
033800     .                                                                    
033900     EJECT                                                                
034000                                                                          
034100 B-CREATE-SH-O-KGR-POST SECTION.                                          
034200                                                                          
034300     MOVE SPACE        TO WS-IDDC                                         
034400                          W-IDLAND-CURR                                   
034500     MOVE ZERO         TO WS-KDPSLLOC                                     
034600                                                                          
034700     PERFORM UNTIL EOF-W51284 = JA                                        
034800        IF IN-IDDC NOT = WS-IDDC                                          
034900           PERFORM S10-SOK-IDLAND                                         
035000        END-IF                                                            
035100        IF W-IDLAND NOT = W-IDLAND-CURR                                   
035200                                                                          
035300          IF WS-IDDC NOT = SPACE                                          
035400             PERFORM BF-WRITE-KGR-POST                                    
035500             PERFORM BG-WRITE-TOTAL                                       
035600             MOVE W-KURSUM2-S TO ROWS-LVALUE                              
035700             MOVE W-KURSUM2-S TO WS-KURSUM                                
035800             PERFORM BH-COMPUTE-OBSOLITE                                  
035900             PERFORM BI-WRITE-SUPERTOTAL                                  
036000                                                                          
036100             PERFORM BC-WRITE-SHELFLIFE-POST                              
036200                                                                          
036300             MOVE W-KURSUM2-S TO ROWS-LVALUE                              
036400             MOVE W-KURSUM2-S TO WS-KURSUM                                
036500                                                                          
036600             WRITE W51287-S-REC FROM ROW-SUPERTOTAL                       
036700             WRITE W51287-S-REC FROM FILLER-LINE                          
036800             MOVE ZERO TO W-KURSUM2-S                                     
036900                          WS-KURSUM                                       
037000                          WS-KUR1                                         
037100                          WS-KUR2                                         
037200                          WS-KUR3                                         
037300                          WS-KUR4                                         
037400                          WS-KUR5                                         
037500                          WS-KUR6                                         
037600          END-IF                                                          
037700          MOVE W-IDLAND    TO W-IDLAND-CURR                               
037800          MOVE IN-IDDC     TO WS-IDDC                                     
037900          MOVE IN-KDPSLLOC TO WS-KDPSLLOC                                 
038000          PERFORM S02-SKRIV-DAP1-S                                        
038100          PERFORM S03-SKRIV-DAP2-S                                        
038200          PERFORM BA-HEADER-SHELFLIFE                                     
038300          PERFORM S04-SKRIV-DAP1-K                                        
038400          PERFORM S05-SKRIV-DAP2-K                                        
038500          PERFORM BD-HEADER-KGR                                           
038600       END-IF                                                             
038700       IF WS-KDPSLLOC = IN-KDPSLLOC                                       
038800         PERFORM BB-COMPUTE-STOCKVALUE                                    
038900         PERFORM BE-COMPUTE-STOCKVALUE                                    
039000       ELSE                                                               
039100         IF WS-KDPSLLOC > ZERO                                            
039200            PERFORM BC-WRITE-SHELFLIFE-POST                               
039300         END-IF                                                           
039400         PERFORM BB-COMPUTE-STOCKVALUE                                    
039500         PERFORM BF-WRITE-KGR-POST                                        
039600         MOVE IN-KDPSLLOC  TO WS-KDPSLLOC                                 
039700         PERFORM BE-COMPUTE-STOCKVALUE                                    
039800       END-IF                                                             
039900       PERFORM S01-READ-W51284-POST                                       
040000     END-PERFORM                                                          
040100                                                                          
040200     PERFORM BC-WRITE-SHELFLIFE-POST                                      
040300                                                                          
040400     MOVE W-KURSUM2-S TO ROWS-LVALUE                                      
040500     MOVE W-KURSUM2-S TO WS-KURSUM                                        
040600                                                                          
040700     WRITE W51287-S-REC FROM ROW-SUPERTOTAL                               
040800     WRITE W51287-S-REC FROM FILLER-LINE                                  
040900                                                                          
041000     PERFORM BF-WRITE-KGR-POST                                            
041100     PERFORM BG-WRITE-TOTAL                                               
041200     PERFORM BH-COMPUTE-OBSOLITE                                          
041300     PERFORM BI-WRITE-SUPERTOTAL                                          
041400     .                                                                    
041500     EJECT                                                                
041600                                                                          
041700 BA-HEADER-SHELFLIFE SECTION.                                             
041800                                                                          
041900     MOVE W-IDLAND-CURR TO RUB1-IDLAND                                    
042000                           LAND-IDLANDX2                                  
042100     MOVE SPACE         TO LAND-IDLANDX3                                  
042200     CALL WISOLAND USING LAND-WISOLAND                                    
042300     IF LAND-KDSVAR = SPACE                                               
042400        MOVE LAND-KDVALISO(1)  TO RUB1-KDVALISO                           
042500     ELSE                                                                 
042600        MOVE SPACE             TO RUB1-KDVALISO                           
042700     END-IF                                                               
042800                                                                          
042900     WRITE W51287-S-REC  FROM MAIN-LINE1                                  
043000     WRITE W51287-S-REC  FROM FILLER-LINE                                 
043100     WRITE W51287-S-REC  FROM ROW-LINE1                                   
043200     WRITE W51287-S-REC  FROM FILLER-LINE                                 
043300     .                                                                    
043400                                                                          
043500                                                                          
043600 BB-COMPUTE-STOCKVALUE SECTION.                                           
043700     COMPUTE W-KUR1-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS1) +        
043800                                 W-KUR1-S                                 
043900     COMPUTE W-KUR2-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS2) +        
044000                                 W-KUR2-S                                 
044100     COMPUTE W-KUR3-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS3) +        
044200                                 W-KUR3-S                                 
044300     COMPUTE W-KUR4-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS4) +        
044400                                 W-KUR4-S                                 
044500     COMPUTE W-KUR5-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS5) +        
044600                                 W-KUR5-S                                 
044700     COMPUTE W-KUR6-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS6) +        
044800                                 W-KUR6-S                                 
044900     COMPUTE W-KURSUM-S = W-KUR1-S + W-KUR2-S + W-KUR3-S                  
045000                        + W-KUR4-S + W-KUR5-S + W-KUR6-S                  
045100     .                                                                    
045200     EJECT                                                                
045300                                                                          
045400 BC-WRITE-SHELFLIFE-POST SECTION.                                         
045500     MOVE ZERO TO TOT-INKRES                                              
045600*FOR KURANS GRUPP 1                                                       
045700     MOVE 1           TO ROW-KGR                                          
045800     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
045900     MOVE W-KUR1-S TO ROW-LVALUE                                          
046000     IF W-KURSUM-S = ZERO                                                 
046100       MOVE ZERO TO ROW-PROC                                              
046200       MOVE ZERO TO ROW-INKRES                                            
046300     ELSE                                                                 
046400       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR1-S / W-KURSUM-S             
046500       COMPUTE ROW-INKRES ROUNDED = W-KUR1-S * 0 / 100                    
046600     END-IF                                                               
046700     MOVE ROW-INKRES TO RAD-INKRES                                        
046800     ADD RAD-INKRES TO TOT-INKRES                                         
046900     WRITE W51287-S-REC FROM ROW                                          
047000*FOR KURANS GRUPP 2                                                       
047100     MOVE 2           TO ROW-KGR                                          
047200     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
047300     MOVE W-KUR2-S TO ROW-LVALUE                                          
047400     IF W-KURSUM-S = ZERO                                                 
047500       MOVE ZERO TO ROW-PROC                                              
047600       MOVE ZERO TO ROW-INKRES                                            
047700     ELSE                                                                 
047800       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR2-S / W-KURSUM-S             
047900       COMPUTE ROW-INKRES ROUNDED = W-KUR2-S * 0 / 100                    
048000     END-IF                                                               
048100     MOVE ROW-INKRES TO RAD-INKRES                                        
048200     ADD RAD-INKRES TO TOT-INKRES                                         
048300     WRITE W51287-S-REC FROM ROW                                          
048400*FOR KURANS GRUPP 3                                                       
048500     MOVE 3           TO ROW-KGR                                          
048600     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
048700     MOVE W-KUR3-S TO ROW-LVALUE                                          
048800     IF W-KURSUM-S = ZERO                                                 
048900       MOVE ZERO TO ROW-PROC                                              
049000       MOVE ZERO TO ROW-INKRES                                            
049100     ELSE                                                                 
049200       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR3-S / W-KURSUM-S             
049300       COMPUTE ROW-INKRES ROUNDED = W-KUR3-S * 18 / 100                   
049400     END-IF                                                               
049500     MOVE ROW-INKRES TO RAD-INKRES                                        
049600     ADD RAD-INKRES TO TOT-INKRES                                         
049700     WRITE W51287-S-REC FROM ROW                                          
049800*FOR KURANS GRUPP 4                                                       
049900     MOVE 4           TO ROW-KGR                                          
050000     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
050100     MOVE W-KUR4-S TO ROW-LVALUE                                          
050200     IF W-KURSUM-S = ZERO                                                 
050300       MOVE ZERO TO ROW-PROC                                              
050400       MOVE ZERO TO ROW-INKRES                                            
050500     ELSE                                                                 
050600       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR4-S / W-KURSUM-S             
050700       COMPUTE ROW-INKRES ROUNDED = W-KUR4-S * 53 / 100                   
050800     END-IF                                                               
050900     MOVE ROW-INKRES TO RAD-INKRES                                        
051000     ADD RAD-INKRES TO TOT-INKRES                                         
051100     WRITE W51287-S-REC FROM ROW                                          
051200*FOR KURANS GRUPP 5                                                       
051300     MOVE 5           TO ROW-KGR                                          
051400     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
051500     MOVE W-KUR5-S TO ROW-LVALUE                                          
051600     IF W-KURSUM-S = ZERO                                                 
051700       MOVE ZERO TO ROW-PROC                                              
051800       MOVE ZERO TO ROW-INKRES                                            
051900     ELSE                                                                 
052000       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR5-S / W-KURSUM-S             
052100       COMPUTE ROW-INKRES ROUNDED = W-KUR5-S * 81 / 100                   
052200     END-IF                                                               
052300     MOVE ROW-INKRES TO RAD-INKRES                                        
052400     ADD RAD-INKRES TO TOT-INKRES                                         
052500     WRITE W51287-S-REC FROM ROW                                          
052600*FOR KURANS GRUPP 6                                                       
052700     MOVE 6           TO ROW-KGR                                          
052800     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
052900     MOVE W-KUR6-S TO ROW-LVALUE                                          
053000     IF W-KURSUM-S = ZERO                                                 
053100       MOVE ZERO TO ROW-PROC                                              
053200       MOVE ZERO TO ROW-INKRES                                            
053300     ELSE                                                                 
053400       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR6-S / W-KURSUM-S             
053500       COMPUTE ROW-INKRES ROUNDED = W-KUR6-S * 100 / 100                  
053600     END-IF                                                               
053700     MOVE ROW-INKRES TO RAD-INKRES                                        
053800     ADD RAD-INKRES TO TOT-INKRES                                         
053900     WRITE W51287-S-REC FROM ROW                                          
054000*FOR KURANS TOTAL FOR PK                                                  
054100     MOVE WS-KDPSLLOC TO ROWT-PKOD                                        
054200     MOVE W-KURSUM-S TO ROWT-LVALUE                                       
054300     IF W-KURSUM-S = ZERO                                                 
054400       MOVE ZERO TO ROWT-PROC                                             
054500     ELSE                                                                 
054600       COMPUTE ROWT-PROC ROUNDED = 100 * W-KURSUM-S / W-KURSUM-S          
054700     END-IF                                                               
054800     MOVE TOT-INKRES                 TO ROWT-INKRES                       
054900     WRITE W51287-S-REC FROM ROW-TOTAL                                    
055000     WRITE W51287-S-REC FROM FILLER-LINE                                  
055100     COMPUTE W-KURSUM2-S = W-KURSUM2-S + W-KURSUM-S                       
055200     MOVE ZERO TO W-KUR1-S                                                
055300     MOVE ZERO TO W-KUR2-S                                                
055400     MOVE ZERO TO W-KUR3-S                                                
055500     MOVE ZERO TO W-KUR4-S                                                
055600     MOVE ZERO TO W-KUR5-S                                                
055700     MOVE ZERO TO W-KUR6-S                                                
055800     MOVE ZERO TO W-KURSUM-S                                              
055900     MOVE ZERO TO TOT-INKRES                                              
056000     .                                                                    
056100     SKIP3                                                                
056200 BD-HEADER-KGR SECTION.                                                   
056300                                                                          
056400     MOVE W-IDLAND-CURR TO RUB1-IDLAND                                    
056500                           LAND-IDLANDX2                                  
056600     MOVE SPACE         TO LAND-IDLANDX3                                  
056700     CALL WISOLAND USING LAND-WISOLAND                                    
056800     IF LAND-KDSVAR = SPACE                                               
056900        MOVE LAND-KDVALISO(1)  TO RUB1-KDVALISO                           
057000     ELSE                                                                 
057100        MOVE SPACE             TO RUB1-KDVALISO                           
057200     END-IF                                                               
057300                                                                          
057400     WRITE W51287-K-REC FROM MAIN-LINE1                                   
057500     WRITE W51287-K-REC FROM FILLER-LINE                                  
057600     WRITE W51287-K-REC FROM ROW-LINE2                                    
057700     WRITE W51287-K-REC FROM FILLER-LINE                                  
057800     .                                                                    
057900                                                                          
058000                                                                          
058100 BE-COMPUTE-STOCKVALUE SECTION.                                           
058200     COMPUTE W-KUR1 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS1) +          
058300                               W-KUR1                                     
058400     COMPUTE W-KUR2 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS2) +          
058500                               W-KUR2                                     
058600     COMPUTE W-KUR3 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS3) +          
058700                               W-KUR3                                     
058800     COMPUTE W-KUR4 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS4) +          
058900                               W-KUR4                                     
059000     COMPUTE W-KUR5 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS5) +          
059100                               W-KUR5                                     
059200     COMPUTE W-KUR6 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS6) +          
059300                               W-KUR6                                     
059400     .                                                                    
059500     EJECT                                                                
059600                                                                          
059700 BF-WRITE-KGR-POST  SECTION.                                              
059800*TOTAL TURNOVERVALUE PER PRODUCT GROUP                                    
059900     MOVE WS-KDPSLLOC TO ROW2-PKOD                                        
060000     MOVE W-KUR1 TO ROW2-LVALUE1                                          
060100     MOVE W-KUR2 TO ROW2-LVALUE2                                          
060200     MOVE W-KUR3 TO ROW2-LVALUE3                                          
060300     MOVE W-KUR4 TO ROW2-LVALUE4                                          
060400     MOVE W-KUR5 TO ROW2-LVALUE5                                          
060500     MOVE W-KUR6 TO ROW2-LVALUE6                                          
060600     COMPUTE W-KURSUM = W-KUR1 + W-KUR2 + W-KUR3 + W-KUR4 + W-KUR5        
060700                      + W-KUR6                                            
060800     WRITE W51287-K-REC FROM ROW2                                         
060900     COMPUTE WS-KUR1 = WS-KUR1 + W-KUR1                                   
061000     COMPUTE WS-KUR2 = WS-KUR2 + W-KUR2                                   
061100     COMPUTE WS-KUR3 = WS-KUR3 + W-KUR3                                   
061200     COMPUTE WS-KUR4 = WS-KUR4 + W-KUR4                                   
061300     COMPUTE WS-KUR5 = WS-KUR5 + W-KUR5                                   
061400     COMPUTE WS-KUR6 = WS-KUR6 + W-KUR6                                   
061500     COMPUTE W-KURSUM2 = WS-KUR1 + WS-KUR2 + WS-KUR3 + WS-KUR4 +          
061600                         WS-KUR5 + WS-KUR6                                
061700     MOVE ZERO TO W-KUR1                                                  
061800     MOVE ZERO TO W-KUR2                                                  
061900     MOVE ZERO TO W-KUR3                                                  
062000     MOVE ZERO TO W-KUR4                                                  
062100     MOVE ZERO TO W-KUR5                                                  
062200     MOVE ZERO TO W-KUR6                                                  
062300     .                                                                    
062400     SKIP3                                                                
062500                                                                          
062600 BG-WRITE-TOTAL   SECTION.                                                
062700                                                                          
062800     MOVE WS-KUR1 TO ROW2T-LVALUE1                                        
062900     MOVE WS-KUR2 TO ROW2T-LVALUE2                                        
063000     MOVE WS-KUR3 TO ROW2T-LVALUE3                                        
063100     MOVE WS-KUR4 TO ROW2T-LVALUE4                                        
063200     MOVE WS-KUR5 TO ROW2T-LVALUE5                                        
063300     MOVE WS-KUR6 TO ROW2T-LVALUE6                                        
063400     WRITE W51287-K-REC FROM ROW2-TOTAL                                   
063500     WRITE W51287-K-REC FROM FILLER-LINE                                  
063600     .                                                                    
063700     SKIP3                                                                
063800                                                                          
063900 BH-COMPUTE-OBSOLITE   SECTION.                                           
064000                                                                          
064100     COMPUTE WO-KUR1   ROUNDED = WS-KUR1 * 0   / 100                      
064200     COMPUTE WO-KUR2   ROUNDED = WS-KUR2 * 0   / 100                      
064300     COMPUTE WO-KUR3   ROUNDED = WS-KUR3 * 18  / 100                      
064400     COMPUTE WO-KUR4   ROUNDED = WS-KUR4 * 53  / 100                      
064500     COMPUTE WO-KUR5   ROUNDED = WS-KUR5 * 81  / 100                      
064600     COMPUTE WO-KUR6   ROUNDED = WS-KUR6 * 100 / 100                      
064700     COMPUTE WO-KURSUM = WO-KUR1 + WO-KUR2 + WO-KUR3 + WO-KUR4 +          
064800                         WO-KUR5 + WO-KUR6                                
064900     MOVE WO-KUR1   TO ROW3T-LVALUE1                                      
065000     MOVE WO-KUR2   TO ROW3T-LVALUE2                                      
065100     MOVE WO-KUR3   TO ROW3T-LVALUE3                                      
065200     MOVE WO-KUR4   TO ROW3T-LVALUE4                                      
065300     MOVE WO-KUR5   TO ROW3T-LVALUE5                                      
065400     MOVE WO-KUR6   TO ROW3T-LVALUE6                                      
065500     MOVE WO-KURSUM TO ROW4T-LVALUE1                                      
065600                                                                          
065700     IF WO-KURSUM = ZERO                                                  
065800     OR WS-KURSUM = ZERO                                                  
065900       MOVE ZERO TO ROW4T-PROC                                            
066000     ELSE                                                                 
066100       COMPUTE ROW4T-PROC ROUNDED = 100 * WO-KURSUM / WS-KURSUM           
066200     END-IF                                                               
066300     .                                                                    
066400     EJECT                                                                
066500                                                                          
066600 BI-WRITE-SUPERTOTAL SECTION.                                             
066700                                                                          
066800     WRITE W51287-K-REC FROM ROW-OBSOL                                    
066900     WRITE W51287-K-REC FROM FILLER-LINE                                  
067000     WRITE W51287-K-REC FROM ROWT-OBSOL                                   
067100     WRITE W51287-K-REC FROM FILLER-LINE                                  
067200     MOVE W-KURSUM2 TO ROWS-LVALUE                                        
067300     WRITE W51287-K-REC FROM ROW-SUPERTOTAL                               
067400     .                                                                    
067500     EJECT                                                                
067600                                                                          
067700 Z-END    SECTION.                                                        
067800     CLOSE W51284                                                         
067900           W51287S W51287K                                                
068000                                                                          
068100     MOVE 'S'        TO POSTSUM-OPKOD                                     
068200     CALL POSTSUM USING POSTSUM-PARM                                      
068300     .                                                                    
068400     EJECT                                                                
068500                                                                          
068600 S01-READ-W51284-POST SECTION.                                            
068700     READ W51284 INTO INAREA                                              
068800     AT END                                                               
068900       MOVE JA TO EOF-W51284                                              
069000     NOT AT END                                                           
069100       MOVE W51284-TRANSID TO POSTSUM-TRANSID                             
069200       CALL POSTSUM USING POSTSUM-PARM                                    
069300     END-READ                                                             
069400     .                                                                    
069500     EJECT                                                                
069600 S02-SKRIV-DAP1-S SECTION.                                                
069700                                                                          
069800     MOVE ' ¤DAPW51287-001' TO W001-DAP                                   
069900     WRITE W51287-S-REC  FROM W001-DAP                                    
070000                                                                          
070100     MOVE SPACE TO W001-DAP                                               
070200     .                                                                    
070300                                                                          
070400 S03-SKRIV-DAP2-S SECTION.                                                
070500                                                                          
070600     STRING ' ¤DAP' W-IDLAND-CURR                                         
070700            DELIMITED BY SIZE INTO W001-DAP                               
070800     WRITE W51287-S-REC  FROM W001-DAP                                    
070900                                                                          
071000     MOVE SPACE TO W001-DAP                                               
071100     .                                                                    
071200 S04-SKRIV-DAP1-K SECTION.                                                
071300                                                                          
071400     MOVE ' ¤DAPW51287-002' TO W001-DAP                                   
071500     WRITE W51287-K-REC  FROM W001-DAP                                    
071600                                                                          
071700     MOVE SPACE TO W001-DAP                                               
071800     .                                                                    
071900 S05-SKRIV-DAP2-K SECTION.                                                
072000                                                                          
072100     STRING ' ¤DAP' W-IDLAND-CURR                                         
072200            DELIMITED BY SIZE INTO W001-DAP                               
072300     WRITE W51287-K-REC  FROM W001-DAP                                    
072400                                                                          
072500     MOVE SPACE TO W001-DAP                                               
072600     .                                                                    
072700 S10-SOK-IDLAND SECTION.                                                  
072800                                                                          
072900     SEARCH ALL DC-LAND                                                   
073000        AT END                                                            
073100           MOVE SPACE          TO W-IDLAND                                
073200        WHEN DCLAND-IDDC (DCLAND-IX) = IN-IDDC                            
073300           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
073400                               TO W-IDLAND                                
073500     END-SEARCH                                                           
073600     .                                                                    

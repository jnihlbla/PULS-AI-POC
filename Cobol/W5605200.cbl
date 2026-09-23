000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W5605200.                                                 
000300                                                                          
000400*    AUTHOR.        ANDERS HENRIKSSON.                                    
000500*    DATE-WRITTEN   SEPTEMBER 05.                                         
000600*                                                                         
000700*    FUNKTION:                                                            
000800*               SKAPAR W56052-0XX FÖR KURANSGRUPPER PER DC                
000900*               TURNOVERVALUE PER PRODUCTGROUP                            
001000*               TURNOVERVALUE PER PRODUCTGROUP AND TURNOVERGROUP          
001100*                                                                         
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600                                                                          
001700     SELECT W56051  ASSIGN       TO W56052D1.                             
001800                                                                          
001900     SELECT W56052S ASSIGN       TO W56052D2.                             
002000     SELECT W56052K ASSIGN       TO W56052D3.                             
002100     EJECT                                                                
002200                                                                          
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W56051                                                               
002700     RECORDING F                                                          
002800     BLOCK CONTAINS 0.                                                    
002900                                                                          
003000*01  INTRANS     -COPY W56051    -L                                       
003100     SKIP2                                                                
003200                                                                          
003300 FD  W56052S                                                              
003400     RECORDING V                                                          
003500     BLOCK CONTAINS 0.                                                    
003600                                                                          
003700 01  W56052-S-REC            PIC X(121).                                  
003800     SKIP2                                                                
003900                                                                          
004000 FD  W56052K                                                              
004100     RECORDING V                                                          
004200     BLOCK CONTAINS 0.                                                    
004300                                                                          
004400 01  W56052-K-REC            PIC X(121).                                  
004500     EJECT                                                                
004600                                                                          
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP3                                                                
004900 77  IDPGM                   PIC X(8)      VALUE 'W5605200'.              
005000 77  JA                      PIC X         VALUE 'J'.                     
005100 77  NEJ                     PIC X         VALUE 'N'.                     
005200 77  EOF-W56051              PIC X         VALUE 'N'.                     
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
006400 01  W56051-TRANSID.                                                      
006500     03  FILLER              PIC X(6) VALUE 'W56051'.                     
006600     03  FILLER              PIC X(8) VALUE 'W56052D1'.                   
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
007800 01  W-IDLAND-X.                                                          
007900     03  W-IDLAND            PIC X(2)    VALUE SPACE.                     
008000*01  -COPY WWDCLAND                                                       
008100                                                                          
008200 01  FILLER                  PIC X(16)   VALUE 'W56051-AREA'.             
008300 01  INAREA.                                                              
008400*    03  -COPY W56051     -PRE IN-                                        
008500     EJECT                                                                
008600                                                                          
008700 01  W001-DAP.                                                            
008800     03  FILLER                  PIC X(165)  VALUE SPACE.                 
008900                                                                          
009000*FOR SHELFLIFE                                                            
009100 01  TEXT-AREA.                                                           
009200     03  MAIN-LINE1.                                                      
009300         05  FILLER          PIC X(8)    VALUE                            
009400             'W56052-0'.                                                  
009500         05  RUB1-IDDC       PIC XX.                                      
009600         05  FILLER          PIC X(1)    VALUE '	'.                       
009700         05  FILLER          PIC X(17)   VALUE                            
009800             'STOCK TURNOVER BY'.                                         
009900         05  FILLER          PIC X(1)    VALUE '	'.                       
010000         05  FILLER          PIC X(16)   VALUE                            
010100             'SHELF-LIFE METOD'.                                          
010200         05  FILLER          PIC X(1)    VALUE '	'.                       
010300         05  FILLER          PIC X(5)    VALUE 'DATE '.                   
010400         05  RUB1-YYYY       PIC 9999.                                    
010500         05  FILLER          PIC X(1)    VALUE '-'.                       
010600         05  RUB1-MM         PIC 99.                                      
010700         05  FILLER          PIC X(1)    VALUE '-'.                       
010800         05  RUB1-DD         PIC 99.                                      
010900         05  FILLER          PIC X(1)    VALUE '	'.                       
011000         05  FILLER          PIC X(9)    VALUE 'CURRENCY '.               
011100         05  RUB1-KDVALISO   PIC X(3).                                    
011200         05  FILLER          PIC X(1)    VALUE '	'.                       
011300         05  FILLER          PIC X(1)    VALUE '	'.                       
011400                                                                          
011500     03  FILLER-LINE.                                                     
011600         05  FILLER          PIC X(1)    VALUE '	'.                       
011700         05  FILLER          PIC X(1)    VALUE '	'.                       
011800         05  FILLER          PIC X(1)    VALUE '	'.                       
011900         05  FILLER          PIC X(1)    VALUE '	'.                       
012000         05  FILLER          PIC X(1)    VALUE '	'.                       
012100         05  FILLER          PIC X(1)    VALUE '	'.                       
012200                                                                          
012300     03  ROW-LINE1.                                                       
012400         05  FILLER          PIC X(14)   VALUE                            
012500            'TURNOVER GROUP'.                                             
012600         05  FILLER          PIC X(1)    VALUE '	'.                       
012700         05  FILLER          PIC X(13)   VALUE                            
012800            'PRODUCT GROUP'.                                              
012900         05  FILLER          PIC X(1)    VALUE '	'.                       
013000         05  FILLER          PIC X(11)   VALUE                            
013100            'STOCKVALUE'.                                                 
013200         05  FILLER          PIC X(1)    VALUE '	'.                       
013300         05  FILLER          PIC X(24)   VALUE                            
013400            '% OF STOCKVALUE IN P.GR'.                                    
013500         05  FILLER          PIC X(1)    VALUE '	'.                       
013600         05  FILLER          PIC X(12)   VALUE                            
013700            'OBSOLESCENCE'.                                               
013800         05  FILLER          PIC X(1)    VALUE '	'.                       
013900         05  FILLER          PIC X(1)    VALUE '	'.                       
014000                                                                          
014100     03  ROW.                                                             
014200         05  ROW-KGR         PIC 9.                                       
014300         05  FILLER          PIC X(1)    VALUE '	'.                       
014400         05  ROW-PKOD        PIC 99.                                      
014500         05  FILLER          PIC X(1)    VALUE '	'.                       
014600         05  ROW-LVALUE      PIC Z(9)9.99-.                               
014700         05  FILLER          PIC X(1)    VALUE '	'.                       
014800         05  ROW-PROC        PIC Z(2)9.9-.                                
014900         05  FILLER          PIC X(1)    VALUE '	'.                       
015000         05  ROW-INKRES      PIC Z(9)9.99-.                               
015100         05  FILLER          PIC X(1)    VALUE '	'.                       
015200         05  FILLER          PIC X(1)    VALUE '	'.                       
015300                                                                          
015400     03  ROW-TOTAL.                                                       
015500         05  FILLER          PIC X(5)    VALUE                            
015600            'TOTAL'.                                                      
015700         05  FILLER          PIC X(1)    VALUE '	'.                       
015800         05  ROWT-PKOD       PIC 99.                                      
015900         05  FILLER          PIC X(1)    VALUE '	'.                       
016000         05  ROWT-LVALUE     PIC Z(9)9.99-.                               
016100         05  FILLER          PIC X(1)    VALUE '	'.                       
016200         05  ROWT-PROC       PIC Z(2)9.9-.                                
016300         05  FILLER          PIC X(1)    VALUE '	'.                       
016400         05  ROWT-INKRES     PIC Z(9)9.99-.                               
016500         05  FILLER          PIC X(1)    VALUE '	'.                       
016600         05  FILLER          PIC X(1)    VALUE '	'.                       
016700                                                                          
016800     03  ROW-SUPERTOTAL.                                                  
016900         05  FILLER          PIC X(17)   VALUE                            
017000            'TOTAL IN TURNOVER'.                                          
017100         05  FILLER          PIC X(1)    VALUE '	'.                       
017200         05  ROWS-LVALUE     PIC Z(9)9.99-.                               
017300         05  FILLER          PIC X(1)    VALUE '	'.                       
017400         05  FILLER          PIC X(1)    VALUE '	'.                       
017500         05  FILLER          PIC X(1)    VALUE '	'.                       
017600         05  FILLER          PIC X(1)    VALUE '	'.                       
017700         05  FILLER          PIC X(1)    VALUE '	'.                       
017800     EJECT                                                                
017900                                                                          
018000*FOR KGR                                                                  
018100 01  TEXT-AREA2.                                                          
018200     03  ROW-LINE2.                                                       
018300         05  FILLER          PIC X(13)   VALUE                            
018400            'PRODUCT GROUP'.                                              
018500         05  FILLER          PIC X(1)    VALUE '	'.                       
018600         05  FILLER          PIC X(17)   VALUE                            
018700            'STOCKVALUE T.GR 1'.                                          
018800         05  FILLER          PIC X(1)    VALUE '	'.                       
018900         05  FILLER          PIC X(17)   VALUE                            
019000            'STOCKVALUE T.GR 2'.                                          
019100         05  FILLER          PIC X(1)    VALUE '	'.                       
019200         05  FILLER          PIC X(17)   VALUE                            
019300            'STOCKVALUE T.GR 3'.                                          
019400         05  FILLER          PIC X(1)    VALUE '	'.                       
019500         05  FILLER          PIC X(17)   VALUE                            
019600            'STOCKVALUE T.GR 4'.                                          
019700         05  FILLER          PIC X(1)    VALUE '	'.                       
019800         05  FILLER          PIC X(17)   VALUE                            
019900            'STOCKVALUE T.GR 5'.                                          
020000         05  FILLER          PIC X(1)    VALUE '	'.                       
020100         05  FILLER          PIC X(17)   VALUE                            
020200            'STOCKVALUE T.GR 6'.                                          
020300                                                                          
020400     03  ROW2.                                                            
020500         05  ROW2-PKOD       PIC 99.                                      
020600         05  FILLER          PIC X(1)    VALUE '	'.                       
020700         05  ROW2-LVALUE1    PIC Z(9)9.99-.                               
020800         05  FILLER          PIC X(1)    VALUE '	'.                       
020900         05  ROW2-LVALUE2    PIC Z(9)9.99-.                               
021000         05  FILLER          PIC X(1)    VALUE '	'.                       
021100         05  ROW2-LVALUE3    PIC Z(9)9.99-.                               
021200         05  FILLER          PIC X(1)    VALUE '	'.                       
021300         05  ROW2-LVALUE4    PIC Z(9)9.99-.                               
021400         05  FILLER          PIC X(1)    VALUE '	'.                       
021500         05  ROW2-LVALUE5    PIC Z(9)9.99-.                               
021600         05  FILLER          PIC X(1)    VALUE '	'.                       
021700         05  ROW2-LVALUE6    PIC Z(9)9.99-.                               
021800                                                                          
021900     03  ROW2-TOTAL.                                                      
022000         05  FILLER          PIC X(5)    VALUE                            
022100            'TOTAL'.                                                      
022200         05  FILLER          PIC X(1)    VALUE '	'.                       
022300         05  ROW2T-LVALUE1   PIC Z(9)9.99-.                               
022400         05  FILLER          PIC X(1)    VALUE '	'.                       
022500         05  ROW2T-LVALUE2   PIC Z(9)9.99-.                               
022600         05  FILLER          PIC X(1)    VALUE '	'.                       
022700         05  ROW2T-LVALUE3   PIC Z(9)9.99-.                               
022800         05  FILLER          PIC X(1)    VALUE '	'.                       
022900         05  ROW2T-LVALUE4   PIC Z(9)9.99-.                               
023000         05  FILLER          PIC X(1)    VALUE '	'.                       
023100         05  ROW2T-LVALUE5   PIC Z(9)9.99-.                               
023200         05  FILLER          PIC X(1)    VALUE '	'.                       
023300         05  ROW2T-LVALUE6   PIC Z(9)9.99-.                               
023400                                                                          
023500     03  ROW-OBSOL.                                                       
023600         05  FILLER          PIC X(12)   VALUE                            
023700            'OBSOLESCENCE'.                                               
023800         05  FILLER          PIC X(1)    VALUE '	'.                       
023900         05  ROW3T-LVALUE1   PIC Z(9)9.99-.                               
024000         05  FILLER          PIC X(1)    VALUE '	'.                       
024100         05  ROW3T-LVALUE2   PIC Z(9)9.99-.                               
024200         05  FILLER          PIC X(1)    VALUE '	'.                       
024300         05  ROW3T-LVALUE3   PIC Z(9)9.99-.                               
024400         05  FILLER          PIC X(1)    VALUE '	'.                       
024500         05  ROW3T-LVALUE4   PIC Z(9)9.99-.                               
024600         05  FILLER          PIC X(1)    VALUE '	'.                       
024700         05  ROW3T-LVALUE5   PIC Z(9)9.99-.                               
024800         05  FILLER          PIC X(1)    VALUE '	'.                       
024900         05  ROW3T-LVALUE6   PIC Z(9)9.99-.                               
025000                                                                          
025100     03  ROWT-OBSOL.                                                      
025200         05  FILLER          PIC X(9)   VALUE                             
025300            'TOTAL OBS'.                                                  
025400         05  FILLER          PIC X(1)    VALUE '	'.                       
025500         05  ROW4T-LVALUE1   PIC Z(9)9.99-.                               
025600         05  FILLER          PIC X(1)    VALUE '	'.                       
025700         05  ROW4T-PROC      PIC Z(2)9.9-.                                
025800         05  FILLER          PIC X(1)    VALUE '	'.                       
025900         05  FILLER          PIC X(10)   VALUE                            
026000            '% OF TOTAL'.                                                 
026100         05  FILLER          PIC X(1)    VALUE '	'.                       
026200         05  FILLER          PIC X(1)    VALUE '	'.                       
026300         05  FILLER          PIC X(1)    VALUE '	'.                       
026400     EJECT                                                                
026500                                                                          
026600 01  FILLER                  PIC X(16)   VALUE 'W-KURTAB-S '.             
026700 01  W-KURTAB-S.                                                          
026800     03  W-KUR1-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
026900     03  W-KUR2-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
027000     03  W-KUR3-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
027100     03  W-KUR4-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
027200     03  W-KUR5-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
027300     03  W-KUR6-S            PIC S9(11)V99  COMP-3 VALUE ZERO.            
027400     03  W-KURSUM-S          PIC S9(11)V99  COMP-3 VALUE ZERO.            
027500     03  W-KURSUM2-S         PIC S9(11)V99  COMP-3 VALUE ZERO.            
027600                                                                          
027700 01  FILLER                  PIC X(16)   VALUE 'W-KURTAB   '.             
027800 01  W-KURTAB.                                                            
027900     03  W-KUR1              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028000     03  W-KUR2              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028100     03  W-KUR3              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028200     03  W-KUR4              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028300     03  W-KUR5              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028400     03  W-KUR6              PIC S9(11)V99  COMP-3 VALUE ZERO.            
028500     03  W-KURSUM            PIC S9(11)V99  COMP-3 VALUE ZERO.            
028600     03  W-KURSUM2           PIC S9(11)V99  COMP-3 VALUE ZERO.            
028700                                                                          
028800 01  FILLER                  PIC X(16)   VALUE 'WS-KURTAB  '.             
028900 01  WS-KURTAB.                                                           
029000     03  WS-KUR1             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029100     03  WS-KUR2             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029200     03  WS-KUR3             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029300     03  WS-KUR4             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029400     03  WS-KUR5             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029500     03  WS-KUR6             PIC S9(11)V99  COMP-3 VALUE ZERO.            
029600     03  WS-KURSUM           PIC S9(11)V99  COMP-3 VALUE ZERO.            
029700                                                                          
029800 01  FILLER                  PIC X(16)   VALUE 'WO-KURTAB  '.             
029900 01  WO-KURTAB.                                                           
030000     03  WO-KUR1             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030100     03  WO-KUR2             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030200     03  WO-KUR3             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030300     03  WO-KUR4             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030400     03  WO-KUR5             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030500     03  WO-KUR6             PIC S9(11)V99  COMP-3 VALUE ZERO.            
030600     03  WO-KURSUM           PIC S9(11)V99  COMP-3 VALUE ZERO.            
030700                                                                          
030800 PROCEDURE DIVISION.                                                      
030900                                                                          
031000 MAIN SECTION.                                                            
031100                                                                          
031200     PERFORM A-INIT                                                       
031300                                                                          
031400     PERFORM S01-READ-W56051-POST                                         
031500     IF EOF-W56051 = JA                                                   
031600        CONTINUE                                                          
031700     ELSE                                                                 
031800        PERFORM UNTIL EOF-W56051 = JA                                     
031900           PERFORM B-CREATE-SH-O-KGR-POST                                 
032000        END-PERFORM                                                       
032100     END-IF                                                               
032200                                                                          
032300     PERFORM Z-END                                                        
032400                                                                          
032500     MOVE ZERO TO RETURN-CODE                                             
032600     GOBACK                                                               
032700     .                                                                    
032800     EJECT                                                                
032900                                                                          
033000 A-INIT SECTION.                                                          
033100     OPEN INPUT  W56051                                                   
033200          OUTPUT W56052S W56052K                                          
033300                                                                          
033400     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
033500                                                                          
033600     MOVE FUNCTION CURRENT-DATE(1:4) TO RUB1-YYYY                         
033700     MOVE FUNCTION CURRENT-DATE(5:2) TO RUB1-MM                           
033800     MOVE FUNCTION CURRENT-DATE(7:2) TO RUB1-DD                           
033900     .                                                                    
034000     EJECT                                                                
034100                                                                          
034200 B-CREATE-SH-O-KGR-POST SECTION.                                          
034300                                                                          
034400     MOVE SPACE        TO WS-IDDC                                         
034500     MOVE ZERO         TO WS-KDPSLLOC                                     
034600                                                                          
034700     PERFORM UNTIL EOF-W56051 = JA                                        
034800       IF IN-IDDC NOT = WS-IDDC                                           
034900                                                                          
035000          IF WS-IDDC NOT = SPACE                                          
035100             PERFORM BF-WRITE-KGR-POST                                    
035200             PERFORM BG-WRITE-TOTAL                                       
035300             MOVE W-KURSUM2-S TO ROWS-LVALUE                              
035400             MOVE W-KURSUM2-S TO WS-KURSUM                                
035500             PERFORM BH-COMPUTE-OBSOLITE                                  
035600             PERFORM BI-WRITE-SUPERTOTAL                                  
035700                                                                          
035800             PERFORM BC-WRITE-SHELFLIFE-POST                              
035900                                                                          
036000             MOVE W-KURSUM2-S TO ROWS-LVALUE                              
036100             MOVE W-KURSUM2-S TO WS-KURSUM                                
036200                                                                          
036300             WRITE W56052-S-REC FROM ROW-SUPERTOTAL                       
036400             WRITE W56052-S-REC FROM FILLER-LINE                          
036500             MOVE ZERO TO W-KURSUM2-S                                     
036600                          WS-KURSUM                                       
036700                          WS-KUR1                                         
036800                          WS-KUR2                                         
036900                          WS-KUR3                                         
037000                          WS-KUR4                                         
037100                          WS-KUR5                                         
037200                          WS-KUR6                                         
037300          END-IF                                                          
037400          MOVE IN-IDDC     TO WS-IDDC                                     
037500          MOVE IN-KDPSLLOC TO WS-KDPSLLOC                                 
037600          PERFORM S02-SKRIV-DAP1-S                                        
037700          PERFORM S03-SKRIV-DAP2-S                                        
037800          PERFORM BA-HEADER-SHELFLIFE                                     
037900          PERFORM S04-SKRIV-DAP1-K                                        
038000          PERFORM S05-SKRIV-DAP2-K                                        
038100          PERFORM BD-HEADER-KGR                                           
038200       END-IF                                                             
038300       IF WS-KDPSLLOC = IN-KDPSLLOC                                       
038400         PERFORM BB-COMPUTE-STOCKVALUE                                    
038500         PERFORM BE-COMPUTE-STOCKVALUE                                    
038600       ELSE                                                               
038700         IF WS-KDPSLLOC > ZERO                                            
038800            PERFORM BC-WRITE-SHELFLIFE-POST                               
038900         END-IF                                                           
039000         PERFORM BB-COMPUTE-STOCKVALUE                                    
039100         PERFORM BF-WRITE-KGR-POST                                        
039200         MOVE IN-KDPSLLOC  TO WS-KDPSLLOC                                 
039300         PERFORM BE-COMPUTE-STOCKVALUE                                    
039400       END-IF                                                             
039500       PERFORM S01-READ-W56051-POST                                       
039600     END-PERFORM                                                          
039700                                                                          
039800     PERFORM BC-WRITE-SHELFLIFE-POST                                      
039900                                                                          
040000     MOVE W-KURSUM2-S TO ROWS-LVALUE                                      
040100     MOVE W-KURSUM2-S TO WS-KURSUM                                        
040200                                                                          
040300     WRITE W56052-S-REC FROM ROW-SUPERTOTAL                               
040400     WRITE W56052-S-REC FROM FILLER-LINE                                  
040500                                                                          
040600     PERFORM BF-WRITE-KGR-POST                                            
040700     PERFORM BG-WRITE-TOTAL                                               
040800     PERFORM BH-COMPUTE-OBSOLITE                                          
040900     PERFORM BI-WRITE-SUPERTOTAL                                          
041000     .                                                                    
041100     EJECT                                                                
041200                                                                          
041300 BA-HEADER-SHELFLIFE SECTION.                                             
041400                                                                          
041500     MOVE IN-IDDC      TO RUB1-IDDC                                       
041600                                                                          
041700     MOVE SPACE TO W-IDLAND                                               
041800     SEARCH ALL DC-LAND                                                   
041900        AT END                                                            
042000           MOVE SPACE          TO W-IDLAND                                
042100        WHEN DCLAND-IDDC (DCLAND-IX) = IN-IDDC                            
042200           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
042300                               TO W-IDLAND                                
042400     END-SEARCH                                                           
042500                                                                          
042600     MOVE W-IDLAND        TO LAND-IDLANDX2                                
042700     MOVE SPACE           TO LAND-IDLANDX3                                
042800     CALL WISOLAND USING LAND-WISOLAND                                    
042900     IF LAND-KDSVAR = SPACE                                               
043000        MOVE LAND-KDVALISO(1)  TO RUB1-KDVALISO                           
043100     ELSE                                                                 
043200        MOVE SPACE             TO RUB1-KDVALISO                           
043300     END-IF                                                               
043400                                                                          
043500     WRITE W56052-S-REC  FROM MAIN-LINE1                                  
043600     WRITE W56052-S-REC  FROM FILLER-LINE                                 
043700     WRITE W56052-S-REC  FROM ROW-LINE1                                   
043800     WRITE W56052-S-REC  FROM FILLER-LINE                                 
043900     .                                                                    
044000                                                                          
044100                                                                          
044200 BB-COMPUTE-STOCKVALUE SECTION.                                           
044300     COMPUTE W-KUR1-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS1) +        
044400                                 W-KUR1-S                                 
044500     COMPUTE W-KUR2-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS2) +        
044600                                 W-KUR2-S                                 
044700     COMPUTE W-KUR3-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS3) +        
044800                                 W-KUR3-S                                 
044900     COMPUTE W-KUR4-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS4) +        
045000                                 W-KUR4-S                                 
045100     COMPUTE W-KUR5-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS5) +        
045200                                 W-KUR5-S                                 
045300     COMPUTE W-KUR6-S ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS6) +        
045400                                 W-KUR6-S                                 
045500     COMPUTE W-KURSUM-S = W-KUR1-S + W-KUR2-S + W-KUR3-S                  
045600                        + W-KUR4-S + W-KUR5-S + W-KUR6-S                  
045700     .                                                                    
045800     EJECT                                                                
045900                                                                          
046000 BC-WRITE-SHELFLIFE-POST SECTION.                                         
046100     MOVE ZERO TO TOT-INKRES                                              
046200*FOR KURANS GRUPP 1                                                       
046300     MOVE 1           TO ROW-KGR                                          
046400     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
046500     MOVE W-KUR1-S TO ROW-LVALUE                                          
046600     IF W-KURSUM-S = ZERO                                                 
046700       MOVE ZERO TO ROW-PROC                                              
046800       MOVE ZERO TO ROW-INKRES                                            
046900     ELSE                                                                 
047000       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR1-S / W-KURSUM-S             
047100       COMPUTE ROW-INKRES ROUNDED = W-KUR1-S * 0 / 100                    
047200     END-IF                                                               
047300     MOVE ROW-INKRES TO RAD-INKRES                                        
047400     ADD RAD-INKRES TO TOT-INKRES                                         
047500     WRITE W56052-S-REC FROM ROW                                          
047600*FOR KURANS GRUPP 2                                                       
047700     MOVE 2           TO ROW-KGR                                          
047800     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
047900     MOVE W-KUR2-S TO ROW-LVALUE                                          
048000     IF W-KURSUM-S = ZERO                                                 
048100       MOVE ZERO TO ROW-PROC                                              
048200       MOVE ZERO TO ROW-INKRES                                            
048300     ELSE                                                                 
048400       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR2-S / W-KURSUM-S             
048500       COMPUTE ROW-INKRES ROUNDED = W-KUR2-S * 0 / 100                    
048600     END-IF                                                               
048700     MOVE ROW-INKRES TO RAD-INKRES                                        
048800     ADD RAD-INKRES TO TOT-INKRES                                         
048900     WRITE W56052-S-REC FROM ROW                                          
049000*FOR KURANS GRUPP 3                                                       
049100     MOVE 3           TO ROW-KGR                                          
049200     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
049300     MOVE W-KUR3-S TO ROW-LVALUE                                          
049400     IF W-KURSUM-S = ZERO                                                 
049500       MOVE ZERO TO ROW-PROC                                              
049600       MOVE ZERO TO ROW-INKRES                                            
049700     ELSE                                                                 
049800       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR3-S / W-KURSUM-S             
049900       COMPUTE ROW-INKRES ROUNDED = W-KUR3-S * 18 / 100                   
050000     END-IF                                                               
050100     MOVE ROW-INKRES TO RAD-INKRES                                        
050200     ADD RAD-INKRES TO TOT-INKRES                                         
050300     WRITE W56052-S-REC FROM ROW                                          
050400*FOR KURANS GRUPP 4                                                       
050500     MOVE 4           TO ROW-KGR                                          
050600     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
050700     MOVE W-KUR4-S TO ROW-LVALUE                                          
050800     IF W-KURSUM-S = ZERO                                                 
050900       MOVE ZERO TO ROW-PROC                                              
051000       MOVE ZERO TO ROW-INKRES                                            
051100     ELSE                                                                 
051200       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR4-S / W-KURSUM-S             
051300       COMPUTE ROW-INKRES ROUNDED = W-KUR4-S * 53 / 100                   
051400     END-IF                                                               
051500     MOVE ROW-INKRES TO RAD-INKRES                                        
051600     ADD RAD-INKRES TO TOT-INKRES                                         
051700     WRITE W56052-S-REC FROM ROW                                          
051800*FOR KURANS GRUPP 5                                                       
051900     MOVE 5           TO ROW-KGR                                          
052000     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
052100     MOVE W-KUR5-S TO ROW-LVALUE                                          
052200     IF W-KURSUM-S = ZERO                                                 
052300       MOVE ZERO TO ROW-PROC                                              
052400       MOVE ZERO TO ROW-INKRES                                            
052500     ELSE                                                                 
052600       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR5-S / W-KURSUM-S             
052700       COMPUTE ROW-INKRES ROUNDED = W-KUR5-S * 81 / 100                   
052800     END-IF                                                               
052900     MOVE ROW-INKRES TO RAD-INKRES                                        
053000     ADD RAD-INKRES TO TOT-INKRES                                         
053100     WRITE W56052-S-REC FROM ROW                                          
053200*FOR KURANS GRUPP 6                                                       
053300     MOVE 6           TO ROW-KGR                                          
053400     MOVE WS-KDPSLLOC TO ROW-PKOD                                         
053500     MOVE W-KUR6-S TO ROW-LVALUE                                          
053600     IF W-KURSUM-S = ZERO                                                 
053700       MOVE ZERO TO ROW-PROC                                              
053800       MOVE ZERO TO ROW-INKRES                                            
053900     ELSE                                                                 
054000       COMPUTE ROW-PROC ROUNDED = 100 * W-KUR6-S / W-KURSUM-S             
054100       COMPUTE ROW-INKRES ROUNDED = W-KUR6-S * 100 / 100                  
054200     END-IF                                                               
054300     MOVE ROW-INKRES TO RAD-INKRES                                        
054400     ADD RAD-INKRES TO TOT-INKRES                                         
054500     WRITE W56052-S-REC FROM ROW                                          
054600*FOR KURANS TOTAL FOR PK                                                  
054700     MOVE WS-KDPSLLOC TO ROWT-PKOD                                        
054800     MOVE W-KURSUM-S TO ROWT-LVALUE                                       
054900     IF W-KURSUM-S = ZERO                                                 
055000       MOVE ZERO TO ROWT-PROC                                             
055100     ELSE                                                                 
055200       COMPUTE ROWT-PROC ROUNDED = 100 * W-KURSUM-S / W-KURSUM-S          
055300     END-IF                                                               
055400     MOVE TOT-INKRES                 TO ROWT-INKRES                       
055500     WRITE W56052-S-REC FROM ROW-TOTAL                                    
055600     WRITE W56052-S-REC FROM FILLER-LINE                                  
055700     COMPUTE W-KURSUM2-S = W-KURSUM2-S + W-KURSUM-S                       
055800     MOVE ZERO TO W-KUR1-S                                                
055900     MOVE ZERO TO W-KUR2-S                                                
056000     MOVE ZERO TO W-KUR3-S                                                
056100     MOVE ZERO TO W-KUR4-S                                                
056200     MOVE ZERO TO W-KUR5-S                                                
056300     MOVE ZERO TO W-KUR6-S                                                
056400     MOVE ZERO TO W-KURSUM-S                                              
056500     MOVE ZERO TO TOT-INKRES                                              
056600     .                                                                    
056700     SKIP3                                                                
056800 BD-HEADER-KGR SECTION.                                                   
056900                                                                          
057000     MOVE IN-IDDC      TO RUB1-IDDC                                       
057100                                                                          
057200     MOVE SPACE TO W-IDLAND                                               
057300     SEARCH ALL DC-LAND                                                   
057400        AT END                                                            
057500           MOVE SPACE          TO W-IDLAND                                
057600        WHEN DCLAND-IDDC (DCLAND-IX) = IN-IDDC                            
057700           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
057800                               TO W-IDLAND                                
057900     END-SEARCH                                                           
058000                                                                          
058100     MOVE W-IDLAND        TO LAND-IDLANDX2                                
058200     MOVE SPACE           TO LAND-IDLANDX3                                
058300     CALL WISOLAND USING LAND-WISOLAND                                    
058400     IF LAND-KDSVAR = SPACE                                               
058500        MOVE LAND-KDVALISO(1)  TO RUB1-KDVALISO                           
058600     ELSE                                                                 
058700        MOVE SPACE             TO RUB1-KDVALISO                           
058800     END-IF                                                               
058900                                                                          
059000     WRITE W56052-K-REC FROM MAIN-LINE1                                   
059100     WRITE W56052-K-REC FROM FILLER-LINE                                  
059200     WRITE W56052-K-REC FROM ROW-LINE2                                    
059300     WRITE W56052-K-REC FROM FILLER-LINE                                  
059400     .                                                                    
059500                                                                          
059600                                                                          
059700 BE-COMPUTE-STOCKVALUE SECTION.                                           
059800     COMPUTE W-KUR1 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS1) +          
059900                               W-KUR1                                     
060000     COMPUTE W-KUR2 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS2) +          
060100                               W-KUR2                                     
060200     COMPUTE W-KUR3 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS3) +          
060300                               W-KUR3                                     
060400     COMPUTE W-KUR4 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS4) +          
060500                               W-KUR4                                     
060600     COMPUTE W-KUR5 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS5) +          
060700                               W-KUR5                                     
060800     COMPUTE W-KUR6 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS6) +          
060900                               W-KUR6                                     
061000     .                                                                    
061100     EJECT                                                                
061200                                                                          
061300 BF-WRITE-KGR-POST  SECTION.                                              
061400*TOTAL TURNOVERVALUE PER PRODUCT GROUP                                    
061500     MOVE WS-KDPSLLOC TO ROW2-PKOD                                        
061600     MOVE W-KUR1 TO ROW2-LVALUE1                                          
061700     MOVE W-KUR2 TO ROW2-LVALUE2                                          
061800     MOVE W-KUR3 TO ROW2-LVALUE3                                          
061900     MOVE W-KUR4 TO ROW2-LVALUE4                                          
062000     MOVE W-KUR5 TO ROW2-LVALUE5                                          
062100     MOVE W-KUR6 TO ROW2-LVALUE6                                          
062200     COMPUTE W-KURSUM = W-KUR1 + W-KUR2 + W-KUR3 + W-KUR4 + W-KUR5        
062300                      + W-KUR6                                            
062400     WRITE W56052-K-REC FROM ROW2                                         
062500     COMPUTE WS-KUR1 = WS-KUR1 + W-KUR1                                   
062600     COMPUTE WS-KUR2 = WS-KUR2 + W-KUR2                                   
062700     COMPUTE WS-KUR3 = WS-KUR3 + W-KUR3                                   
062800     COMPUTE WS-KUR4 = WS-KUR4 + W-KUR4                                   
062900     COMPUTE WS-KUR5 = WS-KUR5 + W-KUR5                                   
063000     COMPUTE WS-KUR6 = WS-KUR6 + W-KUR6                                   
063100     COMPUTE W-KURSUM2 = WS-KUR1 + WS-KUR2 + WS-KUR3 + WS-KUR4 +          
063200                         WS-KUR5 + WS-KUR6                                
063300     MOVE ZERO TO W-KUR1                                                  
063400     MOVE ZERO TO W-KUR2                                                  
063500     MOVE ZERO TO W-KUR3                                                  
063600     MOVE ZERO TO W-KUR4                                                  
063700     MOVE ZERO TO W-KUR5                                                  
063800     MOVE ZERO TO W-KUR6                                                  
063900     .                                                                    
064000     SKIP3                                                                
064100                                                                          
064200 BG-WRITE-TOTAL   SECTION.                                                
064300                                                                          
064400     MOVE WS-KUR1 TO ROW2T-LVALUE1                                        
064500     MOVE WS-KUR2 TO ROW2T-LVALUE2                                        
064600     MOVE WS-KUR3 TO ROW2T-LVALUE3                                        
064700     MOVE WS-KUR4 TO ROW2T-LVALUE4                                        
064800     MOVE WS-KUR5 TO ROW2T-LVALUE5                                        
064900     MOVE WS-KUR6 TO ROW2T-LVALUE6                                        
065000     WRITE W56052-K-REC FROM ROW2-TOTAL                                   
065100     WRITE W56052-K-REC FROM FILLER-LINE                                  
065200     .                                                                    
065300     SKIP3                                                                
065400                                                                          
065500 BH-COMPUTE-OBSOLITE   SECTION.                                           
065600                                                                          
065700     COMPUTE WO-KUR1   ROUNDED = WS-KUR1 * 0   / 100                      
065800     COMPUTE WO-KUR2   ROUNDED = WS-KUR2 * 0   / 100                      
065900     COMPUTE WO-KUR3   ROUNDED = WS-KUR3 * 18  / 100                      
066000     COMPUTE WO-KUR4   ROUNDED = WS-KUR4 * 53  / 100                      
066100     COMPUTE WO-KUR5   ROUNDED = WS-KUR5 * 81  / 100                      
066200     COMPUTE WO-KUR6   ROUNDED = WS-KUR6 * 100 / 100                      
066300     COMPUTE WO-KURSUM = WO-KUR1 + WO-KUR2 + WO-KUR3 + WO-KUR4 +          
066400                         WO-KUR5 + WO-KUR6                                
066500     MOVE WO-KUR1   TO ROW3T-LVALUE1                                      
066600     MOVE WO-KUR2   TO ROW3T-LVALUE2                                      
066700     MOVE WO-KUR3   TO ROW3T-LVALUE3                                      
066800     MOVE WO-KUR4   TO ROW3T-LVALUE4                                      
066900     MOVE WO-KUR5   TO ROW3T-LVALUE5                                      
067000     MOVE WO-KUR6   TO ROW3T-LVALUE6                                      
067100     MOVE WO-KURSUM TO ROW4T-LVALUE1                                      
067200                                                                          
067300     IF WO-KURSUM = ZERO                                                  
067400     OR WS-KURSUM = ZERO                                                  
067500       MOVE ZERO TO ROW4T-PROC                                            
067600     ELSE                                                                 
067700       COMPUTE ROW4T-PROC ROUNDED = 100 * WO-KURSUM / WS-KURSUM           
067800     END-IF                                                               
067900     .                                                                    
068000     EJECT                                                                
068100                                                                          
068200 BI-WRITE-SUPERTOTAL SECTION.                                             
068300                                                                          
068400     WRITE W56052-K-REC FROM ROW-OBSOL                                    
068500     WRITE W56052-K-REC FROM FILLER-LINE                                  
068600     WRITE W56052-K-REC FROM ROWT-OBSOL                                   
068700     WRITE W56052-K-REC FROM FILLER-LINE                                  
068800     MOVE W-KURSUM2 TO ROWS-LVALUE                                        
068900     WRITE W56052-K-REC FROM ROW-SUPERTOTAL                               
069000     .                                                                    
069100     EJECT                                                                
069200                                                                          
069300 Z-END    SECTION.                                                        
069400     CLOSE W56051                                                         
069500           W56052S W56052K                                                
069600                                                                          
069700     MOVE 'S'        TO POSTSUM-OPKOD                                     
069800     CALL POSTSUM USING POSTSUM-PARM                                      
069900     .                                                                    
070000     EJECT                                                                
070100                                                                          
070200 S01-READ-W56051-POST SECTION.                                            
070300     READ W56051 INTO INAREA                                              
070400     AT END                                                               
070500       MOVE JA TO EOF-W56051                                              
070600     NOT AT END                                                           
070700       MOVE W56051-TRANSID TO POSTSUM-TRANSID                             
070800       CALL POSTSUM USING POSTSUM-PARM                                    
070900     END-READ                                                             
071000     .                                                                    
071100     EJECT                                                                
071200 S02-SKRIV-DAP1-S SECTION.                                                
071300                                                                          
071400     MOVE ' ¤DAPW56052-001' TO W001-DAP                                   
071500     WRITE W56052-S-REC  FROM W001-DAP                                    
071600                                                                          
071700     MOVE SPACE TO W001-DAP                                               
071800     .                                                                    
071900                                                                          
072000 S03-SKRIV-DAP2-S SECTION.                                                
072100                                                                          
072200     STRING ' ¤DAP' IN-IDDC                                               
072300            DELIMITED BY SIZE INTO W001-DAP                               
072400     WRITE W56052-S-REC  FROM W001-DAP                                    
072500                                                                          
072600     MOVE SPACE TO W001-DAP                                               
072700     .                                                                    
072800 S04-SKRIV-DAP1-K SECTION.                                                
072900                                                                          
073000     MOVE ' ¤DAPW56052-002' TO W001-DAP                                   
073100     WRITE W56052-K-REC  FROM W001-DAP                                    
073200                                                                          
073300     MOVE SPACE TO W001-DAP                                               
073400     .                                                                    
073500 S05-SKRIV-DAP2-K SECTION.                                                
073600                                                                          
073700     STRING ' ¤DAP' IN-IDDC                                               
073800            DELIMITED BY SIZE INTO W001-DAP                               
073900     WRITE W56052-K-REC  FROM W001-DAP                                    
074000                                                                          
074100     MOVE SPACE TO W001-DAP                                               
074200     .                                                                    

000100 ID DIVISION.                                                             
000200     SKIP1                                                                
000300 PROGRAM-ID.    W4788600.                                                 
000400     SKIP1                                                                
000500*AUTHOR.        BERTIL HANSSON/ERIK KÅREBY.                               
000600*    SKIP1                                                                
000700*DATE-WRITTEN.  AUG 1979/ DEC 1981.                                       
000800*    SKIP1                                                                
000900*REMARKS.                                                                 
001000*        ORDERTRANS (W47884) MATCHAS MOT LAGERBAND (W01172).              
001100*        VID DIFFERANS BILDAS JUSTERINGSPOSTER PÅ FILEN W47886.           
001200*        DIFFERANSEN LISTAS ÄVEN I FELRAPPORT (W47886-001).               
001300*                                                                         
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP1                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800     SKIP1                                                                
001900 FILE-CONTROL.                                                            
002000     SELECT W47884       ASSIGN UT-S-W47886D1.                            
002100     SELECT W01172       ASSIGN UT-S-W47886D2.                            
002200     SELECT W47886       ASSIGN UT-S-W47886D3.                            
002300     SELECT W47886-001   ASSIGN UT-S-W47886D4.                            
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP1                                                                
002700 FILE SECTION.                                                            
002800     SKIP1                                                                
002900**************** L A G E R B A N D *******************************        
003000     SKIP1                                                                
003100 FD      W01172                                                           
003200         RECORDING F                                                      
003300         BLOCK 0                                                          
003400                              .                                           
003500     SKIP1                                                                
003600*01      LAG-POST -COPY W011100 -L.                                       
003700     SKIP2                                                                
003800**************** O R D E R T R A N S *****************************        
003900     SKIP1                                                                
004000 FD      W47884                                                           
004100         RECORDING F                                                      
004200         BLOCK 0                                                          
004300                              .                                           
004400     SKIP1                                                                
004500*01      TRA-POST -COPY W4788002 -L.                                      
004600     SKIP2                                                                
004700**************** J U S T E R I N G S T R A N S *******************        
004800     SKIP1                                                                
004900 FD      W47886                                                           
005000         RECORDING V                                                      
005100         BLOCK CONTAINS 0.                                                
005200                                                                          
005300     SKIP1                                                                
005400*01      JTR-POST -COPY W092R05T -L.                                      
005500     SKIP2                                                                
005600**************** R A D S K R I V A R F I L ***********************        
005700     SKIP2                                                                
005800 FD      W47886-001                                                       
005900         LABEL RECORD   STANDARD                                          
006000         RECORDING      V                                                 
006100         BLOCK CONTAINS 0.                                                
006200 01  W47886-001-LINE    PIC X(121).                                       
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500                                                                          
006600*    -- CHECKED BY WY2000                                                 
006700 77  IDPGM                   PIC X(8)       VALUE 'W4788600'.             
006800     SKIP2                                                                
006900 01      KONSTANTER.                                                      
007000     03  JA                  PIC X(1)       VALUE 'J'.                    
007100     03  NEJ                 PIC X(1)       VALUE 'N'.                    
007200     03  PLUS-TECKEN         PIC X(1)       VALUE '+'.                    
007300     03  MINUS-TECKEN        PIC X(1)       VALUE '-'.                    
007400     03  MINUS-1             PIC S9(1)      COMP-3 VALUE -1.              
007500     SKIP2                                                                
007600 01      SWITCHAR.                                                        
007700     03  TRANS-EOF-SW        PIC X(1)       VALUE 'N'.                    
007800      88 TRANS-EOF                          VALUE 'J'.                    
007900     03  LAGER-EOF-SW        PIC X(1)       VALUE 'N'.                    
008000      88 LAGER-EOF                          VALUE 'J'.                    
008100     03  LIKA-SW             PIC X(1)       VALUE 'N'.                    
008200      88 LIKA                               VALUE 'J'.                    
008300     SKIP2                                                                
008400*      --- VALID IDDC CODES                                               
008500*                                                                         
008600*01    -COPY WWDC99                                                       
008700       EJECT                                                              
008800 01  DAGENS-DATUM.                                                        
008900     03  DAGENS-DATUM-AR     PIC 9(2).                                    
009000     03  DAGENS-DATUM-MAN    PIC 9(2).                                    
009100     03  DAGENS-DATUM-DAG    PIC 9(2).                                    
009200     SKIP2                                                                
009300 01      DIVERSE-VARIABLER.                                               
009400     03  DV-KVLEVART         PIC S9(7)      COMP-3.                       
009500     03  DV-DIFF1            PIC S9(7)      COMP-3.                       
009600     03  DV-DIFF2            PIC S9(7)      COMP-3.                       
009700     03  DV-MELLAN-VAERDE    PIC 9(7).                                    
009800     03  DV-IDARTNR          PIC 9(9).                                    
009900     03  FILLER              REDEFINES DV-IDARTNR.                        
010000      05 FILLER              PIC 9(1).                                    
010100      05 DV-IDARTNR-RED      PIC 9(8).                                    
010200     SKIP2                                                                
010300     03  VAERDE              PIC S9(8)V99   COMP-3.                       
010400     03  VAERDE-SUM          PIC S9(9)V99   COMP-3.                       
010500     SKIP1                                                                
010600     EJECT                                                                
010700 01  W001-DAP.                                                            
010800     03  FILLER              PIC X(185)  VALUE SPACE.                     
010900**************** POSTOMRÅDE FÖR LAGERBAND ************************        
011000     SKIP1                                                                
011100*01      AREA -COPY W011100 -PRE LAG-                                     
011200     EJECT                                                                
011300**************** POSTOMRÅDE FÖR ORDERTRANS ***********************        
011400     SKIP1                                                                
011500*01      AREA -COPY W4788002 -PRE TRA-                                    
011600     EJECT                                                                
011700**************** POSTOMRÅDE FÖR JUSTERINGSTRANS ******************        
011800     SKIP1                                                                
011900*01      AREA -COPY W092R05T -PRE JTR-                                    
012000     EJECT                                                                
012100 01  R1-LINE                 PIC X(130) VALUE SPACE.                      
012200                                                                          
012300 01  R1-ENKELSKIP            PIC S9(5)      VALUE +1.                     
012400 01  R1-DUBBELSKIP           PIC S9(5)      VALUE +2.                     
012500 01  R1-TRIPPELSKIP          PIC S9(5)      VALUE +3.                     
012600 01  R1-FYRSKIP              PIC S9(5)      VALUE +4.                     
012700                                                                          
012800 01  R1-HELP-AREAS.                                                       
012900     03 R1-PAGECOUNTER       PIC S9(5) COMP-3 VALUE ZERO.                 
013000     03 R1-LINECOUNTER       PIC S9(5) COMP-3 VALUE +100.                 
013100     03 R1-LINE-SKIP         PIC S9(5) COMP-3 VALUE +100.                 
013200     03 R1-MAXLINES-PER-PAGE PIC  9(3)        VALUE 42.                   
013300                                                                          
013400     SKIP2                                                                
013500 01  RUBRIK2-HEADER.                                                      
013600      03 FILLER              PIC X(4)       VALUE SPACE.                  
013700      03 FILLER              PIC X(85)      VALUE                         
013800         'PART NUMBER;DC;EFRS OK;EFR MINSKAT;KVLS OK;KVLS MINSKAT;        
013900-        'STANDARD PRICE;VALUE;'.                                         
014000     SKIP2                                                                
014100 01  L1-LINE                 PIC X(130) VALUE SPACE.                      
014200 01  L1-DETALJRAD.                                                        
014300      03 FILLER              PIC X(5)   VALUE SPACE.                      
014400      03 LI-IDARTNR          PIC Z(9).                                    
014500      03 FILLER              PIC X(1)   VALUE ';'.                        
014600      03 LI-IDDC             PIC X(2).                                    
014700      03 FILLER              PIC X(1)   VALUE ';'.                        
014800      03 LI-KVEFRS-OEK       PIC Z(7).                                    
014900      03 FILLER              PIC X(1)   VALUE ';'.                        
015000      03 LI-KVEFRS-MIN       PIC Z(7).                                    
015100      03 FILLER              PIC X(1)   VALUE ';'.                        
015200      03 LI-KVLS-OEK         PIC Z(7).                                    
015300      03 FILLER              PIC X(1)   VALUE ';'.                        
015400      03 LI-KVLS-MIN         PIC Z(7).                                    
015500      03 FILLER              PIC X(1)   VALUE ';'.                        
015600      03 LI-PRARTSTD         PIC Z(8)9.99.                                
015700      03 FILLER              PIC X(1)   VALUE ';'.                        
015800      03 LI-VAERDE           PIC Z(8)9.99-.                               
015900      03 FILLER              PIC X(1)   VALUE ';'.                        
016000     SKIP2                                                                
016100 01  L2-LINE                 PIC X(130) VALUE SPACE.                      
016200 01  L2-DETALJRAD.                                                        
016300      03 FILLER              PIC X(1)   VALUE ';'.                        
016400      03 FILLER              PIC X(1)   VALUE ';'.                        
016500      03 FILLER              PIC X(1)   VALUE ';'.                        
016600      03 FILLER              PIC X(1)   VALUE ';'.                        
016700      03 FILLER              PIC X(1)   VALUE ';'.                        
016800      03 FILLER              PIC X(1)   VALUE ';'.                        
016900      03 FILLER              PIC X(1)   VALUE ';'.                        
017000      03 FILLER              PIC X(12)  VALUE 'TOTAL VALUE:'.             
017100      03 FILLER              PIC X(1)       VALUE SPACE.                  
017200      03 LI-VAERDE-SUM       PIC Z(9)9.99-.                               
017300      03 FILLER              PIC X(1)   VALUE ';'.                        
017400     EJECT                                                                
017500**************** P R O C E D U R E  D I V I S I O N **************        
017600     SKIP1                                                                
017700 PROCEDURE DIVISION.                                                      
017800     SKIP1                                                                
017900 STYR SECTION.                                                            
018000     SKIP1                                                                
018100     PERFORM A-INITIERING                                                 
018200     SKIP1                                                                
018300     PERFORM   B-LAES-LAGER-POST                                          
018400     PERFORM   C-LAES-TRANS-POST                                          
018500     MOVE TRA-IDDC     TO WS-IDDC                                         
018600     PERFORM UNTIL CDC-SE OR TRANS-EOF                                    
018700       PERFORM C-LAES-TRANS-POST                                          
018800       MOVE TRA-IDDC   TO WS-IDDC                                         
018900     END-PERFORM                                                          
019000     SKIP1                                                                
019100     PERFORM S25A-WRITE-DAP                                               
019200     PERFORM S25C-WRITE-DAP                                               
019300     PERFORM S21-WRITE-HEAD-LINES                                         
019400     PERFORM UNTIL LAGER-EOF OR TRANS-EOF                                 
019500       IF TRA-IDARTNR = LAG-IDARTNR                                       
019600         MOVE JA TO LIKA-SW                                               
019700         PERFORM D-KOLLA-VAERDEN                                          
019800         PERFORM B-LAES-LAGER-POST                                        
019900         PERFORM C-LAES-TRANS-POST                                        
020000         MOVE TRA-IDDC   TO WS-IDDC                                       
020100         PERFORM UNTIL TRA-IDDC = '11' OR TRANS-EOF                       
020200           PERFORM C-LAES-TRANS-POST                                      
020300           MOVE TRA-IDDC   TO WS-IDDC                                     
020400         END-PERFORM                                                      
020500       ELSE                                                               
020600         IF TRA-IDARTNR > LAG-IDARTNR                                     
020700           MOVE NEJ TO LIKA-SW                                            
020800           PERFORM D-KOLLA-VAERDEN                                        
020900           PERFORM B-LAES-LAGER-POST                                      
021000         ELSE                                                             
021100           PERFORM E-LAGERPOST-SAKNAS                                     
021200           PERFORM C-LAES-TRANS-POST                                      
021300           MOVE TRA-IDDC   TO WS-IDDC                                     
021400           PERFORM UNTIL TRA-IDDC = '11' OR TRANS-EOF                     
021500             PERFORM C-LAES-TRANS-POST                                    
021600             MOVE TRA-IDDC   TO WS-IDDC                                   
021700           END-PERFORM                                                    
021800         END-IF                                                           
021900       END-IF                                                             
022000     END-PERFORM                                                          
022100     SKIP2                                                                
022200     PERFORM F-AVSLUTA                                                    
022300     GOBACK                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 A-INITIERING SECTION.                                                    
022700     SKIP2                                                                
022800     OPEN INPUT  W01172                                                   
022900                 W47884                                                   
023000     OPEN OUTPUT W47886                                                   
023100                 W47886-001                                               
023200     SKIP2                                                                
023300     ACCEPT DAGENS-DATUM FROM DATE                                        
023400     SKIP2                                                                
023500     MOVE ZERO TO DV-DIFF1      DV-DIFF2                                  
023600                  LI-KVEFRS-OEK LI-KVEFRS-MIN                             
023700                  LI-KVLS-OEK   LI-KVLS-MIN                               
023800                  LI-VAERDE     LI-VAERDE-SUM                             
023900                  VAERDE        VAERDE-SUM                                
024000     .                                                                    
024100     EJECT                                                                
024200 B-LAES-LAGER-POST SECTION.                                               
024300     SKIP2                                                                
024400     READ W01172 INTO LAG-AREA AT END                                     
024500     MOVE +999999999 TO LAG-IDARTNR                                       
024600     MOVE JA TO LAGER-EOF-SW                                              
024700     END-READ                                                             
024800     .                                                                    
024900     SKIP3                                                                
025000 C-LAES-TRANS-POST SECTION.                                               
025100     SKIP2                                                                
025200     READ W47884 INTO TRA-AREA AT END                                     
025300     MOVE +999999999 TO TRA-IDARTNR                                       
025400     MOVE JA TO TRANS-EOF-SW                                              
025500     END-READ                                                             
025600     .                                                                    
025700     EJECT                                                                
025800 D-KOLLA-VAERDEN SECTION.                                                 
025900     SKIP2                                                                
026000     IF LIKA                                                              
026100*        TRANSPOST MATCHAR LAGERPOST                                      
026200       IF TRA-KVLEVART    NOT = LAG-KVEFRS                                
026300         PERFORM DA-DIFF-LEVART                                           
026400         PERFORM DC-SKRIV-LARM-RAD                                        
026500       END-IF                                                             
026600     ELSE                                                                 
026700       IF LAG-KVEFRS NOT = ZERO                                           
026800         MOVE TRA-KVLEVART TO DV-KVLEVART                                 
026900         MOVE ZERO TO TRA-KVLEVART                                        
027000         PERFORM  DA-DIFF-LEVART                                          
027100         MOVE DV-KVLEVART TO TRA-KVLEVART                                 
027200         PERFORM  DC-SKRIV-LARM-RAD                                       
027300       END-IF                                                             
027400     END-IF                                                               
027500     MOVE ZERO TO DV-DIFF1 DV-DIFF2                                       
027600     .                                                                    
027700     EJECT                                                                
027800 DA-DIFF-LEVART SECTION.                                                  
027900     SKIP2                                                                
028000     COMPUTE DV-DIFF1 = LAG-KVEFRS - TRA-KVLEVART                         
028100     IF DV-DIFF1 > ZERO                                                   
028200       MOVE DV-DIFF1 TO LI-KVEFRS-MIN LI-KVLS-OEK                         
028300       MOVE PLUS-TECKEN TO JTR-KDTECKEN-NYTT                              
028400     ELSE                                                                 
028500       MOVE DV-DIFF1 TO LI-KVEFRS-OEK LI-KVLS-MIN                         
028600       MOVE MINUS-TECKEN TO JTR-KDTECKEN-NYTT                             
028700     END-IF                                                               
028800     SKIP1                                                                
028900     MOVE 'KVLS' TO JTR-IDELMT                                            
029000     MOVE  DV-DIFF1 TO DV-MELLAN-VAERDE                                   
029100     PERFORM S01-SKRIV-JUSTTRANS                                          
029200     SKIP1                                                                
029300     MOVE 'KVEFRS' TO JTR-IDELMT                                          
029400     IF JTR-KDTECKEN-NYTT = PLUS-TECKEN                                   
029500       MOVE MINUS-TECKEN TO JTR-KDTECKEN-NYTT                             
029600     ELSE                                                                 
029700       MOVE PLUS-TECKEN TO JTR-KDTECKEN-NYTT                              
029800     END-IF                                                               
029900     SUBTRACT DV-DIFF1 FROM ZERO GIVING DV-MELLAN-VAERDE                  
030000     PERFORM S01-SKRIV-JUSTTRANS                                          
030100     .                                                                    
030200     EJECT                                                                
030300 DC-SKRIV-LARM-RAD SECTION.                                               
030400     SKIP2                                                                
030500     IF DV-DIFF1 NOT = ZERO                                               
030600       COMPUTE VAERDE     = MINUS-1 * DV-DIFF1 * LAG-PRARTSTD             
030700       COMPUTE VAERDE-SUM = VAERDE-SUM + VAERDE                           
030800       MOVE LAG-PRARTSTD          TO LI-PRARTSTD                          
030900       MOVE VAERDE                TO LI-VAERDE                            
031000     END-IF                                                               
031100     MOVE LAG-IDARTNR           TO LI-IDARTNR                             
031200     MOVE TRA-IDDC              TO LI-IDDC                                
031300     PERFORM S20-WRITE-W47886-001                                         
031400     MOVE ZERO TO LI-KVEFRS-OEK LI-KVEFRS-MIN                             
031500                  LI-KVLS-OEK   LI-KVLS-MIN                               
031600                  LI-VAERDE                                               
031700     .                                                                    
031800     EJECT                                                                
031900 E-LAGERPOST-SAKNAS SECTION.                                              
032000     SKIP2                                                                
032100     DISPLAY '***** POST SAKNAS PÅ LAGERBANDET: ART.NR '                  
032200     TRA-IDARTNR '    LEVART ' TRA-KVLEVART ' *****'                      
032300     .                                                                    
032400     SKIP3                                                                
032500 F-AVSLUTA SECTION.                                                       
032600     SKIP2                                                                
032700     IF VAERDE-SUM NOT = ZERO                                             
032800       MOVE VAERDE-SUM            TO LI-VAERDE-SUM                        
032900**     MOVE R1-DUBBELSKIP TO R1-LINE-SKIP                                 
033000       MOVE L2-DETALJRAD TO L1-LINE                                       
033100       PERFORM S26-WRITE-W47886-001-DETAIL                                
033200     END-IF                                                               
033300     CLOSE W01172                                                         
033400           W47884                                                         
033500           W47886                                                         
033600     .                                                                    
033700     EJECT                                                                
033800 S01-SKRIV-JUSTTRANS SECTION.                                             
033900     SKIP2                                                                
034000      MOVE 'R05'            TO JTR-IDPTYP                                 
034100      MOVE LAG-IDARTNR      TO DV-IDARTNR                                 
034200      MOVE DV-IDARTNR-RED   TO JTR-IDARTNR                                
034300      MOVE 1                TO JTR-KDCLAGER                               
034400      MOVE DV-MELLAN-VAERDE TO JTR-IDFVARDE-NYTT                          
034500      MOVE SPACE            TO JTR-KDTECKEN-BEF                           
034600                               JTR-IDFVARDE-BEF                           
034700     WRITE JTR-POST FROM JTR-AREA                                         
034800     SKIP3                                                                
034900     .                                                                    
035000 S20-WRITE-W47886-001 SECTION.                                            
035100     MOVE L1-DETALJRAD  TO L1-LINE                                        
035200     PERFORM S26-WRITE-W47886-001-DETAIL                                  
035300     SKIP3                                                                
035400     .                                                                    
035500 S21-WRITE-HEAD-LINES SECTION.                                            
035600     SKIP2                                                                
035700     MOVE RUBRIK2-HEADER TO R1-LINE                                       
035800     PERFORM S25B-WRITE-W47886-001                                        
035900     EJECT                                                                
036000     .                                                                    
036100 S25A-WRITE-DAP SECTION.                                                  
036200                                                                          
036300     MOVE '¤DAPW47886-001' TO W001-DAP                                    
036400     WRITE W47886-001-LINE  FROM W001-DAP                                 
036500                                                                          
036600     MOVE SPACE TO W001-DAP                                               
036700     .                                                                    
036800 S25C-WRITE-DAP SECTION.                                                  
036900                                                                          
037000     MOVE '¤DAPW47886' TO W001-DAP                                        
037100     WRITE W47886-001-LINE   FROM W001-DAP                                
037200                                                                          
037300     MOVE SPACE TO W001-DAP                                               
037400     .                                                                    
037500                                                                          
037600 S25B-WRITE-W47886-001 SECTION.                                           
037700     SKIP2                                                                
037800     WRITE W47886-001-LINE FROM R1-LINE                                   
037900     SKIP3                                                                
038000     .                                                                    
038100 S26-WRITE-W47886-001-DETAIL SECTION.                                     
038200     SKIP2                                                                
038300     WRITE W47886-001-LINE FROM L1-LINE                                   
038400*    AFTER ADVANCING R1-LINE-SKIP                                         
038500     .                                                                    

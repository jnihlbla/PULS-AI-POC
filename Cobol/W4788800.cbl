000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.    W4788800.                                                 
000400                                                                          
000500*AUTHOR.        BERTIL HANSSON/ERIK KÅREBY/MARGARETA G.                   
000600*                                                                         
000700*DATE-WRITTEN.  AUG 1979/ DEC 1981        /JAN 1995.                      
000800*                                                                         
000900*REMARKS.                                                                 
001000*        PROGRAM FÖR ATT SKAPA EFR-DIFFLISTA FÖR SDC:ERNA.                
001100*        ORDERTRANS (W47884) MATCHAS MOT LAGERBAND (W01184).              
001200*        VID DIFFERANS BILDAS JUSTERINGSPOSTER PÅ FILEN W47888.           
001300*        DIFFERANSEN LISTAS ÄVEN I FELRAPPORT (W47888-001).               
001400*                                                                         
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SELECT W47884       ASSIGN UT-S-W47888D1.                            
002200     SELECT W01184       ASSIGN UT-S-W47888D2.                            
002300     SELECT W47888       ASSIGN UT-S-W47888D3.                            
002400     SELECT W47888-001   ASSIGN UT-S-W47888D4.                            
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700                                                                          
002800 FILE SECTION.                                                            
002900                                                                          
003000**************** O R D E R T R A N S *****************************        
003100                                                                          
003200 FD      W47884                                                           
003300         RECORDING F                                                      
003400         BLOCK 0                                                          
003500                              .                                           
003600                                                                          
003700*01      TRA-POST -COPY W4788002 -L.                                      
003800     SKIP2                                                                
003900**************** L A G E R B A N D *******************************        
004000                                                                          
004100 FD      W01184                                                           
004200         RECORDING F                                                      
004300         BLOCK 0                                                          
004400                              .                                           
004500                                                                          
004600*01      LAG-POST -COPY W01184  -L.                                       
004700     SKIP2                                                                
004800**************** J U S T E R I N G S T R A N S *******************        
004900                                                                          
005000 FD      W47888                                                           
005100         RECORDING F                                                      
005200         BLOCK CONTAINS 0.                                                
005300                                                                          
005400                                                                          
005500*01      JTR-POST -COPY W47888   -L.                                      
005600     SKIP2                                                                
005700**************** R A D S K R I V A R F I L ***********************        
005800     SKIP2                                                                
005900 FD      W47888-001                                                       
006000         LABEL RECORD   STANDARD                                          
006100         RECORDING      V                                                 
006200         BLOCK CONTAINS 0.                                                
006300 01  W47888-001-LINE    PIC X(131).                                       
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600                                                                          
006700*    -- CHECKED BY WY2000                                                 
006800 77  IDPGM                   PIC X(8)       VALUE 'W4788800'.             
006900     SKIP2                                                                
007000 01      KONSTANTER.                                                      
007100     03  JA                  PIC X(1)       VALUE 'J'.                    
007200     03  NEJ                 PIC X(1)       VALUE 'N'.                    
007300     03  PLUS-TECKEN         PIC X(1)       VALUE '+'.                    
007400     03  MINUS-TECKEN        PIC X(1)       VALUE '-'.                    
007500     03  MINUS-1             PIC S9(1)      COMP-3 VALUE -1.              
007600     SKIP2                                                                
007700 01      SWITCHAR.                                                        
007800     03  TRANS-EOF-SW        PIC X(1)       VALUE 'N'.                    
007900      88 TRANS-EOF                          VALUE 'J'.                    
008000     03  LAGER-EOF-SW        PIC X(1)       VALUE 'N'.                    
008100      88 LAGER-EOF                          VALUE 'J'.                    
008200     03  LIKA-SW             PIC X(1)       VALUE 'N'.                    
008300      88 LIKA                               VALUE 'J'.                    
008400     03  NEW-POST-SW         PIC X(1)       VALUE 'N'.                    
008500     SKIP2                                                                
008600 01  DAGENS-DATUM.                                                        
008700     03  DAGENS-DATUM-AR     PIC 9(2).                                    
008800     03  DAGENS-DATUM-MAN    PIC 9(2).                                    
008900     03  DAGENS-DATUM-DAG    PIC 9(2).                                    
009000     SKIP2                                                                
009100 01      DIVERSE-VARIABLER.                                               
009200     03  DV-KVLEVART         PIC S9(7)      COMP-3.                       
009300     03  DV-DIFF1            PIC S9(7)      COMP-3.                       
009400     03  DV-DIFF2            PIC S9(7)      COMP-3.                       
009500     03  DV-MELLAN-VAERDE    PIC 9(7).                                    
009600     03  DV-IDARTNR          PIC 9(9).                                    
009700     03  FILLER              REDEFINES DV-IDARTNR.                        
009800      05 FILLER              PIC 9(1).                                    
009900      05 DV-IDARTNR-RED      PIC 9(8).                                    
010000     SKIP2                                                                
010100     03  VAERDE              PIC S9(8)V99   COMP-3.                       
010200     03  VAERDE-SUM          PIC S9(9)V99   COMP-3.                       
010300     03  WS-TRA-IDKUNDNR     PIC 9(7).                                    
010400     03  WS-IDFAKT           PIC 9(7).                                    
010500                                                                          
010600     EJECT                                                                
010700 01  W001-DAP.                                                            
010800     03  FILLER              PIC X(185)  VALUE SPACE.                     
010900**************** POSTOMRÅDE FÖR LAGERBAND ************************        
011000                                                                          
011100*01      -COPY W01184.                                                    
011200     EJECT                                                                
011300**************** POSTOMRÅDE FÖR ORDERTRANS ***********************        
011400                                                                          
011500*01      AREA -COPY W4788002 -PRE TRA-                                    
011600     EJECT                                                                
011700**************** POSTOMRÅDE FÖR JUSTERINGSTRANS ******************        
011800                                                                          
011900*01      AREA -COPY W47888   -PRE JTR-                                    
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
013400 01  RUBRIK2-HEADER.                                                      
013500      03 FILLER              PIC X(199)    VALUE                          
013600         'PART NUMBER;DC;EFRS OK;EFR MINSKAT;KVLS OK;KVLS MINSKAT;        
013700-        'DISTRICT;CUSTOMER-ID;ORDER-IDENTITY;PACKING DOC;DELPAC;         
013800-        'INVOICE NUMBER;'.                                               
013900     SKIP2                                                                
014000 01  L1-LINE                 PIC X(150) VALUE SPACE.                      
014100 01  L1-DETALJRAD.                                                        
014200      03 LI-IDARTNR          PIC Z(9).                                    
014300      03 FILLER              PIC X(1)   VALUE ';'.                        
014400      03 LI-IDDC             PIC X(2).                                    
014500      03 FILLER              PIC X(1)   VALUE ';'.                        
014600      03 LI-KVEFRS-OEK       PIC Z(7).                                    
014700      03 FILLER              PIC X(1)   VALUE ';'.                        
014800      03 LI-KVEFRS-MIN       PIC Z(7).                                    
014900      03 FILLER              PIC X(1)   VALUE ';'.                        
015000      03 LI-KVLS-OEK         PIC Z(7).                                    
015100      03 FILLER              PIC X(1)   VALUE ';'.                        
015200      03 LI-KVLS-MIN         PIC Z(7).                                    
015300      03 FILLER              PIC X(1)   VALUE ';'.                        
015400      03 LI-IDDISTR          PIC Z(5).                                    
015500      03 FILLER              PIC X(1)   VALUE ';'.                        
015600      03 LI-IDKUNDNR         PIC X(6).                                    
015700      03 FILLER              PIC X(1)   VALUE ';'.                        
015800      03 LI-IDKUNDRF         PIC X(5).                                    
015900      03 FILLER              PIC X(1)   VALUE ';'.                        
016000      03 LI-IDPURAD          PIC Z(5).                                    
016100      03 FILLER              PIC X(1)   VALUE ';'.                        
016200      03 LI-FLDELPAC         PIC X(1).                                    
016300      03 FILLER              PIC X(1)   VALUE ';'.                        
016400      03 LI-IDFAKT           PIC X(5).                                    
016500      03 FILLER              PIC X(1)   VALUE ';'.                        
016600     SKIP2                                                                
016700 01  L2-LINE                 PIC X(130) VALUE SPACE.                      
016800 01  L2-DETALJRAD.                                                        
016900      03 FILLER              PIC X(1)   VALUE ';'.                        
017000      03 FILLER              PIC X(1)   VALUE ';'.                        
017100      03 FILLER              PIC X(1)   VALUE ';'.                        
017200      03 FILLER              PIC X(1)   VALUE ';'.                        
017300      03 FILLER              PIC X(1)   VALUE ';'.                        
017400      03 FILLER              PIC X(1)   VALUE ';'.                        
017500      03 FILLER              PIC X(1)   VALUE ';'.                        
017600      03 FILLER              PIC X(1)   VALUE ';'.                        
017700      03 FILLER              PIC X(1)   VALUE ';'.                        
017800      03 FILLER              PIC X(1)   VALUE ';'.                        
017900      03 FILLER              PIC X(1)   VALUE ';'.                        
018000      03 FILLER              PIC X(12)      VALUE 'TOTAL VALUE:'.         
018100      03 FILLER              PIC X(1)   VALUE SPACE.                      
018200      03 LI-VAERDE-SUM       PIC Z(9)9.99-.                               
018300      03 FILLER              PIC X(1)   VALUE ';'.                        
018400     EJECT                                                                
018500**************** P R O C E D U R E  D I V I S I O N **************        
018600                                                                          
018700 PROCEDURE DIVISION.                                                      
018800                                                                          
018900 STYR SECTION.                                                            
019000                                                                          
019100     PERFORM A-INITIERING                                                 
019200                                                                          
019300     PERFORM   B-LAES-LAGER-POST                                          
019400     PERFORM   C-LAES-TRANS-POST                                          
019500                                                                          
019600     PERFORM S25A-WRITE-DAP                                               
019700     PERFORM S25C-WRITE-DAP                                               
019800     PERFORM S21-WRITE-HEAD-LINES                                         
019900     PERFORM UNTIL LAGER-EOF AND TRANS-EOF                                
020000       IF TRA-IDARTNR = SLAG-IDARTNR                                      
020100         AND TRA-IDDC = SLAG-IDDC                                         
020200         MOVE JA TO LIKA-SW                                               
020300         PERFORM D-KOLLA-VAERDEN                                          
020400         PERFORM B-LAES-LAGER-POST                                        
020500         PERFORM C-LAES-TRANS-POST                                        
020600       ELSE                                                               
020700         IF TRA-IDARTNR > SLAG-IDARTNR                                    
020800         OR (TRA-IDARTNR = SLAG-IDARTNR                                   
020900         AND TRA-IDDC > SLAG-IDDC)                                        
021000           MOVE NEJ TO LIKA-SW                                            
021100           PERFORM D-KOLLA-VAERDEN                                        
021200           PERFORM B-LAES-LAGER-POST                                      
021300         ELSE                                                             
021400           PERFORM C-LAES-TRANS-POST                                      
021500         END-IF                                                           
021600       END-IF                                                             
021700     END-PERFORM                                                          
021800                                                                          
021900     PERFORM F-AVSLUTA                                                    
022000     GOBACK                                                               
022100     .                                                                    
022200     EJECT                                                                
022300 A-INITIERING SECTION.                                                    
022400     SKIP2                                                                
022500     OPEN INPUT  W01184                                                   
022600                 W47884                                                   
022700     OPEN OUTPUT W47888                                                   
022800                 W47888-001                                               
022900     SKIP2                                                                
023000     ACCEPT DAGENS-DATUM FROM DATE                                        
023100     SKIP2                                                                
023200     MOVE ZERO TO DV-DIFF1      DV-DIFF2                                  
023300                  LI-KVEFRS-OEK LI-KVEFRS-MIN                             
023400                  LI-KVLS-OEK   LI-KVLS-MIN                               
023500*                 LI-VAERDE     LI-VAERDE-SUM                             
023600                  LI-IDDISTR                                              
023700                  LI-IDKUNDNR                                             
023800                  LI-IDKUNDRF                                             
023900                  LI-IDPURAD                                              
024000                  LI-FLDELPAC                                             
024100                  LI-IDFAKT                                               
024200                  LI-VAERDE-SUM                                           
024300                  VAERDE        VAERDE-SUM                                
024400     .                                                                    
024500     EJECT                                                                
024600 B-LAES-LAGER-POST SECTION.                                               
024700     SKIP2                                                                
024800     READ W01184 INTO SLAG-W01184 AT END                                  
024900     MOVE +999999999 TO SLAG-IDARTNR                                      
025000     MOVE JA TO LAGER-EOF-SW                                              
025100     END-READ                                                             
025200     .                                                                    
025300     SKIP3                                                                
025400 C-LAES-TRANS-POST SECTION.                                               
025500     SKIP2                                                                
025600     READ W47884 INTO TRA-AREA                                            
025700     AT END                                                               
025800       MOVE +999999999 TO TRA-IDARTNR                                     
025900       MOVE JA TO TRANS-EOF-SW                                            
026000       MOVE ZERO           TO LI-IDDISTR                                  
026100       MOVE ZERO           TO LI-IDKUNDNR                                 
026200       MOVE ZERO           TO LI-IDKUNDRF                                 
026300       MOVE ZERO           TO LI-IDPURAD                                  
026400       MOVE ZERO           TO LI-FLDELPAC                                 
026500       MOVE ZERO           TO LI-IDFAKT                                   
026600     NOT AT END                                                           
026700       MOVE JA             TO NEW-POST-SW                                 
026800     END-READ                                                             
026900     .                                                                    
027000     EJECT                                                                
027100 D-KOLLA-VAERDEN SECTION.                                                 
027200     SKIP2                                                                
027300     IF LIKA                                                              
027400*        TRANSPOST MATCHAR LAGERPOST                                      
027500       IF TRA-KVLEVART    NOT = SLAG-KVEFRS                               
027600         PERFORM DD-MOVE-TRA-DATA                                         
027700         PERFORM DA-DIFF-LEVART                                           
027800         PERFORM DC-SKRIV-LARM-RAD                                        
027900       END-IF                                                             
028000     ELSE                                                                 
028100       IF SLAG-KVEFRS NOT = ZERO                                          
028200         MOVE TRA-KVLEVART TO DV-KVLEVART                                 
028300         MOVE ZERO TO TRA-KVLEVART                                        
028400         PERFORM DD-MOVE-TRA-DATA                                         
028500         PERFORM  DA-DIFF-LEVART                                          
028600         MOVE DV-KVLEVART TO TRA-KVLEVART                                 
028700         PERFORM  DC-SKRIV-LARM-RAD                                       
028800       END-IF                                                             
028900     END-IF                                                               
029000     MOVE ZERO TO DV-DIFF1 DV-DIFF2                                       
029100     .                                                                    
029200     EJECT                                                                
029300 DD-MOVE-TRA-DATA SECTION.                                                
029400                                                                          
029500     IF NEW-POST-SW = 'J'                                                 
029600       MOVE NEJ                 TO NEW-POST-SW                            
029700       MOVE TRA-IDDISTR         TO LI-IDDISTR                             
029800       MOVE TRA-IDKUNDNR        TO WS-TRA-IDKUNDNR                        
029900       MOVE WS-TRA-IDKUNDNR     TO LI-IDKUNDNR                            
030000       MOVE TRA-IDKUNDRF(1:5)   TO LI-IDKUNDRF                            
030100       MOVE TRA-IDPURAD         TO LI-IDPURAD                             
030200       MOVE TRA-FLDELPAC        TO LI-FLDELPAC                            
030300       MOVE TRA-IDFAKT          TO WS-IDFAKT                              
030400       MOVE WS-IDFAKT           TO LI-IDFAKT                              
030500     ELSE                                                                 
030600       MOVE ZERO TO LI-IDDISTR                                            
030700       MOVE ZERO TO LI-IDKUNDNR                                           
030800       MOVE ZERO TO LI-IDKUNDRF                                           
030900       MOVE ZERO TO LI-IDPURAD                                            
031000     END-IF                                                               
031100     INSPECT LI-IDKUNDNR REPLACING LEADING ZEROES BY SPACE                
031200     IF LI-IDKUNDNR = SPACE                                               
031300       MOVE '     0'     TO LI-IDKUNDNR                                   
031400     END-IF                                                               
031500     INSPECT LI-IDFAKT   REPLACING LEADING ZEROES BY SPACE                
031600     INSPECT LI-FLDELPAC REPLACING LEADING ZEROES BY SPACE                
031700     INSPECT LI-IDKUNDRF REPLACING LEADING ZEROES BY SPACE                
031800     .                                                                    
031900     EJECT                                                                
032000 DA-DIFF-LEVART SECTION.                                                  
032100     SKIP2                                                                
032200     COMPUTE DV-DIFF1 = SLAG-KVEFRS - TRA-KVLEVART                        
032300     IF DV-DIFF1 > ZERO                                                   
032400       MOVE DV-DIFF1 TO LI-KVEFRS-MIN LI-KVLS-OEK                         
032500       MOVE PLUS-TECKEN  TO JTR-KDTECKEN-LS                               
032600       MOVE MINUS-TECKEN TO JTR-KDTECKEN-EFRS                             
032700     ELSE                                                                 
032800       MOVE DV-DIFF1 TO LI-KVEFRS-OEK LI-KVLS-MIN                         
032900       MOVE MINUS-TECKEN TO JTR-KDTECKEN-LS                               
033000       MOVE PLUS-TECKEN  TO JTR-KDTECKEN-EFRS                             
033100     END-IF                                                               
033200                                                                          
033300     MOVE  DV-DIFF1 TO DV-MELLAN-VAERDE                                   
033400     PERFORM S01-SKRIV-JUSTTRANS                                          
033500                                                                          
033600     .                                                                    
033700     EJECT                                                                
033800 DC-SKRIV-LARM-RAD SECTION.                                               
033900     SKIP2                                                                
034000*    MOVE ZERO                  TO LI-PRARTSTD                            
034100*    MOVE ZERO                  TO LI-VAERDE                              
034200     MOVE SLAG-IDARTNR          TO LI-IDARTNR                             
034300     MOVE SLAG-IDDC             TO LI-IDDC                                
034400                                                                          
034500     PERFORM S20-WRITE-W47888-001                                         
034600                                                                          
034700     MOVE ZERO TO LI-KVEFRS-OEK LI-KVEFRS-MIN                             
034800                  LI-KVLS-OEK   LI-KVLS-MIN                               
034900     MOVE ZERO TO LI-IDDISTR                                              
035000     MOVE ZERO TO LI-IDKUNDNR                                             
035100     MOVE ZERO TO LI-IDKUNDRF                                             
035200     MOVE ZERO TO LI-IDPURAD                                              
035300     MOVE ZERO TO LI-FLDELPAC                                             
035400     MOVE ZERO TO LI-IDFAKT                                               
035500*                 LI-VAERDE                                               
035600     .                                                                    
035700     EJECT                                                                
035800 F-AVSLUTA SECTION.                                                       
035900     SKIP2                                                                
036000     IF VAERDE-SUM > ZERO                                                 
036100       MOVE VAERDE-SUM            TO LI-VAERDE-SUM                        
036200       MOVE R1-DUBBELSKIP TO R1-LINE-SKIP                                 
036300       MOVE L2-DETALJRAD TO L1-LINE                                       
036400       PERFORM S26-WRITE-W47888-001-DETAIL                                
036500     END-IF                                                               
036600     CLOSE W01184                                                         
036700           W47884                                                         
036800           W47888                                                         
036900     .                                                                    
037000     EJECT                                                                
037100 S01-SKRIV-JUSTTRANS SECTION.                                             
037200     SKIP2                                                                
037300     MOVE SLAG-IDARTNR     TO JTR-IDARTNR                                 
037400     MOVE SLAG-IDDC        TO JTR-IDDC                                    
037500     MOVE DV-MELLAN-VAERDE TO JTR-KVEFRS                                  
037600                              JTR-KVLS                                    
037700     WRITE JTR-POST FROM JTR-AREA                                         
037800     SKIP3                                                                
037900     .                                                                    
038000 S20-WRITE-W47888-001 SECTION.                                            
038100     MOVE L1-DETALJRAD  TO L1-LINE                                        
038200     PERFORM S26-WRITE-W47888-001-DETAIL                                  
038300                                                                          
038400     SKIP3                                                                
038500     .                                                                    
038600 S21-WRITE-HEAD-LINES SECTION.                                            
038700     SKIP2                                                                
038800     MOVE RUBRIK2-HEADER TO R1-LINE                                       
038900     PERFORM S25B-WRITE-W47888-001                                        
039000                                                                          
039100     EJECT                                                                
039200     .                                                                    
039300 S25A-WRITE-DAP SECTION.                                                  
039400     SKIP2                                                                
039500                                                                          
039600     MOVE '¤DAPW47888-001' TO W001-DAP                                    
039700     WRITE W47888-001-LINE   FROM W001-DAP                                
039800                                                                          
039900     MOVE SPACE TO W001-DAP                                               
040000     .                                                                    
040100 S25C-WRITE-DAP SECTION.                                                  
040200     SKIP2                                                                
040300                                                                          
040400     MOVE '¤DAPW47888' TO W001-DAP                                        
040500     WRITE W47888-001-LINE   FROM W001-DAP                                
040600                                                                          
040700     MOVE SPACE TO W001-DAP                                               
040800     .                                                                    
040900 S25B-WRITE-W47888-001 SECTION.                                           
041000     SKIP2                                                                
041100                                                                          
041200     WRITE W47888-001-LINE FROM R1-LINE                                   
041300     SKIP3                                                                
041400     .                                                                    
041500 S26-WRITE-W47888-001-DETAIL SECTION.                                     
041600     SKIP2                                                                
041700                                                                          
041800     WRITE W47888-001-LINE FROM L1-LINE                                   
041900     .                                                                    

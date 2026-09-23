000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5103000.                                                
000400 AUTHOR.         KARL JOHAN HANSSON.                                      
000500 DATE-WRITTEN.   96/11/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION: AV FAKTURERINGENS TRANSAR AV TYP 95X O 94X SKAPAS          
000900*              TRANSAR TILL LAB AV TYP A01,A02,A07,A09,A15,L02,L09        
001000                                                                          
001100     SKIP3                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*          --- TRANSAR FRÅN FAKTURERINGEN TYP 95X OCH 94X                 
001900     SELECT W4758C                     ASSIGN TO W51030D1.                
002000*          --- ARTIKELREGISTEREXTRAKT                                     
002100     SELECT W01160                     ASSIGN TO W51030D2.                
002200                                                                          
002300*          --- LAB-TRANSAR TYP A01,AX2,L02,A07,A09 L09 A15                
002400     SELECT W51031                     ASSIGN TO W51030D3.                
002500*                                                                         
002600     SELECT SORTFIL                    ASSIGN TO W51030DS.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W4758C                                                               
003300     RECORDING       V                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  -COPY W51095X      -L.                                               
003700                                                                          
003800*01  -COPY W51094X      -L.                                               
003900     SKIP3                                                                
004000 FD  W01160                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W01160       -L.                                               
004500     EJECT                                                                
004600 FD  W51031                                                               
004700     RECORDING       V                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W510A01 -PRE  UA01-  -L.                                  
005100                                                                          
005200*01  POST -COPY W510AX2 -PRE  UAX2-  -L.                                  
005300                                                                          
005400*01  POST -COPY W510A07 -PRE  UA07-  -L.                                  
005500                                                                          
005600*01  POST -COPY W510A09 -PRE  UA09-  -L.                                  
005700                                                                          
005800*01  POST -COPY W510L09 -PRE  UL09-  -L.                                  
005900                                                                          
006000*01  POST -COPY W510A15 -PRE  UA15-  -L.                                  
006100     SKIP3                                                                
006200 SD  SORTFIL.                                                             
006300                                                                          
006400 01  SRT-POST.                                                            
006500*    03  -COPY W51095X  -PRE SRT-                                         
006600     03  SRT-KDPRODSL     PIC 9(3)      COMP-3.                           
006700     03  SRT-KDPSLLOC     PIC 9(2).                                       
006800 WORKING-STORAGE SECTION.                                                 
006900                                                                          
007000*    -- CHECKED BY WY2000                                                 
007100 77  IDPGM                       PIC X(8)    VALUE 'W5103000'.            
007200 77  JA                          PIC X       VALUE 'J'.                   
007300 77  NEJ                         PIC X       VALUE 'N'.                   
007400                                                                          
007500 77  INFIL-EOF-SW                PIC X         VALUE 'N'.                 
007600     88  END-OF-INFIL                          VALUE 'J'.                 
007700                                                                          
007800 77  LBFIL-EOF-SW                PIC X         VALUE 'N'.                 
007900     88  END-OF-LBFIL                          VALUE 'J'.                 
008000                                                                          
008100 77  SORTFIL-EOF-SW              PIC X         VALUE 'N'.                 
008200     88  END-OF-SORTFIL                        VALUE 'J'.                 
008300                                                                          
008400 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
008500                                                                          
008600 01  W95X-SPAR-AREOR.                                                     
008700     03  W95X-FLOVRLEV           PIC X         VALUE SPACE.               
008800     03  W95X-IDRAPPNR           PIC 9(7)      VALUE ZERO.                
008900     03  W95X-DAFAKT             PIC 9(9)      VALUE ZERO  COMP-3.        
009000                                                                          
009100 01  FELTEXT.                                                             
009200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009400                                                                          
009500 01  DYNAMISKA-SUBPROGRAM.                                                
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009800                                                                          
009900*    --- PARAMETRAR TILL ABEND                                            
010000                                                                          
010100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010200     EJECT                                                                
010300 01  TEST-IDDISTR        PIC 9(5)        COMP-3.                          
010400                                                                          
010500*01  FILLER  -COPY WWDIST07  -RED TEST-IDDISTR.                           
010600                                                                          
010700*01  FILLER  -COPY WWDIST18  -RED TEST-IDDISTR.                           
010800                                                                          
010900*01  FILLER  -COPY WWDIST20  -RED TEST-IDDISTR.                           
011000                                                                          
011100*01  FILLER  -COPY WWDIST35  -RED TEST-IDDISTR.                           
011200                                                                          
011300*01  FILLER  -COPY WWDIST53  -RED TEST-IDDISTR.                           
011400                                                                          
011500*01  FILLER  -COPY WWDIST55  -RED TEST-IDDISTR.                           
011600                                                                          
011700*01  FILLER  -COPY WWDIST70  -RED TEST-IDDISTR.                           
011800                                                                          
011900*01  FILLER  -COPY WWDIS134  -RED TEST-IDDISTR.                           
012000                                                                          
012100*01  FILLER  -COPY WWDIST57                                               
012200     EJECT                                                                
012300*    -- VALID IDDC CODES                                                  
012400*01  FILLER  -COPY WWDC99                                                 
012500     EJECT                                                                
012600*    -- VALID DC CONSTANTS                                                
012610*01  FILLER  -COPY WWDCKONS                                               
012620     EJECT                                                                
012700 01  FILLER                      PIC X(16)   VALUE 'POSTSUM '.            
012800*                                                                         
012900*01  -COPY W0005   -PRE  POSTSUM-                                         
013000     EJECT                                                                
013100 01  FILLER                      PIC X(24)   VALUE 'SORT-AREA'.           
013200                                                                          
013300 01  SORT-AREA.                                                           
013400     03  95X-AREA.                                                        
013500*        05  -COPY W51095X  -PRE 95X-                                     
013600         05  SORT-KDPRODSL    PIC 9(3)      COMP-3.                       
013700         05  SORT-KDPSLLOC    PIC 9(2).                                   
013800                                                                          
013900*    03  AREA -COPY W51094X  -PRE 94X-  -RED  95X-AREA                    
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'A01-AREA'.            
014200                                                                          
014300*01  AREA -COPY W510A01  -PRE A01-                                        
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'AX2-AREA'.            
014600                                                                          
014700*01  AREA -COPY W510AX2  -PRE AX2-                                        
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'A07-AREA'.            
015000                                                                          
015100*01  AREA -COPY W510A07  -PRE A07-                                        
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)   VALUE 'A09-AREA'.            
015400                                                                          
015500*01  AREA -COPY W510A09  -PRE A09-                                        
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)   VALUE 'L09-AREA'.            
015800                                                                          
015900*01  AREA -COPY W510L09  -PRE L09-                                        
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16)   VALUE 'A15-AREA'.            
016200                                                                          
016300*01  AREA -COPY W510A15  -PRE A15-                                        
016400     EJECT                                                                
016500 01  FILLER                      PIC X(16)   VALUE 'LB-AREA'.             
016600                                                                          
016700*01  AREA -COPY W01160   -PRE LB-                                         
016800     EJECT                                                                
016900 PROCEDURE DIVISION.                                                      
017000 MAIN SECTION.                                                            
017100                                                                          
017200     PERFORM A-INIT                                                       
017300                                                                          
017400     SORT SORTFIL ASCENDING  KEY SRT-IDFAKT                               
017500                  DESCENDING KEY SRT-IDPTYP                               
017600                                                                          
017700                  INPUT  PROCEDURE A-KOMPL-FAKT-POST                      
017800                  OUTPUT PROCEDURE B-SKAPA-LAB-TRANSAR                    
017900                                                                          
018000     IF SORT-RETURN NOT = 0                                               
018100       MOVE SORT-RETURN TO SORT-RETURN-X                                  
018200       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
018300           DELIMITED BY SIZE                                              
018400           INTO FELTEXT-STR                                               
018500       DISPLAY FELTEXT                                                    
018600       PERFORM S99-ABEND                                                  
018700     END-IF                                                               
018800                                                                          
018900     PERFORM Z-FINIT                                                      
019000                                                                          
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 A-INIT SECTION.                                                          
019600                                                                          
019700     OPEN INPUT  W4758C                                                   
019800                 W01160                                                   
019900          OUTPUT W51031                                                   
020000                                                                          
020100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020200     .                                                                    
020300     EJECT                                                                
020400 A-KOMPL-FAKT-POST SECTION.                                               
020500                                                                          
020600     PERFORM S01-LAES-INFIL                                               
020700     PERFORM S02-LAES-LBFIL                                               
020800     PERFORM UNTIL END-OF-INFIL                                           
020900       IF 95X-IDPTYP = '94X'                                              
021000         IF 94X-IDARTNR = LB-CLAG-IDARTNR                                 
021100           MOVE LB-CLAG-KDPRODSL   TO SORT-KDPRODSL                       
021200           MOVE LB-CLAG-KDPSLLOC   TO SORT-KDPSLLOC                       
021300           PERFORM S03-RELEASE-SORT-POST                                  
021400           PERFORM S01-LAES-INFIL                                         
021500         ELSE                                                             
021600           IF LB-CLAG-IDARTNR < 94X-IDARTNR                               
021700             PERFORM S02-LAES-LBFIL                                       
021800           ELSE                                                           
021900             DISPLAY '*** OMATCH ' LB-CLAG-IDARTNR ' ' 94X-IDARTNR        
022000             MOVE ZERO             TO SORT-KDPRODSL                       
022100                                      SORT-KDPSLLOC                       
022200             PERFORM S03-RELEASE-SORT-POST                                
022300             PERFORM S01-LAES-INFIL                                       
022400           END-IF                                                         
022500         END-IF                                                           
022600       ELSE                                                               
022700         PERFORM S03-RELEASE-SORT-POST                                    
022800         PERFORM S01-LAES-INFIL                                           
022900       END-IF                                                             
023000     END-PERFORM                                                          
023100     .                                                                    
023200     EJECT                                                                
023300 B-SKAPA-LAB-TRANSAR SECTION.                                             
023400                                                                          
023500     PERFORM S04-LAES-SORTFIL                                             
023600     PERFORM UNTIL END-OF-SORTFIL                                         
023700       IF 95X-IDPTYP = '95X'                                              
023800         MOVE 95X-IDDC            TO WS-IDDC                              
023900         IF GOOD-DDC                                                      
024000*****  DIREKTLEVERANSER SKALL HÖRA TILL CDC I LAB                         
024100           MOVE WC-CDC-SE         TO WS-IDDC                              
024200                                     95X-IDDC                             
024300         END-IF                                                           
024400         MOVE 95X-DAFAKT          TO W95X-DAFAKT                          
024500         MOVE 95X-FLOVRLEV        TO W95X-FLOVRLEV                        
024600         MOVE 95X-IDDISTR         TO TEST-IDDISTR                         
024700         EVALUATE TRUE                                                    
024800           WHEN DIST35-REFILL-NA                                          
024900             PERFORM BB-SKAPA-A01-REFILL                                  
025000           WHEN DIST35-REFILL-NA-JAP                                      
025100             PERFORM BC-SKAPA-A01-REFILL-JAP                              
025200           WHEN DIST07-USA-RETAILER OR DIST07-USA-RET-DISCR               
025300             IF CDC-SE                                                    
025400               PERFORM BD-SKAPA-A01-USA-RETAILER                          
025500             END-IF                                                       
025600           WHEN DIST07-CAN-RETAILER OR DIST07-CAN-RET-DISCR               
025700             IF CDC-SE                                                    
025800               PERFORM BE-SKAPA-A01-CAN-RETAILER                          
025900             END-IF                                                       
026000         END-EVALUATE                                                     
026100       ELSE                                                               
026200         IF 95X-IDPTYP = '94X'                                            
026300           MOVE 94X-IDDISTR         TO TEST-IDDISTR                       
026400           MOVE 94X-IDDC            TO WS-IDDC                            
026500           IF GOOD-DDC                                                    
026600*****  DIREKTLEVERANSER SKALL HÖRA TILL CDC I LAB                         
026700             MOVE WC-CDC-SE         TO WS-IDDC                            
026800                                       94X-IDDC                           
026900           END-IF                                                         
027000           EVALUATE TRUE                                                  
027100             WHEN DIST35-REFILL-NA                                        
027200               PERFORM BF-SKAPA-AX2-REFILL                                
027300             WHEN DIST35-REFILL-NA-JAP                                    
027400               PERFORM BG-SKAPA-AX2-REFILL-JAP                            
027500             WHEN DIST07-USA-RETAILER OR DIST07-USA-RET-DISCR             
027600               IF CDC-SE                                                  
027700                 PERFORM BH-SKAPA-AX2-USA-RETAILER                        
027800               ELSE                                                       
027900                 IF 94X-FLLSBOK = JA                                      
028000                   PERFORM BR-SKAPA-L02-LVANALYS-USA                      
028100                 END-IF                                                   
028200               END-IF                                                     
028300             WHEN DIST07-CAN-RETAILER OR DIST07-CAN-RET-DISCR             
028400               IF CDC-SE                                                  
028500                 PERFORM BJ-SKAPA-AX2-CAN-RETAILER                        
028600               ELSE                                                       
028700                 IF 94X-FLLSBOK = JA                                      
028800                   PERFORM BS-SKAPA-L02-LVANALYS-CAN                      
028900                 END-IF                                                   
029000               END-IF                                                     
029100             WHEN DIST35-NA-CDC-RETURN                                    
029200               PERFORM BL-SKAPA-A09-NA-RETURN                             
029300             WHEN DIST35-CAN-US-TRANSFER                                  
029400               IF NDC-NA                                                  
029500                 PERFORM BS-SKAPA-L09-CAN-US-TRANSFER                     
029600               END-IF                                                     
029610             WHEN DIST35-CA-USA-REFILL                                    
029620               IF NDC-NA                                                  
029630                 PERFORM BS-SKAPA-L09-CAN-US-TRANSFER                     
029640               END-IF                                                     
029650             WHEN DIST35-CA-US-RETUR                                      
029660               IF NDC-NA                                                  
029670                 PERFORM BS-SKAPA-L09-CAN-US-TRANSFER                     
029680               END-IF                                                     
029700             WHEN DIST35-US-US-TRANSFER                                   
029800               IF NDC-NA                                                  
029900                 PERFORM BM-SKAPA-A09-US-US-TRANSFER                      
030000               END-IF                                                     
030010             WHEN DIST35-USA-USA-REFILL                                   
030020               IF NDC-NA                                                  
030030                 PERFORM BM-SKAPA-A09-US-US-TRANSFER                      
030040               END-IF                                                     
030050             WHEN DIST35-US-US-RETUR                                      
030060               IF NDC-NA                                                  
030070                 PERFORM BM-SKAPA-A09-US-US-TRANSFER                      
030080               END-IF                                                     
030100             WHEN DIST35-US-CAN-TRANSFER                                  
030200               IF NDC-NA                                                  
030300                 PERFORM BR-SKAPA-L09-US-CAN-TRANSFER                     
030400               END-IF                                                     
030410             WHEN DIST35-USA-CA-REFILL                                    
030420               IF NDC-NA                                                  
030430                 PERFORM BR-SKAPA-L09-US-CAN-TRANSFER                     
030440               END-IF                                                     
030450             WHEN DIST35-US-CA-RETUR                                      
030460               IF NDC-NA                                                  
030470                 PERFORM BR-SKAPA-L09-US-CAN-TRANSFER                     
030480               END-IF                                                     
030500             WHEN DIST18-SCRAP-NDC    OR DIST55-KITS                      
030600               OR DIST55-APM          OR DIST55-DRILL-BITS                
030700               OR DIST55-SERVICE-SHOP OR DIST53-KEYS                      
030800               OR DIST70-NDC-MIX OR DIST18-SCRAP-NDC-SC-LOCAL             
030900               IF NDC-NA                                                  
031000                 PERFORM BN-SKAPA-A07-NDC-SCRAP-OR-MIX                    
031100               END-IF                                                     
031200             WHEN DIST20-EMBALLAGE-NDC                                    
031300               IF NDC-NA                                                  
031400                 PERFORM BO-SKAPA-A07-PACKING-MATERIAL                    
031500               END-IF                                                     
031600             WHEN DIS134-BYTESREN-NA                                      
031700               IF NDC-NA                                                  
031800                 PERFORM BP-SKAPA-A15-LAB-CORES-TO-REN                    
031900               END-IF                                                     
032000           END-EVALUATE                                                   
032100         END-IF                                                           
032200       END-IF                                                             
032300       PERFORM S04-LAES-SORTFIL                                           
032400     END-PERFORM                                                          
032500     .                                                                    
032600     EJECT                                                                
032700 B01-FYLL-I-A01-SPARA-W95X-FALT SECTION.                                  
032800                                                                          
032900     MOVE 95X-IDDISTR         TO A01-IDDISTR                              
033000     MOVE 95X-IDKUNDNR        TO A01-IDKUNDNR                             
033100     MOVE 95X-IDFAKT          TO A01-IDFAKT                               
033200     MOVE 95X-KDFAKTYP        TO A01-KDFAKTYP                             
033300     MOVE 95X-DAFAKT          TO A01-DAFAKT                               
033400     MOVE 95X-KDFRAKT         TO A01-KDFRAKT                              
033500     MOVE 95X-SUFAKTRE        TO A01-SUFAKTRE                             
033600     MOVE 95X-PREMBHNT        TO A01-PREMBHNT                             
033700     MOVE 95X-PRFRAKT         TO A01-PRFRAKT                              
033800     MOVE 95X-PRFOERS         TO A01-PRFOERS                              
033900     MOVE 95X-PRMOMS          TO A01-PRMOMS                               
034000     MOVE 95X-PRLEGKST        TO A01-PRLEGKST                             
034100     MOVE ZERO                TO A01-SUFKTTILL                            
034200     MOVE 95X-PRAVDRAG        TO A01-PRAVDRAG                             
034300     MOVE 95X-SUFKTBEL        TO A01-SUFKTBEL                             
034400     MOVE 95X-SUFKTUTL        TO A01-SUFKTUTL                             
034500     MOVE 95X-PRKURS          TO A01-PRKURS                               
034600     IF 95X-BEVARREF = SPACE                                              
034700       MOVE ZERO              TO W95X-IDRAPPNR                            
034800     ELSE                                                                 
034900       MOVE 95X-BEVARREF(1:7) TO W95X-IDRAPPNR                            
035000     END-IF                                                               
035100     MOVE W95X-IDRAPPNR       TO A01-IDRAPPNR                             
035200     .                                                                    
035300     EJECT                                                                
035400 BB-SKAPA-A01-REFILL SECTION.                                             
035500                                                                          
035600     PERFORM B01-FYLL-I-A01-SPARA-W95X-FALT                               
035700     MOVE 'A01'               TO A01-IDPTYP                               
035800     MOVE 'T10'               TO A01-KDEKOHT                              
035900     MOVE 53                  TO A01-IDFTG                                
036000     MOVE WC-CDC-SE           TO A01-IDDC-SEND                            
036100     EVALUATE TRUE                                                        
036200       WHEN DIST35-CDC-NDC41-REFILL                                       
036300         MOVE WC-NDC-US-RU    TO A01-IDDC-REC                             
036600       WHEN DIST35-CDC-NDC43-REFILL                                       
036700         MOVE WC-NDC-US-LA    TO A01-IDDC-REC                             
036710       WHEN DIST35-CDC-NDC44-REFILL                                       
036720         MOVE WC-NDC-US-SE    TO A01-IDDC-REC                             
036730       WHEN DIST35-CDC-NDC45-REFILL                                       
036740         MOVE WC-NDC-US-CH    TO A01-IDDC-REC                             
036750       WHEN DIST35-CDC-NDC46-REFILL                                       
036760         MOVE WC-NDC-US-JA    TO A01-IDDC-REC                             
036770       WHEN DIST35-CDC-NDC47-REFILL                                       
036780         MOVE WC-NDC-US-DA    TO A01-IDDC-REC                             
036800       WHEN DIST35-CDC-NDC51-REFILL                                       
036900         MOVE 54              TO A01-IDFTG                                
037000         MOVE WC-NDC-CA       TO A01-IDDC-REC                             
037100     END-EVALUATE                                                         
037200                                                                          
037300     PERFORM S11-SKRIV-W51031-A01                                         
037400     .                                                                    
037500     EJECT                                                                
037600 BC-SKAPA-A01-REFILL-JAP SECTION.                                         
037700                                                                          
037800     PERFORM B01-FYLL-I-A01-SPARA-W95X-FALT                               
037900     MOVE 'A01'               TO A01-IDPTYP                               
038000     MOVE 'I20'               TO A01-KDEKOHT                              
038100     MOVE 53                  TO A01-IDFTG                                
038200     MOVE WC-CDC-SE           TO A01-IDDC-SEND                            
038300     EVALUATE TRUE                                                        
038400       WHEN DIST35-JAP-NDC41-REFILL                                       
038500         MOVE WC-NDC-US-RU    TO A01-IDDC-REC                             
038800       WHEN DIST35-JAP-NDC43-REFILL                                       
038900         MOVE WC-NDC-US-LA    TO A01-IDDC-REC                             
038910       WHEN DIST35-JAP-NDC44-REFILL                                       
038920         MOVE WC-NDC-US-SE    TO A01-IDDC-REC                             
039000       WHEN DIST35-JAP-NDC51-REFILL                                       
039100         MOVE 54              TO A01-IDFTG                                
039200         MOVE WC-NDC-CA       TO A01-IDDC-REC                             
039300     END-EVALUATE                                                         
039400                                                                          
039500     PERFORM S11-SKRIV-W51031-A01                                         
039600     .                                                                    
039700     EJECT                                                                
039800 BD-SKAPA-A01-USA-RETAILER SECTION.                                       
039900                                                                          
040000     PERFORM B01-FYLL-I-A01-SPARA-W95X-FALT                               
040100     MOVE 'A01'               TO A01-IDPTYP                               
040200     MOVE 'O20'               TO A01-KDEKOHT                              
040300     MOVE 53                  TO A01-IDFTG                                
040400     MOVE WC-CDC-SE           TO A01-IDDC-SEND                            
040500     MOVE WC-NDC-US-RU        TO A01-IDDC-REC                             
040600                                                                          
040700     PERFORM S11-SKRIV-W51031-A01                                         
040800     .                                                                    
040900     SKIP3                                                                
041000 BE-SKAPA-A01-CAN-RETAILER SECTION.                                       
041100                                                                          
041200     PERFORM B01-FYLL-I-A01-SPARA-W95X-FALT                               
041300     MOVE 'A01'               TO A01-IDPTYP                               
041400     MOVE 'O20'               TO A01-KDEKOHT                              
041500     MOVE 54                  TO A01-IDFTG                                
041600     MOVE WC-CDC-SE           TO A01-IDDC-SEND                            
041700     MOVE WC-NDC-CA           TO A01-IDDC-REC                             
041800                                                                          
041900     PERFORM S11-SKRIV-W51031-A01                                         
042000     .                                                                    
042100     EJECT                                                                
042200 BF-SKAPA-AX2-REFILL SECTION.                                             
042300                                                                          
042400     MOVE 'T10'               TO AX2-KDEKOHT                              
042500     MOVE 53                  TO AX2-IDFTG                                
042600     EVALUATE TRUE                                                        
042700       WHEN DIST35-CDC-NDC41-REFILL                                       
042800         MOVE WC-NDC-US-RU    TO AX2-IDDC-REC                             
043100       WHEN DIST35-CDC-NDC43-REFILL                                       
043200         MOVE WC-NDC-US-LA    TO AX2-IDDC-REC                             
043210       WHEN DIST35-CDC-NDC44-REFILL                                       
043220         MOVE WC-NDC-US-SE    TO AX2-IDDC-REC                             
043230       WHEN DIST35-CDC-NDC45-REFILL                                       
043240         MOVE WC-NDC-US-CH    TO AX2-IDDC-REC                             
043250       WHEN DIST35-CDC-NDC46-REFILL                                       
043260         MOVE WC-NDC-US-JA    TO AX2-IDDC-REC                             
043270       WHEN DIST35-CDC-NDC47-REFILL                                       
043280         MOVE WC-NDC-US-DA    TO AX2-IDDC-REC                             
043300       WHEN DIST35-CDC-NDC51-REFILL                                       
043400         MOVE 54              TO AX2-IDFTG                                
043500         MOVE WC-NDC-CA       TO AX2-IDDC-REC                             
043600     END-EVALUATE                                                         
043700     MOVE 'AX2'               TO AX2-IDPTYP                               
043800     MOVE WC-CDC-SE           TO AX2-IDDC-SEND                            
043900     MOVE 94X-IDDISTR         TO AX2-IDDISTR                              
044000     MOVE 94X-IDDEALER        TO AX2-IDKUNDNR                             
044100     MOVE 94X-IDFAKT          TO AX2-IDFAKT                               
044200     MOVE 94X-KDFAKTYP        TO AX2-KDFAKTYP                             
044300     MOVE W95X-DAFAKT         TO AX2-DAFAKT                               
044400     MOVE 00                  TO AX2-IDORDNR7(1:2)                        
044500     MOVE 94X-IDKUNDRF(1:5)   TO AX2-IDORDNR7(3:5)                        
044600     MOVE 94X-IDARTNR         TO AX2-IDARTNR                              
044700     MOVE SORT-KDPRODSL       TO AX2-KDPRODSL                             
044800     MOVE SORT-KDPSLLOC       TO AX2-KDPSLLOC                             
044900     MOVE 94X-KVLEVART        TO AX2-KVLEVART                             
045000     MOVE 94X-PRARTNTO        TO AX2-PRARTNTO                             
045100     MOVE W95X-FLOVRLEV       TO AX2-FLOVRLEV                             
045200                                                                          
045300     PERFORM S12-SKRIV-W51031-AX2                                         
045400     .                                                                    
045500     EJECT                                                                
045600 BG-SKAPA-AX2-REFILL-JAP SECTION.                                         
045700                                                                          
045800     MOVE 'I20'               TO AX2-KDEKOHT                              
045900     MOVE 53                  TO AX2-IDFTG                                
046000     EVALUATE TRUE                                                        
046100       WHEN DIST35-JAP-NDC41-REFILL                                       
046200         MOVE WC-NDC-US-RU    TO AX2-IDDC-REC                             
046500       WHEN DIST35-JAP-NDC43-REFILL                                       
046600         MOVE WC-NDC-US-LA    TO AX2-IDDC-REC                             
046610       WHEN DIST35-JAP-NDC44-REFILL                                       
046620         MOVE WC-NDC-US-SE    TO AX2-IDDC-REC                             
046700       WHEN DIST35-JAP-NDC51-REFILL                                       
046800         MOVE 54              TO AX2-IDFTG                                
046900         MOVE WC-NDC-CA       TO AX2-IDDC-REC                             
047000     END-EVALUATE                                                         
047100     MOVE 'AX2'               TO AX2-IDPTYP                               
047200     MOVE WC-CDC-SE           TO AX2-IDDC-SEND                            
047300     MOVE 94X-IDDISTR         TO AX2-IDDISTR                              
047400     MOVE 94X-IDDEALER        TO AX2-IDKUNDNR                             
047500     MOVE 94X-IDFAKT          TO AX2-IDFAKT                               
047600     MOVE 94X-KDFAKTYP        TO AX2-KDFAKTYP                             
047700     MOVE W95X-DAFAKT         TO AX2-DAFAKT                               
047800     MOVE 00                  TO AX2-IDORDNR7(1:2)                        
047900     MOVE 94X-IDKUNDRF(1:5)   TO AX2-IDORDNR7(3:5)                        
048000     MOVE 94X-IDARTNR         TO AX2-IDARTNR                              
048100     MOVE SORT-KDPRODSL       TO AX2-KDPRODSL                             
048200     MOVE SORT-KDPSLLOC       TO AX2-KDPSLLOC                             
048300     MOVE 94X-KVLEVART        TO AX2-KVLEVART                             
048400     MOVE 94X-PRARTNTO        TO AX2-PRARTNTO                             
048500     MOVE W95X-FLOVRLEV       TO AX2-FLOVRLEV                             
048600                                                                          
048700     PERFORM S12-SKRIV-W51031-AX2                                         
048800     .                                                                    
048900     EJECT                                                                
049000 BH-SKAPA-AX2-USA-RETAILER SECTION.                                       
049100                                                                          
049200     MOVE 'O20'               TO AX2-KDEKOHT                              
049300     MOVE 53                  TO AX2-IDFTG                                
049400     MOVE WC-NDC-US-RU        TO AX2-IDDC-REC                             
049500     MOVE 'AX2'               TO AX2-IDPTYP                               
049600     MOVE WC-CDC-SE           TO AX2-IDDC-SEND                            
049700     MOVE 94X-IDDISTR         TO AX2-IDDISTR                              
049800     MOVE 94X-IDDEALER        TO AX2-IDKUNDNR                             
049900     MOVE 94X-IDFAKT          TO AX2-IDFAKT                               
050000     MOVE 94X-KDFAKTYP        TO AX2-KDFAKTYP                             
050100     MOVE W95X-DAFAKT         TO AX2-DAFAKT                               
050200     MOVE 00                  TO AX2-IDORDNR7(1:2)                        
050300     MOVE 94X-IDKUNDRF(1:5)   TO AX2-IDORDNR7(3:5)                        
050400     MOVE 94X-IDARTNR         TO AX2-IDARTNR                              
050500     MOVE SORT-KDPRODSL       TO AX2-KDPRODSL                             
050600     MOVE SORT-KDPSLLOC       TO AX2-KDPSLLOC                             
050700     MOVE 94X-KVLEVART        TO AX2-KVLEVART                             
050800     MOVE 94X-PRARTNTO        TO AX2-PRARTNTO                             
050900     MOVE W95X-FLOVRLEV       TO AX2-FLOVRLEV                             
051000                                                                          
051100     PERFORM S12-SKRIV-W51031-AX2                                         
051200     .                                                                    
051300     EJECT                                                                
051400 BJ-SKAPA-AX2-CAN-RETAILER SECTION.                                       
051500                                                                          
051600     MOVE 'O20'               TO AX2-KDEKOHT                              
051700     MOVE 54                  TO AX2-IDFTG                                
051800     MOVE WC-NDC-CA           TO AX2-IDDC-REC                             
051900     MOVE 'AX2'               TO AX2-IDPTYP                               
052000     MOVE WC-CDC-SE           TO AX2-IDDC-SEND                            
052100     MOVE 94X-IDDISTR         TO AX2-IDDISTR                              
052200     MOVE 94X-IDDEALER        TO AX2-IDKUNDNR                             
052300     MOVE 94X-IDFAKT          TO AX2-IDFAKT                               
052400     MOVE 94X-KDFAKTYP        TO AX2-KDFAKTYP                             
052500     MOVE W95X-DAFAKT         TO AX2-DAFAKT                               
052600     MOVE 00                  TO AX2-IDORDNR7(1:2)                        
052700     MOVE 94X-IDKUNDRF(1:5)   TO AX2-IDORDNR7(3:5)                        
052800     MOVE 94X-IDARTNR         TO AX2-IDARTNR                              
052900     MOVE SORT-KDPRODSL       TO AX2-KDPRODSL                             
053000     MOVE SORT-KDPSLLOC       TO AX2-KDPSLLOC                             
053100     MOVE 94X-KVLEVART        TO AX2-KVLEVART                             
053200     MOVE 94X-PRARTNTO        TO AX2-PRARTNTO                             
053300     MOVE W95X-FLOVRLEV       TO AX2-FLOVRLEV                             
053400                                                                          
053500     PERFORM S12-SKRIV-W51031-AX2                                         
053600     .                                                                    
053700     EJECT                                                                
053800 BS-SKAPA-L09-CAN-US-TRANSFER SECTION.                                    
053900                                                                          
054000     MOVE 'L09'               TO L09-IDPTYP                               
054100     MOVE 'T40'               TO L09-KDEKOHT                              
054200     MOVE 94X-IDDC            TO L09-IDDC-SEND                            
054300     SEARCH ALL DIST57-REFILL-DC                                          
054400       AT END                                                             
054500         MOVE 'EJ TRÄFF I REFILLTAB WWDIST57' TO FELTEXT                  
054600         CALL ABEND                                                       
054700       WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR                  
054800         MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO L09-IDDC-REC              
054900     END-SEARCH                                                           
055000     MOVE 54                  TO L09-IDFTG                                
055100     MOVE 94X-IDDISTR         TO L09-IDDISTR                              
055200     MOVE 94X-IDKUNDNR        TO L09-IDKUNDNR                             
055300     MOVE 94X-IDFAKT          TO L09-IDFAKT                               
055400     MOVE 00                  TO L09-IDORDNR7(1:2)                        
055500     MOVE 94X-IDKUNDRF(1:5)   TO L09-IDORDNR7(3:5)                        
055600     MOVE W95X-DAFAKT         TO L09-DAFAKT                               
055700     MOVE 94X-IDARTNR         TO L09-IDARTNR                              
055800     MOVE SORT-KDPRODSL       TO L09-KDPRODSL                             
055900     MOVE SORT-KDPSLLOC       TO L09-KDPSLLOC                             
056000     MOVE 94X-KVLEVART        TO L09-KVLEVART                             
056100     MOVE 94X-PRAVCOST        TO L09-PRAVCOST                             
056200     MOVE W95X-FLOVRLEV       TO L09-FLOVRLEV                             
056300                                                                          
056400     PERFORM S15-SKRIV-W51031-L09                                         
056500     .                                                                    
056600     EJECT                                                                
056700 BL-SKAPA-A09-NA-RETURN SECTION.                                          
056800                                                                          
056900     MOVE 'A09'               TO A09-IDPTYP                               
057000     MOVE 'T20'               TO A09-KDEKOHT                              
057100     MOVE 94X-IDDC            TO A09-IDDC-SEND                            
057200     MOVE WC-CDC-SE           TO A09-IDDC-REC                             
057300     IF NDC-CA                                                            
057400       MOVE 54                TO A09-IDFTG                                
057500     ELSE                                                                 
057600       MOVE 53                TO A09-IDFTG                                
057700     END-IF                                                               
057800     MOVE 94X-IDDISTR         TO A09-IDDISTR                              
057900     MOVE 94X-IDKUNDNR        TO A09-IDKUNDNR                             
058000     MOVE 94X-IDFAKT          TO A09-IDFAKT                               
058100     MOVE 00                  TO A09-IDORDNR7(1:2)                        
058200     MOVE 94X-IDKUNDRF(1:5)   TO A09-IDORDNR7(3:5)                        
058300     MOVE W95X-DAFAKT         TO A09-DAFAKT                               
058400     MOVE 94X-IDARTNR         TO A09-IDARTNR                              
058500     MOVE SORT-KDPRODSL       TO A09-KDPRODSL                             
058600     MOVE SORT-KDPSLLOC       TO A09-KDPSLLOC                             
058700     MOVE 94X-KVLEVART        TO A09-KVLEVART                             
058800     MOVE 94X-PRAVCOST        TO A09-PRAVCOST                             
058900                                                                          
059000     PERFORM S14-SKRIV-W51031-A09                                         
059100     .                                                                    
059200     EJECT                                                                
059300 BR-SKAPA-L09-US-CAN-TRANSFER SECTION.                                    
059400                                                                          
059500     MOVE 'L09'               TO L09-IDPTYP                               
059600     MOVE 'T40'               TO L09-KDEKOHT                              
059700     MOVE 53                  TO L09-IDFTG                                
059800     MOVE 94X-IDDISTR         TO L09-IDDISTR                              
059900     MOVE 94X-IDDC            TO L09-IDDC-SEND                            
060000     MOVE WC-NDC-CA           TO L09-IDDC-REC                             
060100     MOVE 94X-IDDISTR         TO L09-IDDISTR                              
060200     MOVE 94X-IDKUNDNR        TO L09-IDKUNDNR                             
060300     MOVE 94X-IDFAKT          TO L09-IDFAKT                               
060400     MOVE 00                  TO L09-IDORDNR7(1:2)                        
060500     MOVE 94X-IDKUNDRF(1:5)   TO L09-IDORDNR7(3:5)                        
060600     MOVE W95X-DAFAKT         TO L09-DAFAKT                               
060700     MOVE 94X-IDARTNR         TO L09-IDARTNR                              
060800     MOVE SORT-KDPRODSL       TO L09-KDPRODSL                             
060900     MOVE SORT-KDPSLLOC       TO L09-KDPSLLOC                             
061000     MOVE 94X-KVLEVART        TO L09-KVLEVART                             
061100     MOVE 94X-PRAVCOST        TO L09-PRAVCOST                             
061200     MOVE W95X-FLOVRLEV       TO L09-FLOVRLEV                             
061300                                                                          
061400     PERFORM S15-SKRIV-W51031-L09                                         
061500     .                                                                    
061600     EJECT                                                                
061700 BM-SKAPA-A09-US-US-TRANSFER    SECTION.                                  
061800                                                                          
061900     MOVE 'A09'               TO A09-IDPTYP                               
062000     MOVE 'T30'               TO A09-KDEKOHT                              
062100     MOVE 53                  TO A09-IDFTG                                
062200     MOVE 94X-IDDISTR         TO A09-IDDISTR                              
062300     MOVE 94X-IDDC            TO A09-IDDC-SEND                            
062400     SEARCH ALL DIST57-REFILL-DC                                          
062500       AT END                                                             
062600         MOVE 'EJ TRÄFF I REFILLTAB WWDIST57' TO FELTEXT                  
062700         CALL ABEND                                                       
062800       WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR                  
062900         MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO A09-IDDC-REC              
063000     END-SEARCH                                                           
063100     MOVE 94X-IDDISTR         TO A09-IDDISTR                              
063200     MOVE 94X-IDKUNDNR        TO A09-IDKUNDNR                             
063300     MOVE 94X-IDFAKT          TO A09-IDFAKT                               
063400     MOVE 00                  TO A09-IDORDNR7(1:2)                        
063500     MOVE 94X-IDKUNDRF(1:5)   TO A09-IDORDNR7(3:5)                        
063600     MOVE W95X-DAFAKT         TO A09-DAFAKT                               
063700     MOVE 94X-IDARTNR         TO A09-IDARTNR                              
063800     MOVE SORT-KDPRODSL       TO A09-KDPRODSL                             
063900     MOVE SORT-KDPSLLOC       TO A09-KDPSLLOC                             
064000     MOVE 94X-KVLEVART        TO A09-KVLEVART                             
064100     MOVE 94X-PRAVCOST        TO A09-PRAVCOST                             
064200                                                                          
064300     PERFORM S14-SKRIV-W51031-A09                                         
064400     .                                                                    
064500     EJECT                                                                
064600 BN-SKAPA-A07-NDC-SCRAP-OR-MIX SECTION.                                   
064700                                                                          
064800     MOVE 'A07'               TO A07-IDPTYP                               
064900     EVALUATE TRUE                                                        
065000       WHEN DIST18-SCRAP-NDC-SC                                           
065100         MOVE 'O70'           TO A07-KDEKOHT                              
065110       WHEN DIST18-SCRAP-NDC-SC-LOCAL                                     
065120         MOVE 'O70'           TO A07-KDEKOHT                              
065200       WHEN DIST18-SCRAP-NDC-QUAL                                         
065300         MOVE 'O71'           TO A07-KDEKOHT                              
065400       WHEN DIST18-SCRAP-NDC-DAM                                          
065500         MOVE 'O72'           TO A07-KDEKOHT                              
065600       WHEN DIST18-SCRAP-NDC-ECO                                          
065700         MOVE 'O73'           TO A07-KDEKOHT                              
065800       WHEN DIST55-DRILL-BITS                                             
065900         MOVE 'O81'           TO A07-KDEKOHT                              
066000       WHEN DIST55-KITS                                                   
066100         MOVE 'M40'           TO A07-KDEKOHT                              
066200       WHEN DIST55-APM                                                    
066300         MOVE 'M41'           TO A07-KDEKOHT                              
066400       WHEN DIST55-SERVICE-SHOP                                           
066500         MOVE 'M42'           TO A07-KDEKOHT                              
066600       WHEN DIST70-NDC-MIX                                                
066700         MOVE 'M43'           TO A07-KDEKOHT                              
066800       WHEN DIST53-KEYS                                                   
066900         MOVE 'M44'           TO A07-KDEKOHT                              
067000     END-EVALUATE                                                         
067100     MOVE 94X-IDDC            TO A07-IDDC-SEND                            
067200                                 A07-IDDC-REC                             
067300     IF A07-KDEKOHT = 'O71'                                               
067400       MOVE WC-CDC-SE         TO A07-IDDC-REC                             
067500     END-IF                                                               
067600     IF NDC-CA                                                            
067700       MOVE 54                TO A07-IDFTG                                
067800     ELSE                                                                 
067900       MOVE 53                TO A07-IDFTG                                
068000     END-IF                                                               
068100     MOVE 94X-IDDISTR         TO A07-IDDISTR                              
068200     MOVE 94X-IDKUNDNR        TO A07-IDKUNDNR                             
068300     MOVE 94X-IDFAKT          TO A07-IDFAKT                               
068400     MOVE W95X-DAFAKT         TO A07-DAFAKT                               
068500     MOVE 00                  TO A07-IDORDNR7(1:2)                        
068600     MOVE 94X-IDKUNDRF(1:5)   TO A07-IDORDNR7(3:5)                        
068700     MOVE 94X-IDARTNR         TO A07-IDARTNR                              
068800     MOVE SORT-KDPRODSL       TO A07-KDPRODSL                             
068900     MOVE SORT-KDPSLLOC       TO A07-KDPSLLOC                             
069000     MOVE 94X-KVLEVART        TO A07-KVLEVART                             
069100     MOVE 94X-PRAVCOST        TO A07-PRAVCOST                             
069200     MOVE 94X-IDKONTO         TO A07-IDKONTO                              
069300     MOVE 94X-IDKST           TO A07-IDKST                                
069400                                                                          
069500     PERFORM S13-SKRIV-W51031-A07                                         
069600     .                                                                    
069700     EJECT                                                                
069800 BO-SKAPA-A07-PACKING-MATERIAL SECTION.                                   
069900                                                                          
070000     MOVE 'A07'               TO A07-IDPTYP                               
070100     MOVE 'O80'               TO A07-KDEKOHT                              
070200     MOVE 94X-IDDC            TO A07-IDDC-SEND                            
070300                                 A07-IDDC-REC                             
070400     IF NDC-CA                                                            
070500       MOVE 54                TO A07-IDFTG                                
070600     ELSE                                                                 
070700       MOVE 53                TO A07-IDFTG                                
070800     END-IF                                                               
070900     MOVE 94X-IDDISTR         TO A07-IDDISTR                              
071000     MOVE 94X-IDKUNDNR        TO A07-IDKUNDNR                             
071100     MOVE 94X-IDFAKT          TO A07-IDFAKT                               
071200     MOVE W95X-DAFAKT         TO A07-DAFAKT                               
071300     MOVE 00                  TO A07-IDORDNR7(1:2)                        
071400     MOVE 94X-IDKUNDRF(1:5)   TO A07-IDORDNR7(3:5)                        
071500     MOVE 94X-IDARTNR         TO A07-IDARTNR                              
071600     MOVE SORT-KDPRODSL       TO A07-KDPRODSL                             
071700     MOVE SORT-KDPSLLOC       TO A07-KDPSLLOC                             
071800     MOVE 94X-KVLEVART        TO A07-KVLEVART                             
071900     MOVE 94X-PRAVCOST        TO A07-PRAVCOST                             
072000     MOVE 94X-IDKONTO         TO A07-IDKONTO                              
072100     MOVE 94X-IDKST           TO A07-IDKST                                
072200                                                                          
072300     PERFORM S13-SKRIV-W51031-A07                                         
072400     .                                                                    
072500     EJECT                                                                
072600 BP-SKAPA-A15-LAB-CORES-TO-REN SECTION.                                   
072700                                                                          
072800     MOVE 'A15'               TO A15-IDPTYP                               
072900     MOVE 'O30'               TO A15-KDEKOHT                              
073000     MOVE 94X-IDDC            TO A15-IDDC-SEND                            
073100                                 A15-IDDC-REC                             
073200     IF NDC-CA                                                            
073300       MOVE 54                TO A15-IDFTG                                
073400     ELSE                                                                 
073500       MOVE 53                TO A15-IDFTG                                
073600     END-IF                                                               
073700     MOVE A15-IDDC-REC        TO A15-IDDC-SEND                            
073800     MOVE 94X-IDDISTR         TO A15-IDDISTR                              
073900     MOVE 94X-IDKUNDNR        TO A15-IDKUNDNR                             
074000     MOVE 94X-IDFAKT          TO A15-IDFAKT                               
074100     MOVE W95X-DAFAKT         TO A15-DAFAKT                               
074200     MOVE 94X-IDARTNR         TO A15-IDARTNR                              
074300     MOVE SORT-KDPRODSL       TO A15-KDPRODSL                             
074400     MOVE SORT-KDPSLLOC       TO A15-KDPSLLOC                             
074500     MOVE 94X-KVLEVART        TO A15-KVLEVART                             
074600     MOVE 94X-PRAVCOST        TO A15-PRAVCOST                             
074700     PERFORM S16-SKRIV-W51031-A15                                         
074800     .                                                                    
074900     EJECT                                                                
075000 BR-SKAPA-L02-LVANALYS-USA SECTION.                                       
075100                                                                          
075200     MOVE 'O10'               TO AX2-KDEKOHT                              
075300     MOVE 53                  TO AX2-IDFTG                                
075400     MOVE 94X-IDDC            TO AX2-IDDC-REC                             
075500     MOVE 'L02'               TO AX2-IDPTYP                               
075600     MOVE 94X-IDDC            TO AX2-IDDC-SEND                            
075700     MOVE 94X-IDDISTR         TO AX2-IDDISTR                              
075800     MOVE 94X-IDDEALER        TO AX2-IDKUNDNR                             
075900     MOVE 94X-IDFAKT          TO AX2-IDFAKT                               
076000     MOVE 94X-KDFAKTYP        TO AX2-KDFAKTYP                             
076100     MOVE W95X-DAFAKT         TO AX2-DAFAKT                               
076200     MOVE 00                  TO AX2-IDORDNR7(1:2)                        
076300     MOVE 94X-IDKUNDRF(1:5)   TO AX2-IDORDNR7(3:5)                        
076400     MOVE 94X-IDARTNR         TO AX2-IDARTNR                              
076500     MOVE SORT-KDPRODSL       TO AX2-KDPRODSL                             
076600     MOVE SORT-KDPSLLOC       TO AX2-KDPSLLOC                             
076700     MOVE 94X-KVLEVART        TO AX2-KVLEVART                             
076800     MOVE 94X-PRARTNTO        TO AX2-PRARTNTO                             
076900     MOVE W95X-FLOVRLEV       TO AX2-FLOVRLEV                             
077000                                                                          
077100     PERFORM S12-SKRIV-W51031-AX2                                         
077200     .                                                                    
077300     EJECT                                                                
077400 BS-SKAPA-L02-LVANALYS-CAN SECTION.                                       
077500                                                                          
077600     MOVE 'O10'               TO AX2-KDEKOHT                              
077700     MOVE 54                  TO AX2-IDFTG                                
077800     MOVE 94X-IDDC            TO AX2-IDDC-REC                             
077900     MOVE 'L02'               TO AX2-IDPTYP                               
078000     MOVE 94X-IDDC            TO AX2-IDDC-SEND                            
078100     MOVE 94X-IDDISTR         TO AX2-IDDISTR                              
078200     MOVE 94X-IDDEALER        TO AX2-IDKUNDNR                             
078300     MOVE 94X-IDFAKT          TO AX2-IDFAKT                               
078400     MOVE 94X-KDFAKTYP        TO AX2-KDFAKTYP                             
078500     MOVE W95X-DAFAKT         TO AX2-DAFAKT                               
078600     MOVE 00                  TO AX2-IDORDNR7(1:2)                        
078700     MOVE 94X-IDKUNDRF(1:5)   TO AX2-IDORDNR7(3:5)                        
078800     MOVE 94X-IDARTNR         TO AX2-IDARTNR                              
078900     MOVE SORT-KDPRODSL       TO AX2-KDPRODSL                             
079000     MOVE SORT-KDPSLLOC       TO AX2-KDPSLLOC                             
079100     MOVE 94X-KVLEVART        TO AX2-KVLEVART                             
079200     MOVE 94X-PRARTNTO        TO AX2-PRARTNTO                             
079300     MOVE W95X-FLOVRLEV       TO AX2-FLOVRLEV                             
079400                                                                          
079500     PERFORM S12-SKRIV-W51031-AX2                                         
079600     .                                                                    
079700     EJECT                                                                
079800 Z-FINIT SECTION.                                                         
079900                                                                          
080000     CLOSE W4758C W01160 W51031                                           
080100                                                                          
080200                                                                          
080300     MOVE 'S' TO POSTSUM-OPKOD                                            
080400     CALL POSTSUM USING POSTSUM-PARM                                      
080500     .                                                                    
080600     EJECT                                                                
080700 S01-LAES-INFIL   SECTION.                                                
080800                                                                          
080900     READ W4758C INTO SORT-AREA                                           
081000     AT END                                                               
081100        SET END-OF-INFIL TO TRUE                                          
081200                                                                          
081300     NOT AT END                                                           
081400        MOVE 'W4758C'    TO POSTSUM-FDNAMN                                
081500        MOVE 'W51030D1'  TO POSTSUM-DDNAMN2                               
081600        MOVE 95X-IDPTYP  TO POSTSUM-TRANSTYP                              
081700        CALL POSTSUM USING POSTSUM-PARM                                   
081800     END-READ                                                             
081900     .                                                                    
082000     SKIP3                                                                
082100 S02-LAES-LBFIL   SECTION.                                                
082200                                                                          
082300     READ W01160 INTO LB-AREA                                             
082400     AT END                                                               
082500        SET END-OF-LBFIL TO TRUE                                          
082600                                                                          
082700     NOT AT END                                                           
082800        MOVE 'W01160'    TO POSTSUM-FDNAMN                                
082900        MOVE 'W51030D2'  TO POSTSUM-DDNAMN2                               
083000        MOVE 'LB '       TO POSTSUM-TRANSTYP                              
083100        CALL POSTSUM USING POSTSUM-PARM                                   
083200     END-READ                                                             
083300     .                                                                    
083400     EJECT                                                                
083500 S03-RELEASE-SORT-POST SECTION.                                           
083600                                                                          
083700     RELEASE SRT-POST FROM SORT-AREA                                      
083800                                                                          
083900     MOVE 'SORTIN'     TO POSTSUM-FDNAMN                                  
084000     MOVE 'W51030DS'   TO POSTSUM-DDNAMN2                                 
084100     MOVE 95X-IDPTYP   TO POSTSUM-TRANSTYP                                
084200     CALL POSTSUM USING POSTSUM-PARM                                      
084300     .                                                                    
084400     EJECT                                                                
084500 S04-LAES-SORTFIL SECTION.                                                
084600                                                                          
084700     RETURN SORTFIL INTO SORT-AREA                                        
084800     AT END                                                               
084900        SET END-OF-SORTFIL TO TRUE                                        
085000                                                                          
085100     NOT AT END                                                           
085200        MOVE 'SORTUT'    TO POSTSUM-FDNAMN                                
085300        MOVE 'W51030DS'  TO POSTSUM-DDNAMN2                               
085400        MOVE 95X-IDPTYP  TO POSTSUM-TRANSTYP                              
085500        CALL POSTSUM USING POSTSUM-PARM                                   
085600     END-RETURN                                                           
085700     .                                                                    
085800     EJECT                                                                
085900 S11-SKRIV-W51031-A01 SECTION.                                            
086000                                                                          
086100     WRITE UA01-POST FROM A01-AREA                                        
086200                                                                          
086300     MOVE 'A01'       TO POSTSUM-TRANSTYP                                 
086400     MOVE 'W51031'    TO POSTSUM-FDNAMN                                   
086500     MOVE 'W51030D2'  TO POSTSUM-DDNAMN2                                  
086600     CALL POSTSUM USING POSTSUM-PARM                                      
086700     .                                                                    
086800     SKIP2                                                                
086900 S12-SKRIV-W51031-AX2 SECTION.                                            
087000                                                                          
087100     WRITE UAX2-POST FROM AX2-AREA                                        
087200                                                                          
087300     MOVE 'AX2'       TO POSTSUM-TRANSTYP                                 
087400     MOVE 'W51031'    TO POSTSUM-FDNAMN                                   
087500     MOVE 'W51030D2'  TO POSTSUM-DDNAMN2                                  
087600     CALL POSTSUM USING POSTSUM-PARM                                      
087700     .                                                                    
087800     SKIP2                                                                
087900 S13-SKRIV-W51031-A07 SECTION.                                            
088000                                                                          
088100     WRITE UA07-POST FROM A07-AREA                                        
088200                                                                          
088300     MOVE 'A07'       TO POSTSUM-TRANSTYP                                 
088400     MOVE 'W51031'    TO POSTSUM-FDNAMN                                   
088500     MOVE 'W51030D2'  TO POSTSUM-DDNAMN2                                  
088600     CALL POSTSUM USING POSTSUM-PARM                                      
088700     .                                                                    
088800     SKIP2                                                                
088900 S14-SKRIV-W51031-A09 SECTION.                                            
089000                                                                          
089100     WRITE UA09-POST FROM A09-AREA                                        
089200                                                                          
089300     MOVE 'A09'       TO POSTSUM-TRANSTYP                                 
089400     MOVE 'W51031'    TO POSTSUM-FDNAMN                                   
089500     MOVE 'W51030D2'  TO POSTSUM-DDNAMN2                                  
089600     CALL POSTSUM USING POSTSUM-PARM                                      
089700     .                                                                    
089800     SKIP2                                                                
089900 S15-SKRIV-W51031-L09 SECTION.                                            
090000                                                                          
090100     WRITE UL09-POST FROM L09-AREA                                        
090200                                                                          
090300     MOVE 'L09'       TO POSTSUM-TRANSTYP                                 
090400     MOVE 'W51031'    TO POSTSUM-FDNAMN                                   
090500     MOVE 'W51030D2'  TO POSTSUM-DDNAMN2                                  
090600     CALL POSTSUM USING POSTSUM-PARM                                      
090700     .                                                                    
090800     SKIP2                                                                
090900 S16-SKRIV-W51031-A15 SECTION.                                            
091000                                                                          
091100     WRITE UA15-POST FROM A15-AREA                                        
091200                                                                          
091300     MOVE 'A15'       TO POSTSUM-TRANSTYP                                 
091400     MOVE 'W51031'    TO POSTSUM-FDNAMN                                   
091500     MOVE 'W51030D2'  TO POSTSUM-DDNAMN2                                  
091600     CALL POSTSUM USING POSTSUM-PARM                                      
091700     .                                                                    
091800     SKIP3                                                                
091900 S99-ABEND SECTION.                                                       
092000                                                                          
092100     MOVE 'S' TO POSTSUM-OPKOD                                            
092200     CALL POSTSUM USING POSTSUM-PARM                                      
092300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
092400     .                                                                    

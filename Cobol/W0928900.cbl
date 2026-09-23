000300 ID DIVISION.                                                             
000400 PROGRAM-ID.                 W0928900.                                    
000900*DATE-WRITTEN.               JAN 1979.                                    
001200                                                                          
001210*    FUNKTION.                                                            
001300*        PROGRAMMET SKAPAR EN UTTRANSAKTION FÖR VARJE IFYLLT              
001400*        FÄLT I INTRANSEN, R89.                                           
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700 DATA DIVISION.                                                           
001800     EJECT                                                                
001900 WORKING-STORAGE SECTION.                                                 
001901                                                                          
001910*    -- CHECKED BY WY2000                                                 
002000 77  IDPGM                   PIC X(8)    VALUE 'W0928900'.                
002500                                                                          
002600 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
002700                                                                          
002800 01  KONSTANTER.                                                          
002900     03  JA                  PIC X       VALUE 'J'.                       
003000     03  NEJ                 PIC X       VALUE 'N'.                       
003100                                                                          
003200 01  SWITCHAR.                                                            
003300     03 SW-NY-R89            PIC X       VALUE 'J'.                       
003400     03 SW-IFYLLD-RESEASON   PIC X       VALUE 'N'.                       
003500     03 SW-IFYLLD-TREND      PIC X       VALUE 'N'.                       
003600     03 SW-IFYLLD-PBJUST     PIC X       VALUE 'N'.                       
003700                                                                          
003800 01  IDEX.                                                                
003900     03  IX                  PIC S9(4)               COMP SYNC.           
004000                                                                          
004100 01  W.                                                                   
004200     03  W-RESEASON.                                                      
004300         05  W-RESEASON-1-2  PIC X(2).                                    
004400         05  FILLER          PIC X       VALUE ZERO.                      
004500*                                                                         
004600     03  W-KDCLAGER          PIC X.                                       
004700     03  W-IDARTNR           PIC X(8).                                    
004800*                                                                         
004900     03  W-RVTREND.                                                       
005000         05  W-RVTREND-POS1  PIC X       VALUE ZERO.                      
005100         05  W-RVTREND-POS2  PIC X.                                       
005200     EJECT                                                                
005300*01  AREA -COPY W222R89TC0 -PRE R89-                                      
005500     EJECT                                                                
005600 LINKAGE SECTION.                                                         
005700     SKIP3                                                                
005800*01  AREA -COPY W222R89TC0 -PRE LINK-                                     
006000     SKIP3                                                                
006100 01  LINK-IDPTYP-UTAREA      PIC X(3).                                    
006200     SKIP3                                                                
006300 01  LINK-UTAREA             PIC X(100).                                  
006400     SKIP3                                                                
006500*01  RP1 -COPY W222RP1TC0 -PRE RP1- -RED LINK-UTAREA                      
006700     SKIP3                                                                
006800*01  RP2  -COPY W222RP2TC0 -PRE RP2- -RED LINK-UTAREA                     
007000     SKIP3                                                                
007100*01  RP3  -COPY W222RP3TC0 -PRE RP3- -RED LINK-UTAREA                     
007300     SKIP3                                                                
007400*01  RP4  -COPY W222RP4TC0 -PRE RP4- -RED LINK-UTAREA                     
007600     SKIP3                                                                
007700*01  RP5  -COPY W222RP5TC0 -PRE RP5- -RED LINK-UTAREA                     
007900     SKIP3                                                                
008000*01  RP6  -COPY W222RP6TC0 -PRE RP6- -RED LINK-UTAREA                     
008200     SKIP3                                                                
008300*01  RP7 -COPY W222RP7TC0 -PRE RP7- -RED LINK-UTAREA                      
008500     EJECT                                                                
008600 PROCEDURE DIVISION USING LINK-AREA                                       
008700     LINK-IDPTYP-UTAREA LINK-UTAREA.                                      
008800     SKIP3                                                                
008900     IF SW-NY-R89 = JA                                                    
009000       MOVE NEJ TO SW-NY-R89                                              
009100       MOVE LINK-W222R89T TO R89-W222R89T                                 
009200       MOVE LINK-KDCLAGER TO W-KDCLAGER                                   
009300       MOVE LINK-IDARTNR TO W-IDARTNR                                     
009400       MOVE SPACE TO R89-IDPTYP                                           
009500       MOVE SPACE TO R89-KDCLAGER                                         
009600       MOVE SPACE TO R89-IDARTNR                                          
009700       MOVE NEJ TO SW-IFYLLD-RESEASON                                     
009800       MOVE NEJ TO SW-IFYLLD-TREND                                        
009900       MOVE NEJ TO SW-IFYLLD-PBJUST                                       
010000     END-IF                                                               
010100                                                                          
010200     IF R89-W222R89T NOT = SPACE                                          
010300       IF R89-KVPB-SEP NOT = SPACE                                        
010400         MOVE 'RP1' TO RP1-IDPTYP                                         
010500         MOVE W-IDARTNR TO RP1-IDARTNR                                    
010600         MOVE W-KDCLAGER TO RP1-KDCLAGER                                  
010700         MOVE 'RP1' TO LINK-IDPTYP-UTAREA                                 
010800*      OBS DECIMALEN                                                      
010900         MOVE R89-KVPB-SEP TO RP1-KVPB-SEP-X                              
011000*                                                                         
011100         MOVE SPACE TO R89-KVPB-SEP                                       
011200       ELSE                                                               
011300         IF R89-FLMPB NOT = SPACE                                         
011400           MOVE 'RP6' TO RP6-IDPTYP                                       
011500           MOVE W-IDARTNR TO RP6-IDARTNR                                  
011600           MOVE W-KDCLAGER TO RP6-KDCLAGER                                
011700           MOVE R89-FLMPB TO RP6-FLMPB                                    
011800           MOVE SPACE TO R89-FLMPB                                        
011900           MOVE 'RP6' TO LINK-IDPTYP-UTAREA                               
012000         ELSE                                                             
012100           IF R89-FLOREGPB NOT = SPACE                                    
012200             MOVE 'RP7' TO RP7-IDPTYP                                     
012300             MOVE W-IDARTNR TO RP7-IDARTNR                                
012400             MOVE W-KDCLAGER TO RP7-KDCLAGER                              
012500             MOVE R89-FLOREGPB TO RP7-FLOREGPB                            
012600             MOVE SPACE TO R89-FLOREGPB                                   
012700             MOVE 'RP7' TO LINK-IDPTYP-UTAREA                             
012800           ELSE                                                           
012900             IF R89-TREND NOT = SPACE                                     
013000               MOVE JA TO SW-IFYLLD-TREND                                 
013100               MOVE 'RP2' TO RP2-IDPTYP                                   
013200               MOVE W-IDARTNR TO RP2-IDARTNR                              
013300               MOVE W-KDCLAGER TO RP2-KDCLAGER                            
013400***    OBS DECIMALEN                                                      
013500               IF R89-KVTREND = SPACE                                     
013600                 MOVE ZERO TO RP2-KVTREND-X                               
013700               ELSE                                                       
013800                 MOVE R89-KVTREND TO RP2-KVTREND-X                        
013900               END-IF                                                     
014000               IF R89-FLNEGTR = JA                                        
014100                 MOVE JA TO RP2-FLNEGTR                                   
014200               ELSE                                                       
014300                 MOVE NEJ TO RP2-FLNEGTR                                  
014400               END-IF                                                     
014500               IF R89-TITREND = SPACE                                     
014600                 MOVE ZERO TO RP2-TITREND-X                               
014700               ELSE                                                       
014800                 MOVE R89-TITREND TO RP2-TITREND-X                        
014900               END-IF                                                     
015000               IF R89-RVTREND = SPACE                                     
015100                 MOVE ZERO TO RP2-RVTREND-X                               
015200               ELSE                                                       
015300                 MOVE R89-RVTREND TO W-RVTREND-POS2                       
015400                 MOVE W-RVTREND TO RP2-RVTREND-X                          
015500               END-IF                                                     
015600               MOVE NEJ TO RP2-FLABORT-TREND                              
015700               MOVE 'RP2' TO LINK-IDPTYP-UTAREA                           
015800               MOVE SPACE TO R89-TREND                                    
015900             ELSE                                                         
016000               IF R89-PB-JUSTERINGAR (1) NOT = SPACE                      
016100               OR R89-PB-JUSTERINGAR (2) NOT = SPACE                      
016200                 MOVE JA TO SW-IFYLLD-PBJUST                              
016300                 MOVE 'RP4' TO RP4-IDPTYP                                 
016400                 MOVE W-IDARTNR TO RP4-IDARTNR                            
016500                 MOVE W-KDCLAGER TO RP4-KDCLAGER                          
016600*  OBS DECIMALEN                                                          
016700                 MOVE R89-KVPB-JUST (1) TO RP4-KVPB-JUST-X (1)            
016800                 MOVE R89-KVPB-JUST (2) TO RP4-KVPB-JUST-X (2)            
016900*                                                                         
017000                 MOVE R89-TIPBJUST (1) TO RP4-TIPBJUST-X (1)              
017100                 MOVE R89-TIPBJUST (2) TO RP4-TIPBJUST-X (2)              
017200                 MOVE NEJ TO RP4-FLABORT-PBJUST                           
017300                 MOVE 'RP4' TO LINK-IDPTYP-UTAREA                         
017400                 MOVE SPACE TO R89-PB-JUSTERINGAR (1)                     
017500                 MOVE SPACE TO R89-PB-JUSTERINGAR (2)                     
017600               ELSE                                                       
017700                 IF R89-RESEASON (1) NOT = SPACE                          
017800                   MOVE JA TO SW-IFYLLD-RESEASON                          
017900                   MOVE +1 TO IX                                          
018000                   PERFORM UNTIL                                          
018100                    ( IX GREATER 12 )                                     
018200                     MOVE ZERO TO RP3-RESEASON (IX)                       
018300                     ADD +1 TO IX                                         
018400                   END-PERFORM                                            
018500                   MOVE 'RP3' TO RP3-IDPTYP                               
018600                   MOVE W-IDARTNR TO RP3-IDARTNR                          
018700                   MOVE W-KDCLAGER TO RP3-KDCLAGER                        
018800                   MOVE +1 TO IX                                          
018900                   PERFORM UNTIL                                          
019000                    ( IX GREATER 12 )                                     
019100                     IF R89-RESEASON (IX) NUMERIC                         
019200                       MOVE R89-RESEASON (IX) TO W-RESEASON-1-2           
019300                       MOVE W-RESEASON TO RP3-RESEASON (IX)               
019700                     END-IF                                               
019800                     ADD +1 TO IX                                         
019900                   END-PERFORM                                            
020000                   MOVE NEJ TO RP3-FLABORT-SEASON                         
020100                   MOVE 'RP3' TO LINK-IDPTYP-UTAREA                       
020200                   MOVE +1 TO IX                                          
020300                   MOVE SPACE TO R89-RESEASON (1)                         
020400                 ELSE                                                     
020500                   IF R89-KDBEH-PROG NUMERIC                              
020600                   AND (R89-KDBEH-PROG = '1'                              
020700                   OR R89-KDBEH-PROG = '3'                                
020800                   OR R89-KDBEH-PROG = '5'                                
020900                   OR R89-KDBEH-PROG = '7')                               
021000                   AND SW-IFYLLD-TREND = NEJ                              
021100                     MOVE JA TO SW-IFYLLD-TREND                           
021200                     MOVE SPACE TO RP2-W222RP2T                           
021300                     MOVE 'RP2' TO RP2-IDPTYP                             
021400                     MOVE W-IDARTNR TO RP2-IDARTNR                        
021500                     MOVE W-KDCLAGER TO RP2-KDCLAGER                      
021600                     MOVE JA TO RP2-FLABORT-TREND                         
021700                     MOVE ZERO  TO R89-KDBEH-PROG                         
021800                     MOVE 'RP2' TO LINK-IDPTYP-UTAREA                     
021900                   ELSE                                                   
022000                     IF (R89-KDBEH-PROG = '2'                             
022100                     OR R89-KDBEH-PROG = '3'                              
022200                     OR R89-KDBEH-PROG = '6'                              
022300                     OR R89-KDBEH-PROG = '7')                             
022400                     AND SW-IFYLLD-PBJUST = NEJ                           
022500                       MOVE JA TO SW-IFYLLD-PBJUST                        
022600                       MOVE SPACE TO RP4-W222RP4T                         
022700                       MOVE 'RP4' TO RP4-IDPTYP                           
022800                       MOVE W-IDARTNR TO RP4-IDARTNR                      
022900                       MOVE W-KDCLAGER TO RP4-KDCLAGER                    
023000                       MOVE JA TO RP4-FLABORT-PBJUST                      
023100                       MOVE 'RP4' TO LINK-IDPTYP-UTAREA                   
023200                     ELSE                                                 
023300                       IF (R89-KDBEH-PROG = '4'                           
023400                       OR R89-KDBEH-PROG = '5'                            
023500                       OR R89-KDBEH-PROG = '6'                            
023600                       OR R89-KDBEH-PROG = '7')                           
023700                       AND SW-IFYLLD-RESEASON = NEJ                       
023800                         MOVE JA TO SW-IFYLLD-RESEASON                    
023900                         MOVE SPACE TO RP3-W222RP3T-CTX                   
024000                         MOVE 'RP3' TO RP3-IDPTYP                         
024100                         MOVE W-IDARTNR TO RP3-IDARTNR                    
024200                         MOVE W-KDCLAGER TO RP3-KDCLAGER                  
024300                         MOVE JA TO RP3-FLABORT-SEASON                    
024400                         MOVE 'RP3' TO LINK-IDPTYP-UTAREA                 
024500                       ELSE                                               
024600                         MOVE SPACE TO LINK-IDPTYP-UTAREA                 
024700                         MOVE ZERO  TO R89-KDBEH-PROG                     
024800                         MOVE JA TO SW-NY-R89                             
024900                       END-IF                                             
025000                     END-IF                                               
025100                   END-IF                                                 
025200                 END-IF                                                   
025300               END-IF                                                     
025400             END-IF                                                       
025500           END-IF                                                         
025600         END-IF                                                           
025700       END-IF                                                             
025800     ELSE                                                                 
025900       MOVE SPACE TO LINK-IDPTYP-UTAREA                                   
026000       MOVE JA TO SW-NY-R89                                               
026100     END-IF                                                               
026200                                                                          
026300     MOVE ZERO TO RETURN-CODE                                             
026400     GOBACK                                                               
026500     .                                                                    

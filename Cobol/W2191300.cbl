000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2191300.                                                
000400 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500 DATE-WRITTEN.   APRIL 2003.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000900*    FUNKTION:                                                            
001000*        MATCHAR KAMPANJER FRÅN QW90                                      
001100*        AKTUELL VECKA MOT FÖREGÅENDE VECKAS                              
001200*        (POSTTYP = Q9A / ARTIKELINFO)                                    
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600     SELECT W21911NY                   ASSIGN TO W21913D1.                
002700     SKIP2                                                                
002800     SELECT W21911GAM                  ASSIGN TO W21913D2.                
002900*                                                                         
003000     SELECT W21913UT                   ASSIGN TO W21913D3.                
003100                                                                          
003200                                                                          
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W21911NY                                                             
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST     -COPY W21911  -PRE W21911NY-    -L                          
004300     SKIP3                                                                
004400 FD  W21911GAM                                                            
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  POST     -COPY W21911  -PRE W21911GAM-    -L                         
004900     SKIP3                                                                
005000 FD  W21913UT                                                             
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY W21913 -PRE  W21913UT-     -L.                            
005500                                                                          
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800     SKIP2                                                                
005900                                                                          
006000*    -- CHECKED BY WY2000                                                 
006100 77  IDPGM                       PIC X(8)    VALUE 'W2191300'.            
006200 77  JA                          PIC X       VALUE 'J'.                   
006300 77  NEJ                         PIC X       VALUE 'N'.                   
006400     SKIP2                                                                
006500                                                                          
006600 01 WS-IDNY.                                                              
006700  03 WS-IDNY-IDKAMP             PIC X(7)   VALUE SPACE.                   
006800  03 WS-IDNY-IDARTNR            PIC 9(9)   VALUE ZERO.                    
006900 01 WS-IDGAM.                                                             
007000  03 WS-IDGAM-IDKAMP            PIC X(7)   VALUE SPACE.                   
007100  03 WS-IDGAM-IDARTNR           PIC 9(9)   VALUE ZERO.                    
007200 01 WS-ANTAL-W21911NY           PIC 9(9)   VALUE ZERO.                    
007300 01 WS-ANTAL-W21911GAM          PIC 9(9)   VALUE ZERO.                    
007400 01 WS-ANTAL-W21913UT           PIC 9(9)   VALUE ZERO.                    
007500 01 WS-ANTAL-DISPLAY            PIC 9(9)   VALUE ZERO.                    
007600                                                                          
007700                                                                          
007800 01  FELTEXT.                                                             
007900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008100                                                                          
008200 77  W21911NY-EOF-SW             PIC X       VALUE 'N'.                   
008300     88  END-OF-W21911NY                     VALUE 'J'.                   
008400                                                                          
008500 77  W21911GAM-EOF-SW            PIC X       VALUE 'N'.                   
008600     88  END-OF-W21911GAM                    VALUE 'J'.                   
008700     EJECT                                                                
008800*      --- VALID IDDC CODES                                               
008900*                                                                         
009000*01    -COPY WWDC99                                                       
009100                                                                          
009200     EJECT                                                                
009300 01  DYNAMISKA-SUBPROGRAM.                                                
009400*                                                                         
009500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009900     EJECT                                                                
010000*    ---- PARAMETRAR TILL WDATKONV                                        
010100 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
010200*01  -COPY WDATAREA                                                       
010300*    --- PARAMETRAR TILL POSTSUM                                          
010400*                                                                         
010500*01  -COPY W0005   -PRE  POSTSUM-                                         
010600     EJECT                                                                
010700 01  W21911NY-AREA-START         PIC X(24)   VALUE                        
010800                                 'W21911NY-AREA-START  '.                 
010900     SKIP2                                                                
011000                                                                          
011100 01  W21911NY-AREA.                                                       
011200         05  -COPY W21911   -PRE W21911NY-                                
011300                                                                          
011400     EJECT                                                                
011500 01  W21911GAM-AREA-START        PIC X(24)   VALUE                        
011600                                 'W21911GAM-AREA-START  '.                
011700     SKIP2                                                                
011800                                                                          
011900 01  W21911GAM-AREA.                                                      
012000         05  -COPY W21911   -PRE W21911GAM-                               
012100                                                                          
012200     EJECT                                                                
012300 01  W21913UT-AREA-START         PIC X(24)   VALUE                        
012400                                 'W21913UT-AREA-START  '.                 
012500     SKIP2                                                                
012600                                                                          
012700 01  W21913UT-AREA.                                                       
012800         05  -COPY W21913   -PRE W21913UT-                                
012900                                                                          
013000     EJECT                                                                
013100                                                                          
013200     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400                                                                          
013500 PROCEDURE DIVISION.                                                      
013600                                                                          
013700     SKIP2                                                                
013800     PERFORM A-INIT                                                       
013900                                                                          
014000     PERFORM S01-LAES-W21911NY                                            
014100     PERFORM S02-LAES-W21911GAM                                           
014200                                                                          
014300     PERFORM UNTIL END-OF-W21911NY                                        
014400     AND           END-OF-W21911GAM                                       
014500       PERFORM B-BEARBETA                                                 
014600     END-PERFORM                                                          
014700                                                                          
014800     PERFORM Z-FINIT                                                      
014900                                                                          
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500                                                                          
015600     OPEN INPUT  W21911NY                                                 
015700                 W21911GAM                                                
015800                                                                          
015900     OPEN OUTPUT W21913UT                                                 
016000                                                                          
016100                                                                          
016200     .                                                                    
016300     EJECT                                                                
016400 B-BEARBETA SECTION.                                                      
016500**** READ ALL W21911NY WHEN W21911GAM IS COMPLETED                        
016600     IF END-OF-W21911GAM                                                  
016700       MOVE W21911NY-IDPTYP  TO W21913UT-IDPTYP                           
016800       MOVE 'N'              TO W21913UT-KDSTATUS-KAMP                    
016900       MOVE W21911NY-IDKAMP  TO W21913UT-IDKAMP                           
017000       MOVE W21911NY-IDARTNR TO W21913UT-IDARTNR                          
017100       MOVE W21911NY-KVREPANT                                             
017200                             TO W21913UT-KVREPANT                         
017300       PERFORM S11-SKRIV-W21913UT                                         
017400       PERFORM S01-LAES-W21911NY                                          
017500     ELSE                                                                 
017600**** READ ALL W21911GAM WHEN W21911NY IS COMPLETED                        
017700       IF END-OF-W21911NY                                                 
017800         MOVE W21911GAM-IDPTYP                                            
017900                             TO W21913UT-IDPTYP                           
018000         MOVE 'D'            TO W21913UT-KDSTATUS-KAMP                    
018100         MOVE W21911GAM-IDKAMP                                            
018200                             TO W21913UT-IDKAMP                           
018300         MOVE W21911GAM-IDARTNR                                           
018400                             TO W21913UT-IDARTNR                          
018500         MOVE W21911GAM-KVREPANT                                          
018600                             TO W21913UT-KVREPANT                         
018700         PERFORM S11-SKRIV-W21913UT                                       
018800         PERFORM S02-LAES-W21911GAM                                       
018900       ELSE                                                               
019000         IF WS-IDGAM > WS-IDNY                                            
019100           MOVE W21911NY-IDPTYP  TO W21913UT-IDPTYP                       
019200           MOVE 'N'              TO W21913UT-KDSTATUS-KAMP                
019300           MOVE W21911NY-IDKAMP  TO W21913UT-IDKAMP                       
019400           MOVE W21911NY-IDARTNR TO W21913UT-IDARTNR                      
019500           MOVE W21911NY-KVREPANT                                         
019600                                 TO W21913UT-KVREPANT                     
019700           PERFORM S11-SKRIV-W21913UT                                     
019800           PERFORM S01-LAES-W21911NY                                      
019900         ELSE                                                             
020000           IF WS-IDNY > WS-IDGAM                                          
020100             MOVE W21911GAM-IDPTYP                                        
020200                                 TO W21913UT-IDPTYP                       
020300             MOVE 'D'            TO W21913UT-KDSTATUS-KAMP                
020400             MOVE W21911GAM-IDKAMP                                        
020500                                 TO W21913UT-IDKAMP                       
020600             MOVE W21911GAM-IDARTNR                                       
020700                                 TO W21913UT-IDARTNR                      
020800             MOVE W21911GAM-KVREPANT                                      
020900                                 TO W21913UT-KVREPANT                     
021000             PERFORM S11-SKRIV-W21913UT                                   
021100             PERFORM S02-LAES-W21911GAM                                   
021200           ELSE                                                           
021300             IF W21911NY-KVREPANT NOT = W21911GAM-KVREPANT                
021400               MOVE W21911NY-IDPTYP                                       
021500                                 TO W21913UT-IDPTYP                       
021600               MOVE 'C'          TO W21913UT-KDSTATUS-KAMP                
021700               MOVE W21911NY-IDKAMP                                       
021800                                 TO W21913UT-IDKAMP                       
021900               MOVE W21911NY-IDARTNR                                      
022000                                 TO W21913UT-IDARTNR                      
022100               MOVE W21911NY-KVREPANT                                     
022200                                 TO W21913UT-KVREPANT                     
022300               PERFORM S11-SKRIV-W21913UT                                 
022400             END-IF                                                       
022500             PERFORM S01-LAES-W21911NY                                    
022600             PERFORM S02-LAES-W21911GAM                                   
022700           END-IF                                                         
022800         END-IF                                                           
022900       END-IF                                                             
023000     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
023300                                                                          
023400 Z-FINIT SECTION.                                                         
023500                                                                          
023600     DISPLAY 'WS-ANTAL-W21911NY   : '   WS-ANTAL-W21911NY                 
023700     DISPLAY 'WS-ANTAL-W21911GAM   : '  WS-ANTAL-W21911GAM                
023800     DISPLAY 'WS-ANTAL-W21913UT  : '    WS-ANTAL-W21913UT                 
023900     CLOSE W21911NY                                                       
024000           W21911GAM                                                      
024100     CLOSE W21913UT                                                       
024200     SKIP2                                                                
024300     .                                                                    
024400     EJECT                                                                
024500 S01-LAES-W21911NY SECTION.                                               
024600                                                                          
024700     READ W21911NY           INTO W21911NY-AREA                           
024800     AT END                                                               
024900        SET END-OF-W21911NY  TO TRUE                                      
025000     NOT AT END                                                           
025100        ADD 1                TO WS-ANTAL-W21911NY                         
025200        MOVE W21911NY-IDKAMP                                              
025300                             TO WS-IDNY-IDKAMP                            
025400        MOVE W21911NY-IDARTNR                                             
025500                             TO WS-IDNY-IDARTNR                           
025600     END-READ                                                             
025700     .                                                                    
025800     EJECT                                                                
025900 S02-LAES-W21911GAM SECTION.                                              
026000                                                                          
026100     READ W21911GAM       INTO W21911GAM-AREA                             
026200     AT END                                                               
026300        SET END-OF-W21911GAM TO TRUE                                      
026400     NOT AT END                                                           
026500        ADD 1             TO WS-ANTAL-W21911GAM                           
026600        MOVE W21911GAM-IDKAMP                                             
026700                             TO WS-IDGAM-IDKAMP                           
026800        MOVE W21911GAM-IDARTNR                                            
026900                             TO WS-IDGAM-IDARTNR                          
027000                                                                          
027100     END-READ                                                             
027200     .                                                                    
027300     EJECT                                                                
027400 S11-SKRIV-W21913UT SECTION.                                              
027500                                                                          
027600     WRITE W21913UT-POST FROM W21913UT-AREA                               
027700     ADD 1                   TO WS-ANTAL-W21913UT                         
027800     .                                                                    

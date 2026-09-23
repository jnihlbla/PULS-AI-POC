000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W5109400.                                                 
000300                                                                          
000400*    AUTHOR.        ANDERS HENRIKSSON.                                    
000500*    DATE-WRITTEN   AUG     2010.                                         
000600*                                                                         
000700**** MATCHA TVÅ FILER OCH SKRIV SW DÄR DET BEHÖVS                         
000800*                                                                         
000900     EJECT                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400                                                                          
001500     SELECT W51072A ASSIGN       TO W51094D1.                             
001600                                                                          
001700     SELECT W51073B ASSIGN       TO W51094D2.                             
001800                                                                          
001810     SELECT W51072  ASSIGN       TO W51094D3.                             
001820                                                                          
001830     SELECT W51073  ASSIGN       TO W51094D4.                             
001840                                                                          
001900     EJECT                                                                
002000 DATA DIVISION.                                                           
002100 FILE SECTION.                                                            
002200                                                                          
002300 FD  W51072A                                                              
002400     RECORDING       F                                                    
002500     BLOCK CONTAINS  0.                                                   
002700*01  71LINE-POST -COPY R3LINE20  -PRE  72A-   -L.                         
002800                                                                          
002810 FD  W51073B                                                              
002820     RECORDING       V                                                    
002830     BLOCK CONTAINS  0.                                                   
002840*01  71HEAD-POST -COPY R3HEAD20  -PRE 73B-    -L.                         
002841*01  71LINE-POST -COPY R3LINE20  -PRE 73B-    -L.                         
002850                                                                          
002900 FD  W51072                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003300*01  71LINE-POST -COPY R3LINE20   -PRE  72-   -L.                         
003400     SKIP2                                                                
003500                                                                          
003560 FD  W51073                                                               
003570     RECORDING       V                                                    
003580     BLOCK CONTAINS  0.                                                   
003590*01  71HEAD-POST -COPY R3HEAD20   -PRE  73-   -L.                         
003591*01  71LINE-POST -COPY R3LINE20   -PRE  73-   -L.                         
003592     SKIP2                                                                
003593                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700     SKIP3                                                                
003800 77  IDPGM                   PIC X(8)      VALUE 'W1220900'.              
003900 77  JA                      PIC X         VALUE 'J'.                     
004000 77  NEJ                     PIC X         VALUE 'N'.                     
004100 77  EOF-W51072A             PIC X         VALUE 'N'.                     
004110 77  EOF-W51073B             PIC X         VALUE 'N'.                     
004200 77  LINE-COUNT              PIC S9(3)     VALUE +1 COMP SYNC.            
004300                                                                          
004400 01  SUBPROGRAM.                                                          
004500     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
004600                                                                          
004700 01  W51072A-TRANSID.                                                     
004800     03  FILLER              PIC X(6) VALUE 'W51072'.                     
004900     03  FILLER              PIC X(8) VALUE 'W51094D1'.                   
005000     03  FILLER              PIC X(4) VALUE ' IN1'.                       
005100                                                                          
005200 01  W51072-TRANSID.                                                      
005300     03  FILLER              PIC X(6) VALUE 'W51072'.                     
005400     03  FILLER              PIC X(8) VALUE 'W51094D3'.                   
005500     03  FILLER              PIC X(4) VALUE ' UT2'.                       
005600                                                                          
005610 01  W51073B-TRANSID.                                                     
005620     03  FILLER              PIC X(6) VALUE 'W51073'.                     
005630     03  FILLER              PIC X(8) VALUE 'W51094D2'.                   
005640     03  FILLER              PIC X(4) VALUE ' IN2'.                       
005650                                                                          
005660 01  W51073-TRANSID.                                                      
005670     03  FILLER              PIC X(6) VALUE 'W51073'.                     
005680     03  FILLER              PIC X(8) VALUE 'W51094D4'.                   
005690     03  FILLER              PIC X(4) VALUE ' UT3'.                       
005691                                                                          
005700     EJECT                                                                
005800*   -COPY W0005  -PRE POSTSUM-                                            
005900     EJECT                                                                
006000                                                                          
006100 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
006200 01  INAREA1.                                                             
006300*03  -COPY R3LINE20              -PRE INA-                                
006400     EJECT                                                                
006500                                                                          
006600 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
006700 01  INAREA2.                                                             
006800*03  -COPY R3HEAD20              -PRE IN-                                 
006900     EJECT                                                                
007000                                                                          
007010 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
007020 01  INAREA3.                                                             
007030*03  -COPY R3LINE20              -PRE IN-                                 
007040     EJECT                                                                
007050                                                                          
007100 01  FILLER                  PIC X(16)   VALUE 'SAVE-AREA1 '.             
007200 01  SAVEAREA1.                                                           
007300*03  -COPY R3HEAD20              -PRE SAVE-                               
007400     EJECT                                                                
007500                                                                          
007600 01  FILLER                  PIC X(16)   VALUE 'SAVE-AREA1 '.             
007700 01  SAVEAREA2.                                                           
007800*03  -COPY R3LINE20              -PRE SAVE-                               
007900     EJECT                                                                
008000                                                                          
008010 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
008020 01  UTAREA1.                                                             
008030*03  -COPY R3LINE20              -PRE UTA-                                
008040     EJECT                                                                
008050                                                                          
008100 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
008200 01  UTAREA2.                                                             
008300*03  -COPY R3HEAD20              -PRE UT-                                 
008400     EJECT                                                                
008500                                                                          
008600 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
008700 01  UTAREA3.                                                             
008800*03  -COPY R3LINE20              -PRE UT-                                 
008900     EJECT                                                                
009000                                                                          
009100 PROCEDURE DIVISION.                                                      
009200 MAIN SECTION.                                                            
009300                                                                          
009400     PERFORM A-INIT                                                       
009500                                                                          
009600     PERFORM S01-READ-W51072A-POST                                        
009610     PERFORM S01-READ-W51073B-POST                                        
009700     PERFORM UNTIL  EOF-W51072A = JA AND EOF-W51073B = JA                 
009701       IF EOF-W51072A = JA                                                
009702         MOVE INAREA3       TO INAREA2                                    
009703         IF IN-HEAD-RECORD-TYPE = '200'                                   
009704         OR IN-HEAD-RECORD-TYPE = '300'                                   
009705         OR IN-HEAD-RECORD-TYPE = '600'                                   
009706           MOVE INAREA2 TO UTAREA2                                        
009707           PERFORM S06-CREATE-FILE-W51073-HEAD                            
009709         END-IF                                                           
009710         IF IN-LINE-RECORD-TYPE = '210'                                   
009711         OR IN-LINE-RECORD-TYPE = '310'                                   
009712         OR IN-LINE-RECORD-TYPE = '610'                                   
009713           MOVE INAREA3 TO UTAREA3                                        
009714           PERFORM S06-CREATE-FILE-W51073-LINE                            
009715         END-IF                                                           
009716         PERFORM S01-READ-W51073B-POST                                    
009718       ELSE                                                               
009719         IF EOF-W51073B = JA                                              
009720           MOVE INAREA1 TO UTAREA1                                        
009721           PERFORM S06-CREATE-FILE-W51072-LINE                            
009722           PERFORM S01-READ-W51072A-POST                                  
009723         ELSE                                                             
009724           MOVE INAREA3       TO INAREA2                                  
009725           IF INA-LINE-DOCUMENT-NO-REF = IN-LINE-DOCUMENT-NO-REF          
009726             IF INA-LINE-TEXT(7:2) = 'SW'                                 
009727               IF IN-HEAD-RECORD-TYPE = '200'                             
009728               OR IN-HEAD-RECORD-TYPE = '300'                             
009729               OR IN-HEAD-RECORD-TYPE = '600'                             
009730                 MOVE INAREA2 TO UTAREA2                                  
009731                 MOVE 'SW' TO UT-HEAD-TEXT(17:2)                          
009732                 PERFORM S06-CREATE-FILE-W51073-HEAD                      
009733               END-IF                                                     
009740               IF IN-LINE-RECORD-TYPE = '210'                             
009750               OR IN-LINE-RECORD-TYPE = '310'                             
009760               OR IN-LINE-RECORD-TYPE = '610'                             
009770                 MOVE INAREA3 TO UTAREA3                                  
009771                 MOVE 'SW' TO UT-LINE-TEXT(7:2)                           
009780                 PERFORM S06-CREATE-FILE-W51073-LINE                      
009790               END-IF                                                     
009800             ELSE                                                         
009900               IF IN-HEAD-RECORD-TYPE = '200'                             
010000               OR IN-HEAD-RECORD-TYPE = '300'                             
010100               OR IN-HEAD-RECORD-TYPE = '600'                             
010200                 MOVE INAREA2 TO UTAREA2                                  
010300                 PERFORM S06-CREATE-FILE-W51073-HEAD                      
010400               END-IF                                                     
010500               IF IN-LINE-RECORD-TYPE = '210'                             
010510               OR IN-LINE-RECORD-TYPE = '310'                             
010520               OR IN-LINE-RECORD-TYPE = '610'                             
010530                 MOVE INAREA3 TO UTAREA3                                  
010540                 PERFORM S06-CREATE-FILE-W51073-LINE                      
010550               END-IF                                                     
010600             END-IF                                                       
010700             PERFORM S01-READ-W51073B-POST                                
012200           ELSE                                                           
012300             IF INA-LINE-DOCUMENT-NO-REF > IN-LINE-DOCUMENT-NO-REF        
012301               IF IN-HEAD-RECORD-TYPE = '200'                             
012302               OR IN-HEAD-RECORD-TYPE = '300'                             
012303               OR IN-HEAD-RECORD-TYPE = '600'                             
012304                 MOVE INAREA2 TO UTAREA2                                  
012305                 PERFORM S06-CREATE-FILE-W51073-HEAD                      
012306               END-IF                                                     
012307               IF IN-LINE-RECORD-TYPE = '210'                             
012308               OR IN-LINE-RECORD-TYPE = '310'                             
012309               OR IN-LINE-RECORD-TYPE = '610'                             
012311                 MOVE INAREA3 TO UTAREA3                                  
012312                 PERFORM S06-CREATE-FILE-W51073-LINE                      
012313               END-IF                                                     
012314               PERFORM S01-READ-W51073B-POST                              
012315             ELSE                                                         
012316             IF INA-LINE-DOCUMENT-NO-REF < IN-LINE-DOCUMENT-NO-REF        
012317                 MOVE INAREA1 TO UTAREA1                                  
012318                 PERFORM S06-CREATE-FILE-W51072-LINE                      
012319               END-IF                                                     
012320               PERFORM S01-READ-W51072A-POST                              
012321             END-IF                                                       
012330           END-IF                                                         
012340         END-IF                                                           
012350       END-IF                                                             
012400     END-PERFORM                                                          
012500                                                                          
012600     PERFORM Z-END                                                        
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200                                                                          
013300 A-INIT SECTION.                                                          
013400     OPEN INPUT  W51072A                                                  
013410                 W51073B                                                  
013500     OPEN OUTPUT W51072                                                   
013510                 W51073                                                   
013600                                                                          
013700     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
013800     .                                                                    
013900     EJECT                                                                
014000                                                                          
014100 Z-END SECTION.                                                           
014200     CLOSE W51072A                                                        
014210           W51073B                                                        
014300           W51072                                                         
014310           W51073                                                         
014400                                                                          
014500     MOVE 'S'        TO POSTSUM-OPKOD                                     
014600     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014800     EJECT                                                                
014900                                                                          
015000 S01-READ-W51072A-POST SECTION.                                           
015100     READ W51072A INTO INAREA1                                            
015200     AT END                                                               
015300       MOVE JA TO EOF-W51072A                                             
015400     NOT AT END                                                           
015500       MOVE W51072A-TRANSID TO POSTSUM-TRANSID                            
015600       CALL POSTSUM USING POSTSUM-PARM                                    
015700     END-READ                                                             
015800     .                                                                    
015900     SKIP2                                                                
016000                                                                          
016010 S01-READ-W51073B-POST SECTION.                                           
016020     READ W51073B INTO INAREA3                                            
016030     AT END                                                               
016040       MOVE JA TO EOF-W51073B                                             
016050     NOT AT END                                                           
016060       MOVE W51073B-TRANSID TO POSTSUM-TRANSID                            
016070       CALL POSTSUM USING POSTSUM-PARM                                    
016080     END-READ                                                             
016090     .                                                                    
016091     SKIP2                                                                
016092                                                                          
016093 S06-CREATE-FILE-W51072-LINE SECTION.                                     
016094     WRITE 72-71LINE-POST  FROM UTAREA1                                   
016095     MOVE W51072-TRANSID TO POSTSUM-TRANSID                               
016096     CALL POSTSUM USING POSTSUM-PARM                                      
016097     .                                                                    
016098     EJECT                                                                
016099                                                                          
016100 S06-CREATE-FILE-W51073-HEAD SECTION.                                     
016200     WRITE 73-71HEAD-POST  FROM UTAREA2                                   
016300     MOVE W51073-TRANSID TO POSTSUM-TRANSID                               
016400     CALL POSTSUM USING POSTSUM-PARM                                      
016500     .                                                                    
016600     EJECT                                                                
016700                                                                          
016800 S06-CREATE-FILE-W51073-LINE SECTION.                                     
016900     WRITE 73-71LINE-POST  FROM UTAREA3                                   
017000     MOVE W51073-TRANSID TO POSTSUM-TRANSID                               
017100     CALL POSTSUM USING POSTSUM-PARM                                      
017200     .                                                                    
017300     EJECT                                                                

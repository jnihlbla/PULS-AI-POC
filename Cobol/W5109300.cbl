000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W5109300.                                                 
000300                                                                          
000400*    AUTHOR.        ANDERS HENRIKSSON.                                    
000500*    DATE-WRITTEN   JUNE    2010.                                         
000600*                                                                         
000700**** SAP BEGRÄNSNINGAR SOM VI MÅSTE TA HÄNSYN TILL                        
000800*                                                                         
000900     EJECT                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400                                                                          
001500     SELECT W51071B ASSIGN       TO W51093D1.                             
001600                                                                          
001700     SELECT W51071  ASSIGN       TO W51093D2.                             
001800                                                                          
001900     EJECT                                                                
002000 DATA DIVISION.                                                           
002100 FILE SECTION.                                                            
002200                                                                          
002300 FD  W51071B                                                              
002400     RECORDING       V                                                    
002500     BLOCK CONTAINS  0.                                                   
002600*01  71HEAD-POST -COPY R3HEAD20               -L.                         
002700*01  71LINE-POST -COPY R3LINE20               -L.                         
002800                                                                          
002900 FD  W51071                                                               
003000     RECORDING       V                                                    
003100     BLOCK CONTAINS  0.                                                   
003200*01  71HEAD-POST -COPY R3HEAD20   -PRE  R3-   -L.                         
003300*01  71LINE-POST -COPY R3LINE20   -PRE  R3-   -L.                         
003400     SKIP2                                                                
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700     SKIP3                                                                
003800 77  IDPGM                   PIC X(8)      VALUE 'W1220900'.              
003900 77  JA                      PIC X         VALUE 'J'.                     
004000 77  NEJ                     PIC X         VALUE 'N'.                     
004100 77  EOF-W51071B             PIC X         VALUE 'N'.                     
004200 77  LINE-COUNT              PIC S9(3)     VALUE +1 COMP SYNC.            
004300                                                                          
004400 01  SUBPROGRAM.                                                          
004500     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
004600                                                                          
004700 01  W51071B-TRANSID.                                                     
004800     03  FILLER              PIC X(6) VALUE 'W51071'.                     
004900     03  FILLER              PIC X(8) VALUE 'W51093D1'.                   
005000     03  FILLER              PIC X(4) VALUE ' IN1'.                       
005100                                                                          
005200 01  W51071-TRANSID.                                                      
005300     03  FILLER              PIC X(6) VALUE 'W51071'.                     
005400     03  FILLER              PIC X(8) VALUE 'W51093D4'.                   
005500     03  FILLER              PIC X(4) VALUE ' UT2'.                       
005600                                                                          
005700     EJECT                                                                
005800*   -COPY W0005  -PRE POSTSUM-                                            
005900     EJECT                                                                
006000                                                                          
006100 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
006200 01  INAREA1.                                                             
006300*03  -COPY R3HEAD20              -PRE IN-                                 
006400     EJECT                                                                
006500                                                                          
006600 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
006700 01  INAREA2.                                                             
006800*03  -COPY R3LINE20              -PRE IN-                                 
006900     EJECT                                                                
007000                                                                          
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
008100 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
008200 01  UTAREA1.                                                             
008300*03  -COPY R3HEAD20              -PRE UT-                                 
008400     EJECT                                                                
008500                                                                          
008600 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
008700 01  UTAREA2.                                                             
008800*03  -COPY R3LINE20              -PRE UT-                                 
008900     EJECT                                                                
009000                                                                          
009100 PROCEDURE DIVISION.                                                      
009200 MAIN SECTION.                                                            
009300                                                                          
009400     PERFORM A-INIT                                                       
009500                                                                          
009600     PERFORM S01-READ-W51071B-POST                                        
009700     PERFORM UNTIL  EOF-W51071B = JA                                      
009800       MOVE INAREA2           TO INAREA1                                  
009900       IF IN-HEAD-RECORD-TYPE = '200'                                     
010000       OR IN-HEAD-RECORD-TYPE = '300'                                     
010100       OR IN-HEAD-RECORD-TYPE = '600'                                     
010200         MOVE +1                TO LINE-COUNT                             
010300         MOVE INAREA1           TO UTAREA1                                
010400         IF INAREA1 = SAVEAREA1                                           
010500           CONTINUE                                                       
010600         ELSE                                                             
010700           PERFORM S06-CREATE-FILE-W51071-HEAD                            
010800           MOVE INAREA1           TO SAVEAREA1                            
010900         END-IF                                                           
011000       END-IF                                                             
011100       IF IN-LINE-RECORD-TYPE = '210'                                     
011200       OR IN-LINE-RECORD-TYPE = '310'                                     
011300       OR IN-LINE-RECORD-TYPE = '610'                                     
011400         ADD +1                 TO LINE-COUNT                             
011500         IF LINE-COUNT > +901                                             
011600           MOVE +2                TO LINE-COUNT                           
011700           MOVE SAVEAREA1         TO UTAREA1                              
011800           PERFORM S06-CREATE-FILE-W51071-HEAD                            
011900         END-IF                                                           
012000         MOVE INAREA2 TO UTAREA2                                          
012100         PERFORM S06-CREATE-FILE-W51071-LINE                              
012200       END-IF                                                             
012300       PERFORM S01-READ-W51071B-POST                                      
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
013400     OPEN INPUT  W51071B                                                  
013500     OPEN OUTPUT W51071                                                   
013600                                                                          
013700     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
013800     .                                                                    
013900     EJECT                                                                
014000                                                                          
014100 Z-END SECTION.                                                           
014200     CLOSE W51071B                                                        
014300           W51071                                                         
014400                                                                          
014500     MOVE 'S'        TO POSTSUM-OPKOD                                     
014600     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014800     EJECT                                                                
014900                                                                          
015000 S01-READ-W51071B-POST SECTION.                                           
015100     READ W51071B INTO INAREA2                                            
015200     AT END                                                               
015300       MOVE JA TO EOF-W51071B                                             
015400     NOT AT END                                                           
015500       MOVE W51071B-TRANSID TO POSTSUM-TRANSID                            
015600       CALL POSTSUM USING POSTSUM-PARM                                    
015700     END-READ                                                             
015800     .                                                                    
015900     SKIP2                                                                
016000                                                                          
016100 S06-CREATE-FILE-W51071-HEAD SECTION.                                     
016200     WRITE R3-71HEAD-POST  FROM UTAREA1                                   
016300     MOVE W51071-TRANSID TO POSTSUM-TRANSID                               
016400     CALL POSTSUM USING POSTSUM-PARM                                      
016500     .                                                                    
016600     EJECT                                                                
016700                                                                          
016800 S06-CREATE-FILE-W51071-LINE SECTION.                                     
016900     WRITE R3-71LINE-POST  FROM UTAREA2                                   
017000     MOVE W51071-TRANSID TO POSTSUM-TRANSID                               
017100     CALL POSTSUM USING POSTSUM-PARM                                      
017200     .                                                                    
017300     EJECT                                                                

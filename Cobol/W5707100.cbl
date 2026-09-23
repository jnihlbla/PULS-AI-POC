000100 ID DIVISION.                                                             
000201 PROGRAM-ID.    W5707100.                                                 
000300                                                                          
000400*    AUTHOR.        ANDERS HENRIKSSON.                                    
000501*    DATE-WRITTEN   JAN     2012.                                         
000600*                                                                         
000700**** SAP BEGRÄNSNINGAR SOM VI MÅSTE TA HÄNSYN TILL                        
000800*                                                                         
000900     EJECT                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400                                                                          
001501     SELECT W57071B ASSIGN       TO W57071D1.                             
001600                                                                          
001701     SELECT W57071  ASSIGN       TO W57071D2.                             
001800                                                                          
001900     EJECT                                                                
002000 DATA DIVISION.                                                           
002100 FILE SECTION.                                                            
002200                                                                          
002301 FD  W57071B                                                              
002400     RECORDING       V                                                    
002500     BLOCK CONTAINS  0.                                                   
002600*01  71HEAD-POST -COPY R3HEAD20               -L.                         
002700*01  71LINE-POST -COPY R3LINE20               -L.                         
002800                                                                          
002901 FD  W57071                                                               
003000     RECORDING       V                                                    
003100     BLOCK CONTAINS  0.                                                   
003200*01  71HEAD-POST -COPY R3HEAD20   -PRE  R3-   -L.                         
003300*01  71LINE-POST -COPY R3LINE20   -PRE  R3-   -L.                         
003400     SKIP2                                                                
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700     SKIP3                                                                
003801 77  IDPGM                   PIC X(8)      VALUE 'W5707100'.              
003900 77  JA                      PIC X         VALUE 'J'.                     
004000 77  NEJ                     PIC X         VALUE 'N'.                     
004101 77  EOF-W57071B             PIC X         VALUE 'N'.                     
004200 77  LINE-COUNT              PIC S9(3)     VALUE +1 COMP SYNC.            
004300                                                                          
004400 01  SUBPROGRAM.                                                          
004500     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
004600                                                                          
004701 01  W57071B-TRANSID.                                                     
004801     03  FILLER              PIC X(6) VALUE 'W57071'.                     
004901     03  FILLER              PIC X(8) VALUE 'W57071D1'.                   
005000     03  FILLER              PIC X(4) VALUE ' IN1'.                       
005100                                                                          
005201 01  W57071-TRANSID.                                                      
005301     03  FILLER              PIC X(6) VALUE 'W57071'.                     
005401     03  FILLER              PIC X(8) VALUE 'W57071D4'.                   
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
009601     PERFORM S01-READ-W57071B-POST                                        
009701     PERFORM UNTIL  EOF-W57071B = JA                                      
009800       MOVE INAREA2           TO INAREA1                                  
009900       IF IN-HEAD-RECORD-TYPE = '200'                                     
010000       OR IN-HEAD-RECORD-TYPE = '300'                                     
010100       OR IN-HEAD-RECORD-TYPE = '600'                                     
010200         MOVE +1                TO LINE-COUNT                             
010300         MOVE INAREA1           TO UTAREA1                                
010310*        MOVE SPACE TO UT-HEAD-DOCUMENT-NO-REF(10:1)                      
010400         IF INAREA1 = SAVEAREA1                                           
010500           CONTINUE                                                       
010600         ELSE                                                             
010701           PERFORM S06-CREATE-FILE-W57071-HEAD                            
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
011800*          MOVE SPACE TO UT-HEAD-DOCUMENT-NO-REF(10:1)                    
011801           PERFORM S06-CREATE-FILE-W57071-HEAD                            
011900         END-IF                                                           
012000         MOVE INAREA2 TO UTAREA2                                          
012100*        MOVE SPACE TO UT-LINE-DOCUMENT-NO-REF(10:1)                      
012101         PERFORM S06-CREATE-FILE-W57071-LINE                              
012200       END-IF                                                             
012301       PERFORM S01-READ-W57071B-POST                                      
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
013401     OPEN INPUT  W57071B                                                  
013501     OPEN OUTPUT W57071                                                   
013600                                                                          
013700     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
013800     .                                                                    
013900     EJECT                                                                
014000                                                                          
014100 Z-END SECTION.                                                           
014201     CLOSE W57071B                                                        
014301           W57071                                                         
014400                                                                          
014500     MOVE 'S'        TO POSTSUM-OPKOD                                     
014600     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014800     EJECT                                                                
014900                                                                          
015001 S01-READ-W57071B-POST SECTION.                                           
015101     READ W57071B INTO INAREA2                                            
015200     AT END                                                               
015301       MOVE JA TO EOF-W57071B                                             
015400     NOT AT END                                                           
015501       MOVE W57071B-TRANSID TO POSTSUM-TRANSID                            
015600       CALL POSTSUM USING POSTSUM-PARM                                    
015700     END-READ                                                             
015800     .                                                                    
015900     SKIP2                                                                
016000                                                                          
016101 S06-CREATE-FILE-W57071-HEAD SECTION.                                     
016200     WRITE R3-71HEAD-POST  FROM UTAREA1                                   
016301     MOVE W57071-TRANSID TO POSTSUM-TRANSID                               
016400     CALL POSTSUM USING POSTSUM-PARM                                      
016500     .                                                                    
016600     EJECT                                                                
016700                                                                          
016801 S06-CREATE-FILE-W57071-LINE SECTION.                                     
016900     WRITE R3-71LINE-POST  FROM UTAREA2                                   
017001     MOVE W57071-TRANSID TO POSTSUM-TRANSID                               
017100     CALL POSTSUM USING POSTSUM-PARM                                      
017200     .                                                                    
017300     EJECT                                                                

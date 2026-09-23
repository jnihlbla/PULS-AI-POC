000101 ID DIVISION.                                                             
000201 PROGRAM-ID.    W5157000.                                                 
000301                                                                          
000401*    AUTHOR.        HÅKAN BOHLIN.                                         
000501*    DATE-WRITTEN   AUG     2017.                                         
000601*                                                                         
000701**** MATCHA TVÅ FILER OCH SKRIV SW DÄR DET BEHÖVS                         
000801*                                                                         
000901     EJECT                                                                
001001 ENVIRONMENT DIVISION.                                                    
001101                                                                          
001201 INPUT-OUTPUT SECTION.                                                    
001301 FILE-CONTROL.                                                            
001401                                                                          
001501     SELECT W51572A ASSIGN       TO W51570D1.                             
001601                                                                          
001701     SELECT W51573B ASSIGN       TO W51570D2.                             
001801                                                                          
001901     SELECT W51572  ASSIGN       TO W51570D3.                             
002001                                                                          
002101     SELECT W51573  ASSIGN       TO W51570D4.                             
002201                                                                          
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600                                                                          
002701 FD  W51572A                                                              
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000*01  71LINE-POST -COPY R3LINE20  -PRE  72A-   -L.                         
003100                                                                          
003201 FD  W51573B                                                              
003301     RECORDING       V                                                    
003401     BLOCK CONTAINS  0.                                                   
003501*01  71HEAD-POST -COPY R3HEAD20  -PRE 73B-    -L.                         
003601*01  71LINE-POST -COPY R3LINE20  -PRE 73B-    -L.                         
003701                                                                          
003801 FD  W51572                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100*01  71LINE-POST -COPY R3LINE20   -PRE  72-   -L.                         
004200     SKIP2                                                                
004300                                                                          
004401 FD  W51573                                                               
004501     RECORDING       V                                                    
004601     BLOCK CONTAINS  0.                                                   
004701*01  71HEAD-POST -COPY R3HEAD20   -PRE  73-   -L.                         
004801*01  71LINE-POST -COPY R3LINE20   -PRE  73-   -L.                         
004901     SKIP2                                                                
005001                                                                          
005100 WORKING-STORAGE SECTION.                                                 
005200     SKIP3                                                                
005301 77  IDPGM                   PIC X(8)      VALUE 'W5157000'.              
005400 77  JA                      PIC X         VALUE 'J'.                     
005500 77  NEJ                     PIC X         VALUE 'N'.                     
005601 77  EOF-W51572A             PIC X         VALUE 'N'.                     
005701 77  EOF-W51573B             PIC X         VALUE 'N'.                     
005800 77  LINE-COUNT              PIC S9(3)     VALUE +1 COMP SYNC.            
005900                                                                          
006000 01  SUBPROGRAM.                                                          
006100     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
006200                                                                          
006301 01  W51572A-TRANSID.                                                     
006401     03  FILLER              PIC X(6) VALUE 'W51572'.                     
006501     03  FILLER              PIC X(8) VALUE 'W51570D1'.                   
006600     03  FILLER              PIC X(4) VALUE ' IN1'.                       
006700                                                                          
006801 01  W51572-TRANSID.                                                      
006901     03  FILLER              PIC X(6) VALUE 'W51572'.                     
007001     03  FILLER              PIC X(8) VALUE 'W51570D3'.                   
007100     03  FILLER              PIC X(4) VALUE ' UT2'.                       
007200                                                                          
007301 01  W51573B-TRANSID.                                                     
007401     03  FILLER              PIC X(6) VALUE 'W51573'.                     
007501     03  FILLER              PIC X(8) VALUE 'W51570D2'.                   
007601     03  FILLER              PIC X(4) VALUE ' IN2'.                       
007701                                                                          
007801 01  W51573-TRANSID.                                                      
007901     03  FILLER              PIC X(6) VALUE 'W51573'.                     
008001     03  FILLER              PIC X(8) VALUE 'W51570D4'.                   
008101     03  FILLER              PIC X(4) VALUE ' UT3'.                       
008201                                                                          
008300     EJECT                                                                
008400*   -COPY W0005  -PRE POSTSUM-                                            
008500     EJECT                                                                
008600                                                                          
008700 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
008800 01  INAREA1.                                                             
008900*03  -COPY R3LINE20              -PRE INA-                                
009000     EJECT                                                                
009100                                                                          
009200 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
009300 01  INAREA2.                                                             
009400*03  -COPY R3HEAD20              -PRE IN-                                 
009500     EJECT                                                                
009600                                                                          
009701 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
009801 01  INAREA3.                                                             
009901*03  -COPY R3LINE20              -PRE IN-                                 
010001     EJECT                                                                
010101                                                                          
010200 01  FILLER                  PIC X(16)   VALUE 'SAVE-AREA1 '.             
010300 01  SAVEAREA1.                                                           
010400*03  -COPY R3HEAD20              -PRE SAVE-                               
010500     EJECT                                                                
010600                                                                          
010700 01  FILLER                  PIC X(16)   VALUE 'SAVE-AREA1 '.             
010800 01  SAVEAREA2.                                                           
010900*03  -COPY R3LINE20              -PRE SAVE-                               
011000     EJECT                                                                
011100                                                                          
011201 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
011301 01  UTAREA1.                                                             
011401*03  -COPY R3LINE20              -PRE UTA-                                
011501     EJECT                                                                
011601                                                                          
011700 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
011800 01  UTAREA2.                                                             
011900*03  -COPY R3HEAD20              -PRE UT-                                 
012000     EJECT                                                                
012100                                                                          
012200 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
012300 01  UTAREA3.                                                             
012400*03  -COPY R3LINE20              -PRE UT-                                 
012500     EJECT                                                                
012600                                                                          
012700 PROCEDURE DIVISION.                                                      
012800 MAIN SECTION.                                                            
012900                                                                          
013000     PERFORM A-INIT                                                       
013100                                                                          
013201     PERFORM S01-READ-W51572A-POST                                        
013301     PERFORM S01-READ-W51573B-POST                                        
013401     PERFORM UNTIL  EOF-W51572A = JA AND EOF-W51573B = JA                 
013501       IF EOF-W51572A = JA                                                
013601         MOVE INAREA3       TO INAREA2                                    
013701         IF IN-HEAD-RECORD-TYPE = '200'                                   
013801         OR IN-HEAD-RECORD-TYPE = '300'                                   
013901         OR IN-HEAD-RECORD-TYPE = '600'                                   
014001           MOVE INAREA2 TO UTAREA2                                        
014101           PERFORM S06-CREATE-FILE-W51573-HEAD                            
014201         END-IF                                                           
014301         IF IN-LINE-RECORD-TYPE = '210'                                   
014401         OR IN-LINE-RECORD-TYPE = '310'                                   
014501         OR IN-LINE-RECORD-TYPE = '610'                                   
014601           MOVE INAREA3 TO UTAREA3                                        
014701           PERFORM S06-CREATE-FILE-W51573-LINE                            
014801         END-IF                                                           
014901         PERFORM S01-READ-W51573B-POST                                    
015101       ELSE                                                               
015201         IF EOF-W51573B = JA                                              
015301           MOVE INAREA1 TO UTAREA1                                        
015401           PERFORM S06-CREATE-FILE-W51572-LINE                            
015501           PERFORM S01-READ-W51572A-POST                                  
015601         ELSE                                                             
015701           MOVE INAREA3       TO INAREA2                                  
015801           IF INA-LINE-DOCUMENT-NO-REF = IN-LINE-DOCUMENT-NO-REF          
015901             IF INA-LINE-TEXT(7:2) = 'SW'                                 
016001               IF IN-HEAD-RECORD-TYPE = '200'                             
016101               OR IN-HEAD-RECORD-TYPE = '300'                             
016201               OR IN-HEAD-RECORD-TYPE = '600'                             
016301                 MOVE INAREA2 TO UTAREA2                                  
016401                 MOVE 'SW' TO UT-HEAD-TEXT(17:2)                          
016501                 PERFORM S06-CREATE-FILE-W51573-HEAD                      
016601               END-IF                                                     
016701               IF IN-LINE-RECORD-TYPE = '210'                             
016801               OR IN-LINE-RECORD-TYPE = '310'                             
016901               OR IN-LINE-RECORD-TYPE = '610'                             
017001                 MOVE INAREA3 TO UTAREA3                                  
017101                 MOVE 'SW' TO UT-LINE-TEXT(7:2)                           
017201                 PERFORM S06-CREATE-FILE-W51573-LINE                      
017301               END-IF                                                     
017400             ELSE                                                         
017500               IF IN-HEAD-RECORD-TYPE = '200'                             
017600               OR IN-HEAD-RECORD-TYPE = '300'                             
017700               OR IN-HEAD-RECORD-TYPE = '600'                             
017800                 MOVE INAREA2 TO UTAREA2                                  
017901                 PERFORM S06-CREATE-FILE-W51573-HEAD                      
018000               END-IF                                                     
018100               IF IN-LINE-RECORD-TYPE = '210'                             
018201               OR IN-LINE-RECORD-TYPE = '310'                             
018301               OR IN-LINE-RECORD-TYPE = '610'                             
018401                 MOVE INAREA3 TO UTAREA3                                  
018501                 PERFORM S06-CREATE-FILE-W51573-LINE                      
018601               END-IF                                                     
018700             END-IF                                                       
018801             PERFORM S01-READ-W51573B-POST                                
018900           ELSE                                                           
019000             IF INA-LINE-DOCUMENT-NO-REF > IN-LINE-DOCUMENT-NO-REF        
019101               IF IN-HEAD-RECORD-TYPE = '200'                             
019201               OR IN-HEAD-RECORD-TYPE = '300'                             
019301               OR IN-HEAD-RECORD-TYPE = '600'                             
019401                 MOVE INAREA2 TO UTAREA2                                  
019501                 PERFORM S06-CREATE-FILE-W51573-HEAD                      
019601               END-IF                                                     
019701               IF IN-LINE-RECORD-TYPE = '210'                             
019801               OR IN-LINE-RECORD-TYPE = '310'                             
019901               OR IN-LINE-RECORD-TYPE = '610'                             
020001                 MOVE INAREA3 TO UTAREA3                                  
020101                 PERFORM S06-CREATE-FILE-W51573-LINE                      
020201               END-IF                                                     
020301               PERFORM S01-READ-W51573B-POST                              
020401             ELSE                                                         
020501             IF INA-LINE-DOCUMENT-NO-REF < IN-LINE-DOCUMENT-NO-REF        
020601                 MOVE INAREA1 TO UTAREA1                                  
020701                 PERFORM S06-CREATE-FILE-W51572-LINE                      
020801               END-IF                                                     
020901               PERFORM S01-READ-W51572A-POST                              
021001             END-IF                                                       
021101           END-IF                                                         
021201         END-IF                                                           
021301       END-IF                                                             
021400     END-PERFORM                                                          
021500                                                                          
021600     PERFORM Z-END                                                        
021700                                                                          
021800     MOVE ZERO TO RETURN-CODE                                             
021900     GOBACK                                                               
022000     .                                                                    
022100     EJECT                                                                
022200                                                                          
022300 A-INIT SECTION.                                                          
022401     OPEN INPUT  W51572A                                                  
022501                 W51573B                                                  
022601     OPEN OUTPUT W51572                                                   
022701                 W51573                                                   
022800                                                                          
022900     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
023000     .                                                                    
023100     EJECT                                                                
023200                                                                          
023300 Z-END SECTION.                                                           
023401     CLOSE W51572A                                                        
023501           W51573B                                                        
023601           W51572                                                         
023701           W51573                                                         
023800                                                                          
023900     MOVE 'S'        TO POSTSUM-OPKOD                                     
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     .                                                                    
024200     EJECT                                                                
024300                                                                          
024401 S01-READ-W51572A-POST SECTION.                                           
024501     READ W51572A INTO INAREA1                                            
024600     AT END                                                               
024701       MOVE JA TO EOF-W51572A                                             
024800     NOT AT END                                                           
024901       MOVE W51572A-TRANSID TO POSTSUM-TRANSID                            
025000       CALL POSTSUM USING POSTSUM-PARM                                    
025100     END-READ                                                             
025200     .                                                                    
025300     SKIP2                                                                
025400                                                                          
025501 S01-READ-W51573B-POST SECTION.                                           
025601     READ W51573B INTO INAREA3                                            
025701     AT END                                                               
025801       MOVE JA TO EOF-W51573B                                             
025901     NOT AT END                                                           
026001       MOVE W51573B-TRANSID TO POSTSUM-TRANSID                            
026101       CALL POSTSUM USING POSTSUM-PARM                                    
026201     END-READ                                                             
026301     .                                                                    
026401     SKIP2                                                                
026501                                                                          
026601 S06-CREATE-FILE-W51572-LINE SECTION.                                     
026701     WRITE 72-71LINE-POST  FROM UTAREA1                                   
026801     MOVE W51572-TRANSID TO POSTSUM-TRANSID                               
026901     CALL POSTSUM USING POSTSUM-PARM                                      
027001     .                                                                    
027101     EJECT                                                                
027201                                                                          
027301 S06-CREATE-FILE-W51573-HEAD SECTION.                                     
027400     WRITE 73-71HEAD-POST  FROM UTAREA2                                   
027501     MOVE W51573-TRANSID TO POSTSUM-TRANSID                               
027600     CALL POSTSUM USING POSTSUM-PARM                                      
027700     .                                                                    
027800     EJECT                                                                
027900                                                                          
028001 S06-CREATE-FILE-W51573-LINE SECTION.                                     
028100     WRITE 73-71LINE-POST  FROM UTAREA3                                   
028201     MOVE W51573-TRANSID TO POSTSUM-TRANSID                               
028300     CALL POSTSUM USING POSTSUM-PARM                                      
028400     .                                                                    
028500     EJECT                                                                

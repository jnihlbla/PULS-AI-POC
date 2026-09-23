000001 ID DIVISION.                                                             
000002 PROGRAM-ID.    W5617000.                                                 
000003                                                                          
000004*    AUTHOR.        ARCHANA BHAT.                                         
000005*    DATE-WRITTEN   NOV     2017.                                         
000006*                                                                         
000007**** MATCHA TVÅ FILER OCH SKRIV SW DÄR DET BEHÖVS                         
000008*                                                                         
000009     EJECT                                                                
000010 ENVIRONMENT DIVISION.                                                    
000011                                                                          
000012 INPUT-OUTPUT SECTION.                                                    
000013 FILE-CONTROL.                                                            
000014                                                                          
000015     SELECT W56172A ASSIGN       TO W56170D1.                             
000016                                                                          
000017     SELECT W56173B ASSIGN       TO W56170D2.                             
000018                                                                          
000019     SELECT W56172  ASSIGN       TO W56170D3.                             
000020                                                                          
000021     SELECT W56173  ASSIGN       TO W56170D4.                             
000022                                                                          
000023     EJECT                                                                
000024 DATA DIVISION.                                                           
000025 FILE SECTION.                                                            
000026                                                                          
000027 FD  W56172A                                                              
000028     RECORDING       F                                                    
000029     BLOCK CONTAINS  0.                                                   
000030*01  71LINE-POST -COPY R3LINE20  -PRE  72A-   -L.                         
000031                                                                          
000032 FD  W56173B                                                              
000033     RECORDING       V                                                    
000034     BLOCK CONTAINS  0.                                                   
000035*01  71HEAD-POST -COPY R3HEAD20  -PRE 73B-    -L.                         
000036*01  71LINE-POST -COPY R3LINE20  -PRE 73B-    -L.                         
000037                                                                          
000038 FD  W56172                                                               
000039     RECORDING       F                                                    
000040     BLOCK CONTAINS  0.                                                   
000041*01  71LINE-POST -COPY R3LINE20   -PRE  72-   -L.                         
000042     SKIP2                                                                
000043                                                                          
000044 FD  W56173                                                               
000045     RECORDING       V                                                    
000046     BLOCK CONTAINS  0.                                                   
000047*01  71HEAD-POST -COPY R3HEAD20   -PRE  73-   -L.                         
000048*01  71LINE-POST -COPY R3LINE20   -PRE  73-   -L.                         
000049     SKIP2                                                                
000050                                                                          
000051 WORKING-STORAGE SECTION.                                                 
000052     SKIP3                                                                
000053 77  IDPGM                   PIC X(8)      VALUE 'W5617000'.              
000054 77  JA                      PIC X         VALUE 'J'.                     
000055 77  NEJ                     PIC X         VALUE 'N'.                     
000056 77  EOF-W56172A             PIC X         VALUE 'N'.                     
000057 77  EOF-W56173B             PIC X         VALUE 'N'.                     
000058 77  LINE-COUNT              PIC S9(3)     VALUE +1 COMP SYNC.            
000059                                                                          
000060 01  SUBPROGRAM.                                                          
000061     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
000062                                                                          
000063 01  W56172A-TRANSID.                                                     
000064     03  FILLER              PIC X(6) VALUE 'W56172'.                     
000065     03  FILLER              PIC X(8) VALUE 'W56170D1'.                   
000066     03  FILLER              PIC X(4) VALUE ' IN1'.                       
000067                                                                          
000068 01  W56172-TRANSID.                                                      
000069     03  FILLER              PIC X(6) VALUE 'W56172'.                     
000070     03  FILLER              PIC X(8) VALUE 'W56170D3'.                   
000071     03  FILLER              PIC X(4) VALUE ' UT2'.                       
000072                                                                          
000073 01  W56173B-TRANSID.                                                     
000074     03  FILLER              PIC X(6) VALUE 'W56173'.                     
000075     03  FILLER              PIC X(8) VALUE 'W56170D2'.                   
000076     03  FILLER              PIC X(4) VALUE ' IN2'.                       
000077                                                                          
000078 01  W56173-TRANSID.                                                      
000079     03  FILLER              PIC X(6) VALUE 'W56173'.                     
000080     03  FILLER              PIC X(8) VALUE 'W56170D4'.                   
000081     03  FILLER              PIC X(4) VALUE ' UT3'.                       
000082                                                                          
000083     EJECT                                                                
000084*   -COPY W0005  -PRE POSTSUM-                                            
000085     EJECT                                                                
000086                                                                          
000087 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
000088 01  INAREA1.                                                             
000089*03  -COPY R3LINE20              -PRE INA-                                
000090     EJECT                                                                
000091                                                                          
000092 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
000093 01  INAREA2.                                                             
000094*03  -COPY R3HEAD20              -PRE IN-                                 
000095     EJECT                                                                
000096                                                                          
000097 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
000098 01  INAREA3.                                                             
000099*03  -COPY R3LINE20              -PRE IN-                                 
000100     EJECT                                                                
000101                                                                          
000102 01  FILLER                  PIC X(16)   VALUE 'SAVE-AREA1 '.             
000103 01  SAVEAREA1.                                                           
000104*03  -COPY R3HEAD20              -PRE SAVE-                               
000105     EJECT                                                                
000106                                                                          
000107 01  FILLER                  PIC X(16)   VALUE 'SAVE-AREA1 '.             
000108 01  SAVEAREA2.                                                           
000109*03  -COPY R3LINE20              -PRE SAVE-                               
000110     EJECT                                                                
000111                                                                          
000112 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
000113 01  UTAREA1.                                                             
000114*03  -COPY R3LINE20              -PRE UTA-                                
000115     EJECT                                                                
000116                                                                          
000117 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
000118 01  UTAREA2.                                                             
000119*03  -COPY R3HEAD20              -PRE UT-                                 
000120     EJECT                                                                
000121                                                                          
000122 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
000123 01  UTAREA3.                                                             
000124*03  -COPY R3LINE20              -PRE UT-                                 
000125     EJECT                                                                
000126                                                                          
000127 PROCEDURE DIVISION.                                                      
000128 MAIN SECTION.                                                            
000129                                                                          
000130     PERFORM A-INIT                                                       
000131                                                                          
000132     PERFORM S01-READ-W56172A-POST                                        
000133     PERFORM S01-READ-W56173B-POST                                        
000134     PERFORM UNTIL  EOF-W56172A = JA AND EOF-W56173B = JA                 
000135       IF EOF-W56172A = JA                                                
000136         MOVE INAREA3       TO INAREA2                                    
000137         IF IN-HEAD-RECORD-TYPE = '200'                                   
000138         OR IN-HEAD-RECORD-TYPE = '300'                                   
000139         OR IN-HEAD-RECORD-TYPE = '600'                                   
000140           MOVE INAREA2 TO UTAREA2                                        
000141*          MOVE SPACE TO UT-HEAD-DOCUMENT-NO-REF(10:1)                    
000142           PERFORM S06-CREATE-FILE-W56173-HEAD                            
000143         END-IF                                                           
000144         IF IN-LINE-RECORD-TYPE = '210'                                   
000145         OR IN-LINE-RECORD-TYPE = '310'                                   
000146         OR IN-LINE-RECORD-TYPE = '610'                                   
000147           MOVE INAREA3 TO UTAREA3                                        
000148*          MOVE SPACE TO UT-LINE-DOCUMENT-NO-REF(10:1)                    
000149           PERFORM S06-CREATE-FILE-W56173-LINE                            
000150         END-IF                                                           
000151         PERFORM S01-READ-W56173B-POST                                    
000152         DISPLAY 'EOF-W56172A'                                            
000153       ELSE                                                               
000154         IF EOF-W56173B = JA                                              
000155           MOVE INAREA1 TO UTAREA1                                        
000156*          MOVE SPACE TO UTA-LINE-DOCUMENT-NO-REF(10:1)                   
000157           PERFORM S06-CREATE-FILE-W56172-LINE                            
000158           PERFORM S01-READ-W56172A-POST                                  
000159         ELSE                                                             
000160           MOVE INAREA3       TO INAREA2                                  
000161           IF INA-LINE-DOCUMENT-NO-REF = IN-LINE-DOCUMENT-NO-REF          
000162             IF INA-LINE-TEXT(7:2) = 'SW'                                 
000163               IF IN-HEAD-RECORD-TYPE = '200'                             
000164               OR IN-HEAD-RECORD-TYPE = '300'                             
000165               OR IN-HEAD-RECORD-TYPE = '600'                             
000166                 MOVE INAREA2 TO UTAREA2                                  
000167*                MOVE SPACE TO UT-HEAD-DOCUMENT-NO-REF(10:1)              
000168                 MOVE 'SW' TO UT-HEAD-TEXT(17:2)                          
000169                 PERFORM S06-CREATE-FILE-W56173-HEAD                      
000170               END-IF                                                     
000171               IF IN-LINE-RECORD-TYPE = '210'                             
000172               OR IN-LINE-RECORD-TYPE = '310'                             
000173               OR IN-LINE-RECORD-TYPE = '610'                             
000174                 MOVE INAREA3 TO UTAREA3                                  
000175*                MOVE SPACE TO UT-LINE-DOCUMENT-NO-REF(10:1)              
000176                 MOVE 'SW' TO UT-LINE-TEXT(7:2)                           
000177                 PERFORM S06-CREATE-FILE-W56173-LINE                      
000178               END-IF                                                     
000179             ELSE                                                         
000180               IF IN-HEAD-RECORD-TYPE = '200'                             
000181               OR IN-HEAD-RECORD-TYPE = '300'                             
000182               OR IN-HEAD-RECORD-TYPE = '600'                             
000183                 MOVE INAREA2 TO UTAREA2                                  
000184*                MOVE SPACE TO UT-HEAD-DOCUMENT-NO-REF(10:1)              
000185                 PERFORM S06-CREATE-FILE-W56173-HEAD                      
000186               END-IF                                                     
000187               IF IN-LINE-RECORD-TYPE = '210'                             
000188               OR IN-LINE-RECORD-TYPE = '310'                             
000189               OR IN-LINE-RECORD-TYPE = '610'                             
000190                 MOVE INAREA3 TO UTAREA3                                  
000191*                MOVE SPACE TO UT-LINE-DOCUMENT-NO-REF(10:1)              
000192                 PERFORM S06-CREATE-FILE-W56173-LINE                      
000193               END-IF                                                     
000194             END-IF                                                       
000195             PERFORM S01-READ-W56173B-POST                                
000196           ELSE                                                           
000197             IF INA-LINE-DOCUMENT-NO-REF > IN-LINE-DOCUMENT-NO-REF        
000198               IF IN-HEAD-RECORD-TYPE = '200'                             
000199               OR IN-HEAD-RECORD-TYPE = '300'                             
000200               OR IN-HEAD-RECORD-TYPE = '600'                             
000201                 MOVE INAREA2 TO UTAREA2                                  
000202*                MOVE SPACE TO UT-HEAD-DOCUMENT-NO-REF(10:1)              
000203                 PERFORM S06-CREATE-FILE-W56173-HEAD                      
000204               END-IF                                                     
000205               IF IN-LINE-RECORD-TYPE = '210'                             
000206               OR IN-LINE-RECORD-TYPE = '310'                             
000207               OR IN-LINE-RECORD-TYPE = '610'                             
000208                 MOVE INAREA3 TO UTAREA3                                  
000209*                MOVE SPACE TO UT-LINE-DOCUMENT-NO-REF(10:1)              
000210                 PERFORM S06-CREATE-FILE-W56173-LINE                      
000211               END-IF                                                     
000212               PERFORM S01-READ-W56173B-POST                              
000213             ELSE                                                         
000214             IF INA-LINE-DOCUMENT-NO-REF < IN-LINE-DOCUMENT-NO-REF        
000215                 MOVE INAREA1 TO UTAREA1                                  
000216*                MOVE SPACE TO UTA-LINE-DOCUMENT-NO-REF(10:1)             
000217                 PERFORM S06-CREATE-FILE-W56172-LINE                      
000218               END-IF                                                     
000219               PERFORM S01-READ-W56172A-POST                              
000220             END-IF                                                       
000221           END-IF                                                         
000222         END-IF                                                           
000223       END-IF                                                             
000224     END-PERFORM                                                          
000225                                                                          
000226     PERFORM Z-END                                                        
000227                                                                          
000228     MOVE ZERO TO RETURN-CODE                                             
000229     GOBACK                                                               
000230     .                                                                    
000231     EJECT                                                                
000232                                                                          
000233 A-INIT SECTION.                                                          
000234     OPEN INPUT  W56172A                                                  
000235                 W56173B                                                  
000236     OPEN OUTPUT W56172                                                   
000237                 W56173                                                   
000238                                                                          
000239     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
000240     .                                                                    
000241     EJECT                                                                
000242                                                                          
000243 Z-END SECTION.                                                           
000244     CLOSE W56172A                                                        
000245           W56173B                                                        
000246           W56172                                                         
000247           W56173                                                         
000248                                                                          
000249     MOVE 'S'        TO POSTSUM-OPKOD                                     
000250     CALL POSTSUM USING POSTSUM-PARM                                      
000251     .                                                                    
000252     EJECT                                                                
000253                                                                          
000254 S01-READ-W56172A-POST SECTION.                                           
000255     READ W56172A INTO INAREA1                                            
000256     AT END                                                               
000257       MOVE JA TO EOF-W56172A                                             
000258     NOT AT END                                                           
000259       MOVE W56172A-TRANSID TO POSTSUM-TRANSID                            
000260       CALL POSTSUM USING POSTSUM-PARM                                    
000261     END-READ                                                             
000262     .                                                                    
000263     SKIP2                                                                
000264                                                                          
000265 S01-READ-W56173B-POST SECTION.                                           
000266     READ W56173B INTO INAREA3                                            
000267     AT END                                                               
000268       MOVE JA TO EOF-W56173B                                             
000269     NOT AT END                                                           
000270       MOVE W56173B-TRANSID TO POSTSUM-TRANSID                            
000271       CALL POSTSUM USING POSTSUM-PARM                                    
000272     END-READ                                                             
000273     .                                                                    
000274     SKIP2                                                                
000275                                                                          
000276 S06-CREATE-FILE-W56172-LINE SECTION.                                     
000277     WRITE 72-71LINE-POST  FROM UTAREA1                                   
000278     MOVE W56172-TRANSID TO POSTSUM-TRANSID                               
000279     CALL POSTSUM USING POSTSUM-PARM                                      
000280     .                                                                    
000281     EJECT                                                                
000282                                                                          
000283 S06-CREATE-FILE-W56173-HEAD SECTION.                                     
000284     WRITE 73-71HEAD-POST  FROM UTAREA2                                   
000285     MOVE W56173-TRANSID TO POSTSUM-TRANSID                               
000286     CALL POSTSUM USING POSTSUM-PARM                                      
000287     .                                                                    
000288     EJECT                                                                
000289                                                                          
000290 S06-CREATE-FILE-W56173-LINE SECTION.                                     
000291     WRITE 73-71LINE-POST  FROM UTAREA3                                   
000292     MOVE W56173-TRANSID TO POSTSUM-TRANSID                               
000293     CALL POSTSUM USING POSTSUM-PARM                                      
000294     .                                                                    
000295     EJECT                                                                

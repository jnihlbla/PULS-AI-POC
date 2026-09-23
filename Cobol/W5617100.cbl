000001 ID DIVISION.                                                             
000002 PROGRAM-ID.    W5617100.                                                 
000003                                                                          
000004*    AUTHOR.        ARCHANA BHAT.                                         
000005*    DATE-WRITTEN   NOV 2017.                                             
000006*                                                                         
000007**** SAP BEGRÄNSNINGAR SOM VI MÅSTE TA HÄNSYN TILL                        
000008*                                                                         
000009     EJECT                                                                
000010 ENVIRONMENT DIVISION.                                                    
000011                                                                          
000012 INPUT-OUTPUT SECTION.                                                    
000013 FILE-CONTROL.                                                            
000014                                                                          
000015     SELECT W56171B ASSIGN       TO W56171D1.                             
000016                                                                          
000017     SELECT W56171  ASSIGN       TO W56171D2.                             
000018                                                                          
000019     EJECT                                                                
000020 DATA DIVISION.                                                           
000021 FILE SECTION.                                                            
000022                                                                          
000023 FD  W56171B                                                              
000024     RECORDING       V                                                    
000025     BLOCK CONTAINS  0.                                                   
000026*01  71HEAD-POST -COPY R3HEAD20               -L.                         
000027*01  71LINE-POST -COPY R3LINE20               -L.                         
000028                                                                          
000029 FD  W56171                                                               
000030     RECORDING       V                                                    
000031     BLOCK CONTAINS  0.                                                   
000032*01  71HEAD-POST -COPY R3HEAD20   -PRE  R3-   -L.                         
000033*01  71LINE-POST -COPY R3LINE20   -PRE  R3-   -L.                         
000034     SKIP2                                                                
000035                                                                          
000036 WORKING-STORAGE SECTION.                                                 
000037     SKIP3                                                                
000038 77  IDPGM                   PIC X(8)      VALUE 'W5617100'.              
000039 77  JA                      PIC X         VALUE 'J'.                     
000040 77  NEJ                     PIC X         VALUE 'N'.                     
000041 77  EOF-W56171B             PIC X         VALUE 'N'.                     
000042 77  LINE-COUNT              PIC S9(3)     VALUE +1 COMP SYNC.            
000043                                                                          
000044 01  SUBPROGRAM.                                                          
000045     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
000046                                                                          
000047 01  W56171B-TRANSID.                                                     
000048     03  FILLER              PIC X(6) VALUE 'W56171'.                     
000049     03  FILLER              PIC X(8) VALUE 'W56171D1'.                   
000050     03  FILLER              PIC X(4) VALUE ' IN1'.                       
000051                                                                          
000052 01  W56171-TRANSID.                                                      
000053     03  FILLER              PIC X(6) VALUE 'W56171'.                     
000054     03  FILLER              PIC X(8) VALUE 'W56171D4'.                   
000055     03  FILLER              PIC X(4) VALUE ' UT2'.                       
000056                                                                          
000057     EJECT                                                                
000058*   -COPY W0005  -PRE POSTSUM-                                            
000059     EJECT                                                                
000060                                                                          
000061 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
000062 01  INAREA1.                                                             
000063*03  -COPY R3HEAD20              -PRE IN-                                 
000064     EJECT                                                                
000065                                                                          
000066 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
000067 01  INAREA2.                                                             
000068*03  -COPY R3LINE20              -PRE IN-                                 
000069     EJECT                                                                
000070                                                                          
000071 01  FILLER                  PIC X(16)   VALUE 'SAVE-AREA1 '.             
000072 01  SAVEAREA1.                                                           
000073*03  -COPY R3HEAD20              -PRE SAVE-                               
000074     EJECT                                                                
000075                                                                          
000076 01  FILLER                  PIC X(16)   VALUE 'SAVE-AREA1 '.             
000077 01  SAVEAREA2.                                                           
000078*03  -COPY R3LINE20              -PRE SAVE-                               
000079     EJECT                                                                
000080                                                                          
000081 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
000082 01  UTAREA1.                                                             
000083*03  -COPY R3HEAD20              -PRE UT-                                 
000084     EJECT                                                                
000085                                                                          
000086 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
000087 01  UTAREA2.                                                             
000088*03  -COPY R3LINE20              -PRE UT-                                 
000089     EJECT                                                                
000090                                                                          
000091 PROCEDURE DIVISION.                                                      
000092 MAIN SECTION.                                                            
000093                                                                          
000094     PERFORM A-INIT                                                       
000095                                                                          
000096     PERFORM S01-READ-W56171B-POST                                        
000097     PERFORM UNTIL  EOF-W56171B = JA                                      
000098       MOVE INAREA2           TO INAREA1                                  
000099       IF IN-HEAD-RECORD-TYPE = '200'                                     
000100       OR IN-HEAD-RECORD-TYPE = '300'                                     
000101       OR IN-HEAD-RECORD-TYPE = '600'                                     
000102         MOVE +1                TO LINE-COUNT                             
000103         MOVE INAREA1           TO UTAREA1                                
000104*        MOVE SPACE TO UT-HEAD-DOCUMENT-NO-REF(10:1)                      
000105         IF INAREA1 = SAVEAREA1                                           
000106           CONTINUE                                                       
000107         ELSE                                                             
000108           PERFORM S06-CREATE-FILE-W56171-HEAD                            
000109           MOVE INAREA1           TO SAVEAREA1                            
000110         END-IF                                                           
000111       END-IF                                                             
000112       IF IN-LINE-RECORD-TYPE = '210'                                     
000113       OR IN-LINE-RECORD-TYPE = '310'                                     
000114       OR IN-LINE-RECORD-TYPE = '610'                                     
000115         ADD +1                 TO LINE-COUNT                             
000116         IF LINE-COUNT > +901                                             
000117           MOVE +2                TO LINE-COUNT                           
000118           MOVE SAVEAREA1         TO UTAREA1                              
000119*          MOVE SPACE TO UT-HEAD-DOCUMENT-NO-REF(10:1)                    
000120           PERFORM S06-CREATE-FILE-W56171-HEAD                            
000121         END-IF                                                           
000122         MOVE INAREA2 TO UTAREA2                                          
000123*        MOVE SPACE TO UT-LINE-DOCUMENT-NO-REF(10:1)                      
000124         PERFORM S06-CREATE-FILE-W56171-LINE                              
000125       END-IF                                                             
000126       PERFORM S01-READ-W56171B-POST                                      
000127     END-PERFORM                                                          
000128                                                                          
000129     PERFORM Z-END                                                        
000130                                                                          
000131     MOVE ZERO TO RETURN-CODE                                             
000132     GOBACK                                                               
000133     .                                                                    
000134     EJECT                                                                
000135                                                                          
000136 A-INIT SECTION.                                                          
000137     OPEN INPUT  W56171B                                                  
000138     OPEN OUTPUT W56171                                                   
000139                                                                          
000140     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
000141     .                                                                    
000142     EJECT                                                                
000143                                                                          
000144 Z-END SECTION.                                                           
000145     CLOSE W56171B                                                        
000146           W56171                                                         
000147                                                                          
000148     MOVE 'S'        TO POSTSUM-OPKOD                                     
000149     CALL POSTSUM USING POSTSUM-PARM                                      
000150     .                                                                    
000151     EJECT                                                                
000152                                                                          
000153 S01-READ-W56171B-POST SECTION.                                           
000154     READ W56171B INTO INAREA2                                            
000155     AT END                                                               
000156       MOVE JA TO EOF-W56171B                                             
000157     NOT AT END                                                           
000158       MOVE W56171B-TRANSID TO POSTSUM-TRANSID                            
000159       CALL POSTSUM USING POSTSUM-PARM                                    
000160     END-READ                                                             
000161     .                                                                    
000162     SKIP2                                                                
000163                                                                          
000164 S06-CREATE-FILE-W56171-HEAD SECTION.                                     
000165     WRITE R3-71HEAD-POST  FROM UTAREA1                                   
000166     MOVE W56171-TRANSID TO POSTSUM-TRANSID                               
000167     CALL POSTSUM USING POSTSUM-PARM                                      
000168     .                                                                    
000169     EJECT                                                                
000170                                                                          
000171 S06-CREATE-FILE-W56171-LINE SECTION.                                     
000172     WRITE R3-71LINE-POST  FROM UTAREA2                                   
000173     MOVE W56171-TRANSID TO POSTSUM-TRANSID                               
000174     CALL POSTSUM USING POSTSUM-PARM                                      
000175     .                                                                    
000176     EJECT                                                                

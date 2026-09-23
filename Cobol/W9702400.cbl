000010 ID  DIVISION.                                                            
000020     SKIP2                                                                
000030 PROGRAM-ID.  W9702400.                                                   
000040     SKIP2                                                                
000050 AUTHOR.        KARIN OLSSON.                                             
000060     DATE-WRITTEN.  JAN 1993.                                             
000070*                                                                         
000080*                                                                         
000090*                                                                         
000091     EJECT                                                                
000092 ENVIRONMENT DIVISION.                                                    
000093     SKIP2                                                                
000094 INPUT-OUTPUT  SECTION.                                                   
000095*                                                                         
000096 FILE-CONTROL.                                                            
000097*                                                                         
000098     SELECT USERFIL         ASSIGN TO W97024D1.                           
000099*                                                                         
000102     SELECT LVFIL           ASSIGN TO W97024D2.                           
000103*                                                                         
000104     SELECT PVFIL           ASSIGN TO W97024D3.                           
000105*                                                                         
000106 DATA DIVISION.                                                           
000107                                                                          
000108 FILE  SECTION.                                                           
000109*                                                                         
000110 FD  USERFIL                                                              
000111     LABEL RECORD   STANDARD                                              
000112     RECORDING      F                                                     
000113     BLOCK CONTAINS 0.                                                    
000114                                                                          
000115 01  FILLER                  PIC X(80).                                   
000116*                                                                         
000124 FD  LVFIL                                                                
000125     LABEL RECORD   STANDARD                                              
000126     RECORDING      F                                                     
000127     BLOCK CONTAINS 0.                                                    
000128                                                                          
000129 01  LV-POST                 PIC X(60).                                   
000130*                                                                         
000131 FD  PVFIL                                                                
000132     LABEL RECORD   STANDARD                                              
000133     RECORDING      F                                                     
000134     BLOCK CONTAINS 0.                                                    
000135                                                                          
000136 01  PV-POST                 PIC X(60).                                   
000137     EJECT                                                                
000138 WORKING-STORAGE  SECTION.                                                
000139                                                                          
000140*    -- CHECKED BY WY2000                                                 
000150*                                                                         
000161 01  USER-AREA.                                                           
000162     03  USER-LOGONID            PIC X(8).                                
000164     03  FILLER                  PIC X.                                   
000165     03  USER-NAMN               PIC X(20).                               
000166     03  FILLER                  PIC X.                                   
000167     03  USER-BOLAG              PIC X(3).                                
000168     03  USER-LAND               PIC X(2).                                
000169     03  USER-AVD                PIC X(5).                                
000170     03  USER-JOBFUNC            PIC X(2).                                
000171     03  USER-DIVMISC            PIC X(3).                                
000173     03  FILLER                  PIC X(4).                                
000174     03  USER-ANSTNR             PIC X(5).                                
000175     03  FILLER                  PIC X(1).                                
000176     03  USER-DATUM              PIC X(8).                                
000177     03  FILLER                  PIC X(1).                                
000178     03  USER-MILJOO             PIC X(2).                                
000179     03  FILLER                  PIC X(14).                               
000180*                                                                         
000186 01  UT-AREA.                                                             
000187*                                                                         
000190     03  UT-LOGONID          PIC X(8)   VALUE SPACE.                      
000191     03  UT-NAMN             PIC X(20)  VALUE SPACE.                      
000192     03  UT-BOLAG            PIC X(3)   VALUE SPACE.                      
000193     03  UT-LAND             PIC X(2)   VALUE SPACE.                      
000194     03  UT-JOBFUNC          PIC X(2)   VALUE SPACE.                      
000195     03  UT-DIVMISC          PIC X(3)   VALUE SPACE.                      
000196     03  UT-ANSTNR           PIC X(5)   VALUE SPACE.                      
000197     03  UT-AVD              PIC X(5)   VALUE SPACE.                      
000198     03  UT-DATUM            PIC X(8)   VALUE SPACE.                      
000199     03  FILLER              PIC X(4)   VALUE SPACE.                      
000209*                                                                         
000210 01  GENERELLA-SUBPGM.                                                    
000211*                                                                         
000212     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
000214     EJECT                                                                
000215* --- PARAMETRAR TILL ABEND                                               
000216 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP VALUE +16.                  
000217 01  RKOD-ABEND-MED-DUMP     PIC S9(4)   COMP VALUE +1000.                
000218*                                                                         
000237 01  GENERELLA-KONSTANTER.                                                
000238*                                                                         
000239     03  JA                  PIC X(1)    VALUE 'Y'.                       
000240     03  NEJ                 PIC X(1)    VALUE 'N'.                       
000243                                                                          
000244 01  USERFIL-EOF             PIC X(1)    VALUE 'N'.                       
000246*                                                                         
000251     EJECT                                                                
000252*                                                                         
000253     SKIP2                                                                
000254 01  RETURKODER.                                                          
000255*                                                                         
000256     03  RKOD                PIC S9(4)   COMP SYNC VALUE ZERO.            
000257     SKIP2                                                                
000258 01  W-BOLAG                 PIC X(3).                                    
000259     EJECT                                                                
000260 PROCEDURE DIVISION.                                                      
000261*                                                                         
000262     PERFORM A-INIT                                                       
000263     PERFORM S10-LAES-USERFIL                                             
000264     PERFORM UNTIL USERFIL-EOF = JA                                       
000265       EVALUATE TRUE                                                      
000266         WHEN USER-BOLAG = 'AP ' AND USER-MILJOO = 'V1'                   
000268           PERFORM B-SKAPA-LVPOST                                         
000295                                                                          
000296         WHEN USER-BOLAG = 'AC ' AND USER-LAND = 'GP'                     
000297              AND USER-MILJOO = 'V2'                                      
000298           PERFORM C-SKAPA-PVPOST                                         
000307                                                                          
000320         WHEN USER-BOLAG = 'ADT' AND USER-MILJOO = 'V1'                   
000321           MOVE 'AFT' TO W-BOLAG                                          
000322           PERFORM D-SKAPA-LVPOST2                                        
000323                                                                          
000324         WHEN USER-BOLAG = 'ADV' AND USER-MILJOO = 'V1'                   
000325           MOVE 'AFV' TO W-BOLAG                                          
000326           PERFORM D-SKAPA-LVPOST2                                        
000327                                                                          
000328         WHEN USER-BOLAG = 'ADC' AND USER-MILJOO = 'V1'                   
000329           MOVE 'AFC' TO W-BOLAG                                          
000330           PERFORM E-SKAPA-PVPOST2                                        
000331                                                                          
000332         WHEN USER-BOLAG = 'AIT' AND USER-MILJOO = 'V1'                   
000333           MOVE 'IMT' TO W-BOLAG                                          
000334           PERFORM D-SKAPA-LVPOST2                                        
000335                                                                          
000336         WHEN USER-BOLAG = 'AIV' AND USER-MILJOO = 'V1'                   
000337           MOVE 'IMV' TO W-BOLAG                                          
000338           PERFORM D-SKAPA-LVPOST2                                        
000339                                                                          
000340         WHEN USER-BOLAG = 'AIC' AND USER-MILJOO = 'V1'                   
000341           MOVE 'IMC' TO W-BOLAG                                          
000342           PERFORM E-SKAPA-PVPOST2                                        
000343                                                                          
000344         WHEN OTHER                                                       
000345           CONTINUE                                                       
000346       END-EVALUATE                                                       
000347       PERFORM S10-LAES-USERFIL                                           
000348     END-PERFORM                                                          
000349                                                                          
000350     PERFORM Z-FINIT                                                      
000351     MOVE RKOD TO RETURN-CODE                                             
000352     GOBACK.                                                              
000353     EJECT                                                                
000354 A-INIT  SECTION.                                                         
000355     SKIP2                                                                
000356     OPEN INPUT USERFIL                                                   
000357                                                                          
000358     OPEN OUTPUT LVFIL PVFIL                                              
000359     MOVE NEJ TO USERFIL-EOF                                              
000360     .                                                                    
000370     EJECT                                                                
000410 B-SKAPA-LVPOST   SECTION.                                                
000411     SKIP2                                                                
000413     MOVE SPACE TO UT-AREA                                                
000414     MOVE 'VTP' TO UT-BOLAG                                               
000415     MOVE USER-LOGONID TO UT-LOGONID                                      
000416     MOVE USER-NAMN    TO UT-NAMN                                         
000417     IF USER-LAND = SPACE                                                 
000418       MOVE 'SE'         TO UT-LAND                                       
000419     ELSE                                                                 
000420       MOVE USER-LAND    TO UT-LAND                                       
000421     END-IF                                                               
000422     MOVE USER-JOBFUNC TO UT-JOBFUNC                                      
000423     MOVE USER-DIVMISC TO UT-DIVMISC                                      
000424     MOVE USER-ANSTNR  TO UT-ANSTNR                                       
000425     MOVE USER-AVD     TO UT-AVD                                          
000426     MOVE USER-DATUM   TO UT-DATUM                                        
000430     PERFORM S20-SKRIV-LVFIL                                              
000440     .                                                                    
000441     EJECT                                                                
000442 C-SKAPA-PVPOST   SECTION.                                                
000443     SKIP2                                                                
000444     MOVE SPACE TO UT-AREA                                                
000445     MOVE 'VCP' TO UT-BOLAG                                               
000446     MOVE USER-LOGONID TO UT-LOGONID                                      
000447     MOVE USER-NAMN    TO UT-NAMN                                         
000450     MOVE USER-JOBFUNC TO UT-JOBFUNC                                      
000451     MOVE USER-DIVMISC TO UT-DIVMISC                                      
000455     MOVE USER-ANSTNR  TO UT-ANSTNR                                       
000456     MOVE USER-AVD     TO UT-AVD                                          
000457     MOVE USER-DATUM   TO UT-DATUM                                        
000458     PERFORM S21-SKRIV-PVFIL                                              
000459     .                                                                    
000460     EJECT                                                                
000461 D-SKAPA-LVPOST2   SECTION.                                               
000462     SKIP2                                                                
000463     MOVE SPACE TO UT-AREA                                                
000464     MOVE W-BOLAG TO UT-BOLAG                                             
000465     MOVE USER-LOGONID TO UT-LOGONID                                      
000466     MOVE USER-NAMN    TO UT-NAMN                                         
000467     IF USER-LAND = SPACE                                                 
000468       MOVE 'SE'         TO UT-LAND                                       
000469     ELSE                                                                 
000470       MOVE USER-LAND    TO UT-LAND                                       
000471     END-IF                                                               
000472     MOVE USER-DIVMISC TO UT-DIVMISC                                      
000473     MOVE USER-JOBFUNC TO UT-JOBFUNC                                      
000474     IF USER-DIVMISC NOT = SPACE                                          
000475       MOVE USER-DIVMISC (2:2) TO UT-JOBFUNC                              
000476     END-IF                                                               
000477     MOVE USER-ANSTNR  TO UT-ANSTNR                                       
000478     MOVE USER-AVD     TO UT-AVD                                          
000479     MOVE USER-DATUM   TO UT-DATUM                                        
000480     PERFORM S20-SKRIV-LVFIL                                              
000481     .                                                                    
000482     EJECT                                                                
000483 E-SKAPA-PVPOST2   SECTION.                                               
000484     SKIP2                                                                
000485     MOVE SPACE TO UT-AREA                                                
000486     MOVE W-BOLAG TO UT-BOLAG                                             
000487     MOVE USER-LOGONID TO UT-LOGONID                                      
000488     MOVE USER-NAMN    TO UT-NAMN                                         
000489     IF USER-LAND = SPACE                                                 
000490       MOVE 'SE'         TO UT-LAND                                       
000491     ELSE                                                                 
000492       MOVE USER-LAND    TO UT-LAND                                       
000493     END-IF                                                               
000494     MOVE USER-DIVMISC TO UT-DIVMISC                                      
000495     MOVE USER-JOBFUNC TO UT-JOBFUNC                                      
000496     IF USER-DIVMISC NOT = SPACE                                          
000497       MOVE USER-DIVMISC (2:2) TO UT-JOBFUNC                              
000498     END-IF                                                               
000499     MOVE USER-ANSTNR  TO UT-ANSTNR                                       
000500     MOVE USER-AVD     TO UT-AVD                                          
000501     MOVE USER-DATUM   TO UT-DATUM                                        
000502     PERFORM S21-SKRIV-PVFIL                                              
000503     .                                                                    
000504     EJECT                                                                
000505 Z-FINIT  SECTION.                                                        
000506     SKIP2                                                                
000507     CLOSE USERFIL                                                        
000508           LVFIL                                                          
000509           PVFIL                                                          
000510     .                                                                    
000511 S10-LAES-USERFIL SECTION.                                                
000512     SKIP2                                                                
000513     READ USERFIL INTO USER-AREA                                          
000514       AT END MOVE JA TO USERFIL-EOF                                      
000515     .                                                                    
000516 S20-SKRIV-LVFIL  SECTION.                                                
000517     SKIP2                                                                
000518     WRITE LV-POST   FROM UT-AREA                                         
000519     .                                                                    
000520 S21-SKRIV-PVFIL  SECTION.                                                
000521     SKIP2                                                                
000522     WRITE PV-POST   FROM UT-AREA                                         
000523     .                                                                    
000524 S99-ABEND  SECTION.                                                      
000525     SKIP2                                                                
000530     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
000600     .                                                                    

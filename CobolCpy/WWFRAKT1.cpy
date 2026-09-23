000100*** EDIT ALLOWED                                                          
010000*                            *************************************        
020000*                            *** CONSTANTER FÖR DIV FRAKTKODER            
020100*                            *** ANVÄNDS BL.A. VID UTSKRIFT AV            
030006*                            *** FRAKTSEDEL,FÖR FRAKTSEDLAR               
040006*                            *** SOM SKALL TILL SVERIGE                   
050000*                            *************************************        
060000                                                                          
070006 01  FRAK01-KDFRAKT          PIC X(2).                                    
080000*                                                                         
090017       88  FRAK01-SVERIGE2   VALUE  '04' '05' '06' '07' '08' '09'         
090018                                    '10' '11' '12' '31' '36'              
100017                                    '40' '41' '69' '71'.                  
110006                                                                          
120016       88  FRAK01-NORDEN     VALUE  '32' '46' '50' '63' '64' '66'         
130016                                    '67' '70' '72'.                       
150007                                                                          
160211       88  FRAK01-KDFRAKT21  VALUE  '21'.                                 
161007                                                                          
162008       88  FRAK01-KDFRAKT62  VALUE  '62'.                                 
170007                                                                          
170008       88  FRAK01-FLYG       VALUE  '17'.                                 
170009                                                                          
170008       88  FRAK01-KDFRAKT50  VALUE  '50'.                                 
170009                                                                          
170008       88  FRAK01-KDFRAKT51  VALUE  '51'.                                 
170009                                                                          
240000*                                                                         
250006*** END COPY WWFRAK01  LENGTH=2                                           

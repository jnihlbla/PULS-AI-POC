000010*** EDIT ALLOWED                                                          
000200                                                                          
000210*    EDI LIN GOODS ITEM DETAILS                                           
000211*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000212*                                                                         
000213*    FUNCTION,                                                            
000214*    -TO SPECIFY LINE ITEM                                                
000217*                                                                         
000220*                                                                         
000221 01  WEDILIN.                                                             
000230     03 LIN-IDPTYP                             PIC X(3).                  
000240*                                              LIN                        
000501     03 LIN-LENGTH                             PIC 9(3).                  
000502*                                              LENGTH = 076               
000503*                                                                         
000504     03 LIN-1082-IDENTIFIER                    PIC X(06).                 
000505*                                                                         
000506     03 LIN-C212-PART-NO.                                                 
000507*                                                                         
000508        05 LIN-7140-PART-NO                    PIC X(35).                 
000510        PART NO (9 BYTES)                                                 
000511*                                                                         
000512     03 LIN-7143-QUAL                          PIC X(35).                 
000513*                                              IN                         
000550*** END OF VILMAII-COPY LENGTH=082                                        

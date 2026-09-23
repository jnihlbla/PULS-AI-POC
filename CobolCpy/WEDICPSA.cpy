000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI CPS DETAILS OF TRANSPORT                                         
000030*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -DETAILS OF TRANSPORT                                                
000061*    -LOOP PER DEALER                                                     
000070*                                                                         
000100 01  WEDICPS.                                                             
000230     03 CPS-IDPTYP                             PIC X(03).                 
000240*                                              CPS                        
000250     03 CPS-LENGTH                             PIC 9(03).                 
000260*                                              LENGTH = 038               
000270     03 CPS-7164-HIERARCHIC-ID                 PIC X(35).                 
000280*                                                                         
000504     03 CPS-7075-SHIP-LEVEL                    PIC X(03).                 
000505*                                              5                          
000506*                                                                         
000530*** END OF VILMAII-COPY LENGTH=44                                         

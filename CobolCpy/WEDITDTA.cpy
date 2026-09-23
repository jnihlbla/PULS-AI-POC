000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI TDT DETAILS OF TRANSPORT                                         
000030*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -DETAILS OF TRANSPORT                                                
000070*                                                                         
000100 01  WEDITDT.                                                             
000230     03 TDT-IDPTYP                             PIC X(03).                 
000240*                                              TDT                        
000250     03 TDT-LENGTH                             PIC 9(03).                 
000260*                                              LENGTH = 037               
000261*                                                                         
000270     03 TDT-8051-TRANSP-STAGE-QUAL             PIC X(03).                 
000280*                                              25                         
000504     03 TDT-C222-TRANSPORT-ID.                                            
000505*                                                                         
000506        05 TDT-8213-ID-MEANS-OF-TRPT           PIC X(35).                 
000520*       TRANSPORTID.(3 BYTES)                                             
000521*                                                                         
000530*** END OF VILMAII-COPY LENGTH=43                                         

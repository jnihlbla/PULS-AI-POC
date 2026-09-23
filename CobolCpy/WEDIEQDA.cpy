000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI EQD DETAILS OF TRANSPORT                                         
000030*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -DETAILS OF TRANSPORT                                                
000070*                                                                         
000100 01  WEDIEQD.                                                             
000230     03 EQD-IDPTYP                             PIC X(03).                 
000240*                                              EQD                        
000250     03 EQD-LENGTH                             PIC 9(03).                 
000260*                                              LENGTH = 038               
000270     03 EQD-8053-TRANSP-STAGE-QUAL             PIC X(03).                 
000280*                                              AH                         
000504     03 EQD-C224-TRANSPORT-ID.                                            
000505*                                                                         
000506        05 EQD-8154-ID-MEANS-OF-TRPT           PIC X(35).                 
000520*       CARRIER (12 BYTES)                                                
000521*                                                                         
000530*** END OF VILMAII-COPY LENGTH=44                                         

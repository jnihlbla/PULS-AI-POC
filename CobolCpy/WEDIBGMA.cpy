000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI BGM BEGINNING OF MESSAGE                                         
000030*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000050*                                                                         
000060*    FUNCTION,                                                            
000070*    -TO TRANSMIT IDENTIFYING NUMBER AT MANIFEST LEVEL                    
000080*                                                                         
000100 01  WEDIBGM.                                                             
000230     03 BGM-IDPTYP                             PIC X(03).                 
000240*                                              BGM                        
000250     03 BGM-LENGTH                             PIC 9(03).                 
000260*                                              LENGTH = 038               
000505*                                                                         
000515     03 BGM-C002-DOCUMENT-NAME.                                           
000516*                                                                         
000518        05 BGM-1001-DOCUMENT-NAME              PIC X(03).                 
000519*                                              351                        
000520     03 BGM-C106-DOCUMENT-NAME.                                           
000521*                                                                         
000526        05 BGM-1004-DOCUMENT-NUMBER            PIC X(35).                 
000527*          IDSHIPMENT (7 BYTES)                                           
000528*                                                                         
000530*** END OF VILMAII-COPY LENGTH=44                                         

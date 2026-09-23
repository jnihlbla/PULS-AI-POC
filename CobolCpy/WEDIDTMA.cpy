000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI DTM DATE/TIME/PERIOD                                             
000030*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO SPECIFY DATA AND TIME OR PERIOD                                  
000070*                                                                         
000100 01  WEDIDTM.                                                             
000230     03 DTM-IDPTYP                             PIC X(03).                 
000240*                                              DTM                        
000250     03 DTM-LENGTH                             PIC 9(03).                 
000260*                                              LENGTH = 105               
000503*                                                                         
000515     03 DTM-C107-DOCUMENT-TIME-PER.                                       
000516*                                                                         
000521        05 DTM-2005-DATE-TIME-PER-QUAL         PIC X(35).                 
000522*                                              137                        
000523*                                                                         
000524        05 DTM-2380-DATE-TIME-PER              PIC X(35).                 
000525*                                                                         
000526        05 DTM-2379-DATE-TIME-PER-FORM         PIC X(35).                 
000529*          FORMAT CCYYMMDDHHMM                 203                        
000530*                                                                         
000540*** END OF VILMAII-COPY LENGTH=111                                        

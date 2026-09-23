000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI DTM DATE/TIME/PERIOD                                             
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO SPECIFY DATA AND TIME OR PERIOD                                  
000070*                                                                         
000100 01  WEDIDTM.                                                             
000230     03 DTM-IDPTYP                             PIC X(03).                 
000240*                                              DTM                        
000501     03 DTM-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 041               
000503*                                                                         
000515     03 DTM-C507-DOCUMENT-TIME-PER.                                       
000516*                                                                         
000521        05 DTM-2005-DATE-TIME-PER-QUAL         PIC X(03).                 
000522*                                                                         
000523        05 DTM-2380-DATE-TIME-PER              PIC X(35).                 
000524*                                                                         
000525        05 DTM-2379-DATE-TIME-PER-FORM         PIC X(03).                 
000528*                                                                         
000540*** END OF VILMAII-COPY LENGTH=47                                         

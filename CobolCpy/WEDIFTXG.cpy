000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI FTX, FREE TEXT                                                   
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000031*                                                                         
000032*    FUNCTION,                                                            
000033*    -TO GIVE INFORMATION AND/OR INSTRUCTIONS IN ADDITION                 
000040*                                                                         
000100 01  WEDIFTX.                                                             
000230     03 FTX-IDPTYP                             PIC X(03).                 
000240*                                              FTX                        
000501     03 FTX-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 368               
000503*                                                                         
000515     03 FTX-4451-TEXT-SUB-QUAL                 PIC X(03).                 
000516*                                                                         
000518     03 FTX-C107-FTX-TEXT-REF.                                            
000519*                                                                         
000520        05 FTX-FILLER                          PIC X(12).                 
000521*                                                                         
000518     03 FTX-C108-FTX-TEXT-LITERAL.                                        
000519*                                                                         
000520        05 FTX-4440-FREE-TEXT                  PIC X(70).                 
000521*                                                                         
000520        05 FTX-FILLER                          PIC X(277).                
000521*                                                                         
000570*                                                                         
000580*** END OF VILMAII-COPY LENGTH=368                                        

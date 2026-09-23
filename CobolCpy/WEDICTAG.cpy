000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI CTA CONTACT INFORMATION                                          
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000031*                                                                         
000033*    FUNCTION,                                                            
000034*    -CONTACT INFORMATION.                                                
000040*                                                                         
000100 01  WEDICTA.                                                             
000230     03 CTA-IDPTYP                             PIC X(03).                 
000240*                                              CTA                        
000501     03 CTA-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 055               
000503*                                                                         
000515     03 CTA-3139-CONTACT-FUNCTION              PIC X(03).                 
000517*                                                                         
000518     03 CTA-C056-DEP-OR-EMP-DETAILS.                                      
000519*                                                                         
000520        05 CTA-3413-DEP-OR-EMP-ID              PIC X(17).                 
000521*                                                                         
000522        05 CTA-3412-DEP-OR-EMP                 PIC X(35).                 
000530*                                                                         
000570*** END OF VILMAII-COPY LENGTH=61                                         

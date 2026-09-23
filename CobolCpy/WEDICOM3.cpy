000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI COM COMUNICATION CONTACT                                         
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000031*                                                                         
000033*    FUNCTION,                                                            
000034*    -TO IDENTIFY A COMUNICATION NUMBER TO WHOM COMUNI-                   
000035*     CATION SCHOULD BE DIRECTED.                                         
000036*                                                                         
000040*                                                                         
000100 01  WEDICOM3.                                                            
000230     03 COM3-IDPTYP                            PIC X(03).                 
000240*                                              COM                        
000501     03 COM3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 028               
000503*                                                                         
000504*                                                                         
000515     03 COM3-C076-COMMUNICATION-CONTCT.                                   
000516*                                                                         
000517        05 COM3-3148-COMUNICATION-NO           PIC X(25).                 
000518*                                                                         
000519        05 COM3-3155-COMUNICATION-QUAL         PIC X(03).                 
000520*                                                                         
000530*                                                                         
000570*** END OF VILMAII-COPY LENGTH=34                                         

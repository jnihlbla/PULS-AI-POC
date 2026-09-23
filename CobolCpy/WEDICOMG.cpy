000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI COM COMUNICATION CONTACT                                         
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000031*                                                                         
000033*    FUNCTION,                                                            
000034*    -TO IDENTIFY A COMUNICATION NUMBER TO WHOM COMUNI-                   
000035*     CATION SHOULD BE DIRECTED.                                          
000036*                                                                         
000040*                                                                         
000100 01  WEDICOM.                                                             
000230     03 COM-IDPTYP                             PIC X(03).                 
000240*                                              COM                        
000501     03 COM-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 028               
000503*                                                                         
000504*                                                                         
000515     03 COM-C076-COMMUNICATION-CONT.                                      
000516*                                                                         
000517        05 COM-3148-COMUNICATION-NO            PIC X(25).                 
000518*                                                                         
000519        05 COM-3155-COMUNICATION-QUAL          PIC X(03).                 
000520*                                                                         
000530*                                                                         
000570*** END OF VILMAII-COPY LENGTH=34                                         

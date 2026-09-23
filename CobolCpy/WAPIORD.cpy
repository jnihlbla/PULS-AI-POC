000100 01  WAPIORD.                                                             
000200*                                 COPYTEXT FOR EVENTS FROM ORDER          
000300     03 IDAPIORDREF.                                                      
000400        05 IDDISTR           PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600        05 IDKUNDNR          PIC 9(6).                                    
000700*                                 KUNDNUMMER                              
000800        05 IDORDNR7          PIC 9(7).                                    
000900*                                 ORDERNUMMER                             
001000        05 TIREGDAT          PIC 9(6).                                    
001100*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001200     03 IDMSG                PIC X(3).                                    
001300*                                 MEDDELANDE NUMMER                       
001400     03 TEMFSINF             PIC X(55).                                   
001500*                                 INFORMATIONSMEDDELANDE                  
001600*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  

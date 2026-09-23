000100 01  PIED-WDI101.                                                         
000200*                                 PIE-TRANSAR HISTORIK                    
000300*                                 REGISTRERING                            
000400*                                 FYSISK NYCKEL: WDI101KY:                
000500*                                 (TIREGDAT + IDDISTR + IDKUNDNR)         
000600*                                                                         
000700     03 PIED-TIREGDAT        PIC 9(6).                                    
000800*                                 REGISTRERINGSDATUM (≈≈MMDD)             
000900*                                 REGISTRATION DATE (YYMMDD)              
001000     03 PIED-IDDISTR         PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300     03 PIED-IDKUNDNR        PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 PIED-IDORDNR7        PIC 9(7).                                    
001700*                                 ORDERNUMMER                             
001800*                                 ORDER NUMBER                            
001900*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  

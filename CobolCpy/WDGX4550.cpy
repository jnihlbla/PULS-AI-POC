000100 01  4550-WDGX4550.                                                       
000200*                                 TNTS TRANSPORT TIDER                    
000300*                                 TIDER PER KUND                          
000400*                                 FYSISK NYCKEL: KY4550                   
000500*                                 (IDDISTR + IDKUNDNR)                    
000600     03 4550-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 4550-IDKUNDNR        PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 4550-TITIDTNT        PIC X(5).                                    
001300*                                 SENAST ANKOMST TNT (HH:MM)              
001400*                                 LATEST ARRIVAL TNT (HH:MM)              
001500*** END OF VILMAII-COPY LENGTH= 12 BYTES                                  

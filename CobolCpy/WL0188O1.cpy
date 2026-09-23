000100 01  RESP-WL0188O1.                                                       
000200*                                 RESPONSE COPYTEXT TILL WL0188           
000300     03 RESP-IDSHIPM         PIC Z(6)9.                                   
000400*                                 SKEPPNINGSNUMMER                        
000500     03 RESP-KVRADER         PIC Z(4)9.                                   
000600*                                 ANTAL RADER                             
000700     03 RESP-RADER           OCCURS 100 TIMES.                            
000800*                                                                         
000900        05 RESP-IDDISTR      PIC Z(3)9.                                   
001000*                                 DISTRIKTNUMMER                          
001100        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
001200*                                 KUNDNUMMER                              
001300        05 RESP-IDKUNDNR-ORIG                                             
001400                             PIC Z(5)9.                                   
001500*                                 KUNDNUMMER                              
001600*** END OF VILMAII-COPY LENGTH= 1612 BYTES                                

000100 01  MID-W4I74101.                                                        
000200*                                 MID-COPYTEXT FÖR W40741                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-INPUT.                                                        
000800*                                 INMATNINGSFÄLT                          
000900        05 MID-INPUT-RAD     OCCURS 14 TIMES.                             
001000*                                 INMATNINGSFÄLT                          
001100           07 MID-IDKUNDNR   PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300           07 MID-IDRAPPNR   PIC X(7).                                    
001400*                                 RAPPORT NUMMER                          
001500           07 MID-KVKOLLI    PIC X(4).                                    
001600*                                 ANTAL KOLLI                             
001700           07 MID-IDFRASED   PIC X(15).                                   
001800*                                 FRAKTSEDELSNUMMER                       
001900           07 MID-TERETNOT   PIC X(20).                                   
002000*                                 FRI NOTERING RETURER                    
002100     03 MID-MODFAELT-IN      PIC X(1000).                                 
002200*** END OF VILMAII-COPY LENGTH= 1736 BYTES                                

000100 01  MID-W4I73901.                                                        
000200*                                 MID-COPYTEXT FÖR W40739                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDRAPPNR-IN      PIC X(7).                                    
000800*                                 RAPPORT NUMMER                          
000900     03 MID-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MID-IDRADNR-IN       PIC X(4).                                    
001200*                                 RADNUMMER                               
001300     03 MID-RAD-INFO         OCCURS 10 TIMES.                             
001400*                                 NYCKELFÄLT PÅ RADEN                     
001500        05 MID-IDTRANS-HOPP  PIC X(4).                                    
001600*                                 BILDNUMMER                              
001700        05 MID-IDARTNR       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900        05 MID-IDRADNR       PIC X(4).                                    
002000*                                 RADNUMMER                               
002100     03 MID-INPUT.                                                        
002200*                                 INMATNINGSFÄLT                          
002300        05 MID-IDARTNR-UPD   PIC 9(8).                                    
002400*                                 ARTIKELNUMMER                           
002500        05 MID-IDRADNR-UPD   PIC 9(4).                                    
002600*                                 RADNUMMER                               
002700*** END OF VILMAII-COPY LENGTH= 212 BYTES                                 

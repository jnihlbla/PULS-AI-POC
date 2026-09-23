000100 01  MID-W4I70501.                                                        
000200*                                 MID-COPYTEXT FÖR W4070500               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDRAPPNR-IN      PIC X(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 MID-IDRAPPNR-UT      PIC X(7).                                    
001400*                                 RAPPORT NUMMER                          
001500     03 MID-RELANDCO         PIC X(6).                                    
001600*                                 LANDING COST PROCENT                    
001700     03 MID-PRFRAKT          PIC X(10).                                   
001800*                                 FRAKTKOSTNAD                            
001900     03 MID-PRFOERS          PIC X(10).                                   
002000*                                 FÖRSÄKRINGSPREMIE                       
002100     03 MID-PRLEGKST         PIC X(10).                                   
002200*                                 LEGALISERINSKOSTNAD                     
002300     03 MID-IDORDNR5         PIC X(5).                                    
002400*                                 ORDERNUMMER                             
002500     03 MID-IDFAKT           PIC X(7).                                    
002600*                                 FAKTURANUMMER                           
002700     03 MID-IDPRODNR         PIC X(7).                                    
002800*                                 PRODUKTIONSNUMMER                       
002900     03 MID-KOLLI-FROM-TO    OCCURS 13 TIMES.                             
003000*                                 KOLLI INTERVALL                         
003100        05 MID-IDKOLLI-FOM   PIC X(5).                                    
003200*                                 KOLLINUMMER                             
003300        05 MID-IDKOLLI-TOM   PIC X(5).                                    
003400*                                 KOLLINUMMER                             
003500*** END OF VILMAII-COPY LENGTH= 219 BYTES                                 

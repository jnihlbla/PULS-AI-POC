000100 01  REQU-WL0198I1.                                                       
000200*                                 REQUEST TO PGM WL0198                   
000300     03 REQU-L198-IDDC-KEY   PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-L198-IDANSTNR-KEY                                            
000600                             PIC 9(5).                                    
000700*                                 ANSTÄLLNINGSNUMMER                      
000800     03 REQU-L198-IDDISTR-KEY                                             
000900                             PIC 9(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 REQU-L198-IDKUNDNR-KEY                                            
001200                             PIC 9(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 REQU-L198-IDORDNR-KEY                                             
001500                             PIC 9(5).                                    
001600*                                 ORDERNUMMER                             
001700     03 REQU-L198-IDKOLLI-KEY                                             
001800                             PIC 9(5).                                    
001900*                                 KOLLINUMMER                             
002000     03 REQU-L198-IDPRODNR-KEY                                            
002100                             PIC 9(7).                                    
002200*                                 PRODUKTIONSNUMMER                       
002300     03 REQU-L198-RAD        OCCURS 15 TIMES.                             
002400        05 REQU-L198-IDRADNR PIC 9(4).                                    
002500*                                 RADNUMMER                               
002600        05 REQU-L198-KDARTURS                                             
002700                             PIC X(2).                                    
002800*                                 ARTIKELURSPRUNGSKOD                     
002900*** END OF VILMAII-COPY LENGTH= 124 BYTES                                 

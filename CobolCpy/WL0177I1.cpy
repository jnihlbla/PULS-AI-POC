000100 01  REQU-WL0177I1.                                                       
000200*                                 REQUEST TO PGM WL0177                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDRT-KEY        PIC X(3).                                    
000600*                                 RETURTERMINAL                           
000700     03 REQU-INV-RE0-GRP.                                                 
000800*                                 INVENTERINGSANTALGRUPP                  
000900        05 REQU-KVINVBEG     PIC 9(3).                                    
001000*                                 ANTAL ATT INVENTERA                     
001100        05 REQU-KDVVKL       PIC 9.                                       
001200*                                 VOLYMVÄRDESKLASS                        
001300        05 REQU-ADLAGOMR     PIC 9(2).                                    
001400*                                 LAGEROMRÅDE                             
001500        05 REQU-ADGANG       PIC 9(2).                                    
001600*                                 GÅNG                                    
001700        05 REQU-ADPLATS      PIC 9(5).                                    
001800*                                 LAGERPLATSNUMMER                        
001900        05 REQU-KDPRODSL     PIC 9(2).                                    
002000*                                 PRODUKTSLAG                             
002100        05 REQU-IDFKNGRP     PIC 9(4).                                    
002200*                                 FUNKTIONSGRUPP                          
002300*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  

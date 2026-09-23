000100 01  RESP-WL0177O1.                                                       
000200*                                 RESPONS FROM PGM WL0177                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-RAD-IN.                                                      
000600*                                 RAPPORTERINGS-FÄLT                      
000700        05 RESP-KVINVBEG-IN  PIC Z(2)9.                                   
000800*                                 ANTAL ATT INVENTERA                     
000900        05 RESP-KDVVKL-IN    PIC 9.                                       
001000*                                 VOLYMVÄRDESKLASS                        
001100        05 RESP-ADLAGOMR-IN  PIC 9(2).                                    
001200*                                 LAGEROMRÅDE                             
001300        05 RESP-ADGANG-IN    PIC 9(2).                                    
001400*                                 GÅNG                                    
001500        05 RESP-ADPLATS-IN   PIC 9(5).                                    
001600*                                 LAGERPLATSNUMMER                        
001700        05 RESP-KDPRODSL-IN  PIC Z9.                                      
001800*                                 PRODUKTSLAG                             
001900        05 RESP-IDFKNGRP-IN  PIC Z(3)9.                                   
002000*                                 FUNKTIONSGRUPP                          
002100*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  

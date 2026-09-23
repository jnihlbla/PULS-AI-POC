000100 01  MOD-W6O15101.                                                        
000200*                                 COPYTEXT FOR MOD W6O15101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC Z(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC Z(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-KVLS-CDC         PIC -(6)9.                                   
001200*                                 LAGERSALDO                              
001300     03 MOD-ADLAGOMR-CDC     PIC Z9.                                      
001400*                                 LAGEROMRÅDE                             
001500     03 MOD-ADGANG-CDC       PIC Z9.                                      
001600*                                 GÅNG                                    
001700     03 MOD-ADPLATS-CDC      PIC Z(4)9.                                   
001800*                                 LAGERPLATSNUMMER                        
001900     03 MOD-RAD              OCCURS 5 TIMES.                              
002000*                                 LINES                                   
002100        05 MOD-CMD-ATTR      PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-CMD           PIC X(3).                                    
002400        05 MOD-KVANTAL-JUST-ATTR                                          
002500                             PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-KVANTAL-JUST  PIC Z(7).                                    
002800*                                 ANTAL                                   
002900        05 MOD-KVLS          PIC -(7)9.                                   
003000*                                 LAGERSALDO                              
003100        05 MOD-ADLAGOMR      PIC Z9.                                      
003200*                                 LAGEROMRÅDE                             
003300        05 MOD-ADGANG        PIC Z9.                                      
003400*                                 GÅNG                                    
003500        05 MOD-ADPLATS       PIC Z(4)9.                                   
003600*                                 LAGERPLATSNUMMER                        
003700        05 MOD-TYP           PIC X(3).                                    
003800     03 MOD-TEMFSINF         PIC X(55).                                   
003900*                                 INFORMATIONSMEDDELANDE                  
004000*** END OF VILMAII-COPY LENGTH= 303 BYTES                                 

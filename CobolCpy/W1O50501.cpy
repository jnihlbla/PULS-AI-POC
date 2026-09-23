000100 01  MOD-W1O50501.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 1505.             
000300*                                 FRÅGA TILLÄGGSTEXT                      
000400*                                 INOM KATALOGEN.                         
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 MOD-BETTEXT-ATTR     PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-BETTEXT-SOEK-IN  PIC X(25).                                   
001200*                                 SÖKORD, DEL AV TILLÄGGSTEXT             
001300     03 MOD-BETTEXT-SOEK-UT  PIC X(25).                                   
001400*                                 SÖKORD, DEL AV TILLÄGGSTEXT             
001500     03 MOD-IDSKYLT-ATTR     PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDSKYLT-IN       PIC X(3).                                    
001800*                                 NATIONALITETSTECKEN IN                  
001900     03 MOD-IDSKYLT-UT       PIC X(3).                                    
002000*                                 NATIONALITETSTECKEN UT                  
002100     03 MOD-IDTTEXNR-IN      PIC X(5).                                    
002200*                                 TILLÄGGSTEXT-NR                         
002300     03 MOD-IDTTEXNR-UT      PIC X(5).                                    
002400*                                 TILLÄGGSTEXT-NR                         
002500     03 MOD-IDTTEXNR-SPAR    PIC 9(5).                                    
002600*                                 TILLÄGGSTEXT-NR                         
002700     03 MOD-UTRAD            OCCURS 14 TIMES.                             
002800        05 MOD-IDTTEXNR      PIC Z(4)9.                                   
002900*                                 TILLÄGGSTEXT-NR                         
003000        05 MOD-FILLER        PIC X(3).                                    
003100        05 MOD-BETTEXT       PIC X(25).                                   
003200*                                 TILLÄGGSTEXT                            
003300        05 MOD-FILLER        PIC X(2).                                    
003400        05 MOD-IDSKYLT-RAD   OCCURS 10 TIMES.                             
003500           07 MOD-IDSKYLT    PIC X(3).                                    
003600*                                 NATIONALITETSTECKEN                     
003700*                                 SPRÅKIDENTIFIKATION                     
003800           07 MOD-FILLER     PIC X.                                       
003900     03 MOD-TEMFSINF         PIC X(55).                                   
004000*                                 INFORMATIONSMEDDELANDE                  
004100*** END OF VILMAII-COPY LENGTH= 1224 BYTES                                

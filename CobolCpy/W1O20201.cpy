000100 01  MOD-W1O20201.                                                        
000200*                                 MOD-COPYTEXT FÖR W1020200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDSKYLT-ATTR     PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDSKYLT          PIC X(3).                                    
001000*                                 NATIONALITETSTECKEN                     
001100*                                 SPRÅKIDENTIFIKATION                     
001200     03 MOD-INFO-RAD         OCCURS 6 TIMES.                              
001300*                                 RADINFORMATION                          
001400        05 MOD-IDARTNR-STR-ATTR                                           
001500                             PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700        05 MOD-IDARTNR-STR   PIC Z(7)9.                                   
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-IDNODE-ATTR      PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-IDNODE           PIC X(8).                                    
002200*                                 VTAM NODE-NAMN                          
002300     03 MOD-TEMFSINF         PIC X(55).                                   
002400*                                 INFORMATIONSMEDDELANDE                  
002500*** END OF VILMAII-COPY LENGTH= 174 BYTES                                 

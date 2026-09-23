000100 01  RESP-WL0198O1.                                                       
000200*                                 RESPONS FROM PGM WL0198                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDANSTNR-KEY    PIC Z(4)9.                                   
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000800*                                 DISTRIKTNUMMER                          
000900     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
001000*                                 KUNDNUMMER                              
001100     03 RESP-IDORDNR-KEY     PIC Z(4)9.                                   
001200*                                 ORDERNUMMER                             
001300     03 RESP-IDKOLLI-KEY     PIC Z(4)9.                                   
001400*                                 KOLLINUMMER                             
001500     03 RESP-IDPRODNR-KEY    PIC Z(6)9.                                   
001600*                                 PRODUKTIONSNUMMER                       
001700     03 RESP-KVRADER         PIC Z(4)9.                                   
001800*                                 ANTAL RADER                             
001900     03 RESP-RAD             OCCURS 15 TIMES.                             
002000        05 RESP-IDRADNR      PIC Z(3)9.                                   
002100*                                 RADNUMMER                               
002200        05 RESP-KDARTURS     PIC X(2).                                    
002300*                                 ARTIKELURSPRUNGSKOD                     
002400        05 RESP-IDMSG-ERROR-LINE                                          
002500                             PIC X(3).                                    
002600*                                 FELMEDDELANDE ID                        
002700*** END OF VILMAII-COPY LENGTH= 174 BYTES                                 

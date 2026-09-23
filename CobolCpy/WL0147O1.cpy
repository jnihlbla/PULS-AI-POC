000100 01  RESP-WL0147O1.                                                       
000200*                                 RESPONS FROM PGM WL0147                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDRT-KEY        PIC X(3).                                    
000600*                                 RETURTERMINAL                           
000700     03 RESP-IDRTLOP-KEY     PIC 9(3).                                    
000800*                                 RETUR TERMINAL LÖPNUMMER                
000900     03 RESP-IDKOLLI-KEY     PIC Z(4)9.                                   
001000*                                 KOLLINUMMER                             
001100     03 RESP-IDFRASED        PIC X(15).                                   
001200*                                 FRAKTSEDELSNUMMER                       
001300     03 RESP-KVRADER         PIC Z(4)9.                                   
001400*                                 ANTAL RADER                             
001500     03 RESP-RADER           OCCURS 500 TIMES.                            
001600*                                                                         
001700        05 RESP-KDCMD        PIC X(4).                                    
001800        05 RESP-ADINLOMR-UPD PIC X(4).                                    
001900*                                 INLEVERANSOMRÅDE                        
002000        05 RESP-ADINLOMR     PIC X(4).                                    
002100*                                 INLEVERANSOMRÅDE                        
002200        05 RESP-IDKOLLI      PIC Z(4)9.                                   
002300*                                 KOLLINUMMER                             
002400        05 RESP-FLFARLIG     PIC X.                                       
002500*                                 FARLIGT GODS-FLAGGA                     
002600        05 RESP-BESTATUS     PIC X(4).                                    
002700        05 RESP-IDMSG-ERROR-LINE                                          
002800                             PIC X(3).                                    
002900*                                 FELMEDDELANDE ID                        
003000*** END OF VILMAII-COPY LENGTH= 12533 BYTES                               

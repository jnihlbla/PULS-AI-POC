000100 01  RESP-WL0149O1.                                                       
000200*                                 RESPONS FROM PGM WL0149                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDANSV-KEY      PIC X(6).                                    
000600     03 RESP-IDRT-KEY        PIC X(3).                                    
000700*                                 RETURTERMINAL                           
000800     03 RESP-IDRTLOP-KEY     PIC 9(3).                                    
000900*                                 RETUR TERMINAL LÖPNUMMER                
001000     03 RESP-IDKOLLI-KEY     PIC Z(5).                                    
001100*                                 KOLLINUMMER                             
001200     03 RESP-IDDISTR-KEY     PIC Z(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 RESP-FLVISAAV-KEY    PIC X.                                       
001500*                                 ALLMÄN FLAGGA                           
001600     03 RESP-IDANSTNR        PIC Z(5).                                    
001700*                                 ANSTÄLLNINGSNUMMER                      
001800     03 RESP-KVRADER         PIC Z(4)9.                                   
001900*                                 ANTAL RADER                             
002000     03 RESP-RADER           OCCURS 500 TIMES.                            
002100*                                                                         
002200        05 RESP-KDCMD        PIC X(4).                                    
002300        05 RESP-IDRT         PIC X(3).                                    
002400*                                 RETURTERMINAL                           
002500        05 RESP-IDRTLOP      PIC 9(3).                                    
002600*                                 RETUR TERMINAL LÖPNUMMER                
002700        05 RESP-IDKOLLI      PIC Z(4)9.                                   
002800*                                 KOLLINUMMER                             
002900        05 RESP-FLFARLIG     PIC X.                                       
003000*                                 FARLIGT GODS-FLAGGA                     
003100        05 RESP-TIRETANK     PIC 9(6).                                    
003200*                                 ANKOMSTDATUM                            
003300        05 RESP-ADINLOMR     PIC X(4).                                    
003400*                                 INLEVERANSOMRÅDE                        
003500        05 RESP-BESTATUS     PIC X(4).                                    
003600        05 RESP-IDMSG-ERROR-LINE                                          
003700                             PIC X(3).                                    
003800*                                 FELMEDDELANDE ID                        
003900*** END OF VILMAII-COPY LENGTH= 16534 BYTES                               

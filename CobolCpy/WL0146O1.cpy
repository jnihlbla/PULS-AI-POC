000100 01  RESP-WL0146O1.                                                       
000200*                                 RESPONS FROM PGM WL0146                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-KDRETSTA-KEY    PIC X.                                       
000600*                                 STATUS RETURER                          
000700     03 RESP-IDRT-KEY        PIC X(3).                                    
000800*                                 RETURTERMINAL                           
000900     03 RESP-IDRTLOP-KEY     PIC 9(3).                                    
001000*                                 RETUR TERMINAL LÖPNUMMER                
001100     03 RESP-INPUT.                                                       
001200*                                                                         
001300        05 RESP-IDANSTNR     PIC Z(4)9.                                   
001400*                                 ANSTÄLLNINGSNUMMER                      
001500        05 RESP-ADINLOMR     PIC X(4).                                    
001600*                                 INLEVERANSOMRÅDE                        
001700     03 RESP-KVRADER         PIC Z(4)9.                                   
001800*                                 ANTAL RADER                             
001900     03 RESP-RADER           OCCURS 500 TIMES.                            
002000*                                                                         
002100        05 RESP-KDCMD        PIC X(4).                                    
002200        05 RESP-IDRT         PIC X(3).                                    
002300*                                 RETURTERMINAL                           
002400        05 RESP-IDRTLOP      PIC 9(3).                                    
002500*                                 RETUR TERMINAL LÖPNUMMER                
002600        05 RESP-TISNDDAT     PIC 9(6).                                    
002700*                                 SÄNDNINGSDATUM     (ÅÅMMDD)             
002800        05 RESP-KVKOLLI-SND  PIC Z(4)9.                                   
002900*                                 ANTAL KOLLI                             
003000        05 RESP-TIINLMOT     PIC 9(6).                                    
003100*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
003200        05 RESP-ADINLOMR-MOT PIC X(4).                                    
003300*                                 INLEVERANSOMRÅDE                        
003400        05 RESP-IDANSTNR-MOT PIC Z(4)9.                                   
003500*                                 ANSTÄLLNINGSNUMMER                      
003600        05 RESP-KVKOLLI-MOT  PIC Z(4)9.                                   
003700*                                 ANTAL KOLLI                             
003800        05 RESP-IDMSG-ERROR-LINE                                          
003900                             PIC X(3).                                    
004000*                                 FELMEDDELANDE ID                        
004100*** END OF VILMAII-COPY LENGTH= 22023 BYTES                               

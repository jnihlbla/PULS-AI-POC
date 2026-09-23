000100 01  RESP-WL0151O1.                                                       
000200*                                 RESPONS FROM PGM WL0151                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000800*                                 KUNDNUMMER                              
000900     03 RESP-KDLEVANM-KEY    PIC X.                                       
001000*                                 STATUS LEVERANSANMÄRKNING               
001100     03 RESP-FLSUM-KEY       PIC X.                                       
001200*                                 ALLMÄN FLAGGA                           
001300     03 RESP-WL0154I1.                                                    
001400        05 RESP-IDPGM-REP    PIC X(8).                                    
001500*                                 PROGRAM IDENTITET                       
001600        05 RESP-L154-RT-POST OCCURS 10 TIMES.                             
001700           07 RESP-IDDISTR-REP                                            
001800                             PIC Z(3)9.                                   
001900*                                 DISTRIKTNUMMER                          
002000           07 RESP-IDKUNDNR-REP                                           
002100                             PIC Z(5)9.                                   
002200*                                 KUNDNUMMER                              
002300           07 RESP-IDRAPPNR-REP                                           
002400                             PIC Z(6)9.                                   
002500*                                 RAPPORT NUMMER                          
002600     03 RESP-KVANT-RT        PIC Z(4)9.                                   
002700     03 RESP-KVRADER-RT      PIC Z(4)9.                                   
002800*                                 ANTAL RADER                             
002900     03 RESP-KVRADER         PIC Z(4)9.                                   
003000*                                 ANTAL RADER                             
003100     03 RESP-RADER           OCCURS 2000 TIMES.                           
003200*                                                                         
003300        05 RESP-FLCMD        PIC X.                                       
003400        05 RESP-TIRETILL     PIC 9(6).                                    
003500*                                 RETURTILLSTÅNDSDATUM                    
003600        05 RESP-TIRETANK     PIC 9(6).                                    
003700*                                 ANKOMSTDATUM                            
003800        05 RESP-IDDISTR      PIC Z(3)9.                                   
003900*                                 DISTRIKTNUMMER                          
004000        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
004100*                                 KUNDNUMMER                              
004200        05 RESP-IDRAPPNR     PIC Z(6)9.                                   
004300*                                 RAPPORT NUMMER                          
004400        05 RESP-KVRADER-TOT  PIC Z(4)9.                                   
004500*                                 ANTAL RADER                             
004600        05 RESP-KVRADER-OBEH PIC Z(4)9.                                   
004700*                                 ANTAL RADER                             
004800        05 RESP-KVKOLLI      PIC Z(3)9.                                   
004900*                                 ANTAL KOLLI                             
005000        05 RESP-IDRETSND.                                                 
005100*                                 RETURSÄNDNING                           
005200           07 RESP-IDRT      PIC X(3).                                    
005300*                                 RETURTERMINAL                           
005400           07 RESP-IDRTLOP   PIC Z(2)9.                                   
005500*                                 RETUR TERMINAL LÖPNUMMER                
005600*** END OF VILMAII-COPY LENGTH= 100207 BYTES                              

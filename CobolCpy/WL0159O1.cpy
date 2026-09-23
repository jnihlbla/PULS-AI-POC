000100 01  RESP-WL0159O1.                                                       
000200*                                 RESPONS FROM PGM WL0159                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000800*                                 KUNDNUMMER                              
000900     03 RESP-KDRETSTA-KEY    PIC X.                                       
001000*                                 STATUS RETURER                          
001100     03 RESP-KVRADER         PIC Z(4)9.                                   
001200*                                 ANTAL RADER                             
001300     03 RESP-RADER           OCCURS 500 TIMES.                            
001400*                                                                         
001500        05 RESP-FLCMD        PIC X.                                       
001600        05 RESP-IDRT         PIC X(3).                                    
001700*                                 RETURTERMINAL                           
001800        05 RESP-IDRTLOP      PIC 9(3).                                    
001900*                                 RETUR TERMINAL LÖPNUMMER                
002000        05 RESP-TIREGDAT     PIC 9(6).                                    
002100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002200        05 RESP-TISNDDAT     PIC 9(6).                                    
002300*                                 SÄNDNINGSDATUM     (ÅÅMMDD)             
002400        05 RESP-IDRT-TRANSIT PIC X(3).                                    
002500*                                 TRANSIT RETURTERMINAL                   
002600        05 RESP-TIREGDAT-TRRT                                             
002700                             PIC 9(6).                                    
002800*                                 REG. DATUM I TRANSIT RET.TERM.          
002900        05 RESP-TISNDDAT-TRRT                                             
003000                             PIC 9(6).                                    
003100*                                 SÄND. DATUM FRÅN TRANSIT RT             
003200        05 RESP-IDDISTR      PIC Z(3)9.                                   
003300*                                 DISTRIKTNUMMER                          
003400        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
003500*                                 KUNDNUMMER                              
003600        05 RESP-IDRAPPNR     PIC Z(6)9.                                   
003700*                                 RAPPORT NUMMER                          
003800        05 RESP-KVRADER-LINE PIC Z(4)9.                                   
003900*                                 ANTAL RADER                             
004000        05 RESP-KVKOLLI-AAF  PIC Z(3)9.                                   
004100*                                 ANTAL KOLLI                             
004200        05 RESP-IDMSG-ERROR-LINE                                          
004300                             PIC X(3).                                    
004400*                                 FELMEDDELANDE ID                        
004500*** END OF VILMAII-COPY LENGTH= 31518 BYTES                               

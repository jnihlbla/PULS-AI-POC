000100 01  RESP-WL0160O1.                                                       
000200*                                 RESPONS FROM PGM WL0160                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-KVRADER         PIC Z(4)9.                                   
000600*                                 ANTAL RADER                             
000700     03 RESP-RADER           OCCURS 500 TIMES.                            
000800*                                                                         
000900        05 RESP-IDANSV       PIC X(6).                                    
001000        05 RESP-TILEVANM     PIC 9(6).                                    
001100*                                 DATUM LEVERANSANMÄRKNING                
001200        05 RESP-IDDISTR      PIC Z(3)9.                                   
001300*                                 DISTRIKTNUMMER                          
001400        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
001500*                                 KUNDNUMMER                              
001600        05 RESP-IDRAPPNR     PIC Z(6)9.                                   
001700*                                 RAPPORT NUMMER                          
001800        05 RESP-IDORDNR5     PIC Z(4)9.                                   
001900*                                 ORDERNUMMER                             
002000        05 RESP-IDRADNR      PIC Z(3)9.                                   
002100*                                 RADNUMMER                               
002200        05 RESP-IDARTNR      PIC Z(7)9.                                   
002300*                                 ARTIKELNUMMER                           
002400        05 RESP-KVLEVANM-BEKR                                             
002500                             PIC Z(5)9.                                   
002600*                                 BEKRÄFTAT RETURANTAL                    
002700        05 RESP-KDANMORS     PIC X(2).                                    
002800*                                 ORSAK TILL LEVERANSANMÄRKNING           
002900        05 RESP-FLTEXT       PIC X.                                       
003000*                                 FINNS TEXTINFORMATION ?                 
003100        05 RESP-KDKREBEH     PIC X(3).                                    
003200*                                 BEHANDLINGSSTATUS                       
003300*** END OF VILMAII-COPY LENGTH= 29007 BYTES                               

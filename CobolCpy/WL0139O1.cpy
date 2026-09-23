000100 01  RESP-WL0139O1.                                                       
000200*                                 RESPONS FROM PGM WL0139                 
000300     03 RESP-IDDC-KEY3       PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDARTNR-KEY3    PIC Z(7)9.                                   
000600*                                 ARTIKELNUMMER                           
000700     03 RESP-IDDISTR-KEY3    PIC Z(3)9.                                   
000800*                                 DISTRIKTNUMMER                          
000900     03 RESP-IDKUNDNR-KEY3   PIC Z(5)9.                                   
001000*                                 KUNDNUMMER                              
001100     03 RESP-KDANMORS-KEY3   PIC X(2).                                    
001200*                                 ORSAK TILL LEVERANSANMÄRKNING           
001300     03 RESP-FL-IDDC         PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500     03 RESP-KVRADER         PIC Z(4)9.                                   
001600*                                 ANTAL RADER                             
001700     03 RESP-RADER           OCCURS 500 TIMES.                            
001800*                                 RADINFORMATION                          
001900        05 RESP-FLCMD        PIC X.                                       
002000*                                 ALLMÄN FLAGGA                           
002100        05 RESP-IDDISTR      PIC Z(3)9.                                   
002200*                                 DISTRIKTNUMMER                          
002300        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
002400*                                 KUNDNUMMER                              
002500        05 RESP-IDRAPPNR     PIC Z(6)9.                                   
002600*                                 RAPPORT NUMMER                          
002700        05 RESP-IDDC         PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900        05 RESP-TILEVANM     PIC 9(6).                                    
003000*                                 DATUM LEVERANSANMÄRKNING                
003100        05 RESP-KDANMORS     PIC X(2).                                    
003200*                                 ORSAK TILL LEVERANSANMÄRKNING           
003300        05 RESP-KDKREBEH     PIC X(3).                                    
003400*                                 BEHANDLINGSSTATUS                       
003500        05 RESP-KVLEVANM     PIC Z(5)9.                                   
003600*                                 LEVERANSANMÄRKNINGSANTAL                
003700        05 RESP-DATUM-TEXT   PIC X(3).                                    
003800        05 RESP-TIRETILL     PIC 9(6).                                    
003900*                                 RETURTILLSTÅNDSDATUM                    
004000        05 RESP-KVRETINL     PIC Z(5)9.                                   
004100*                                 INLAGT ANTAL VID RETUR                  
004200        05 RESP-KVRETINL-SKR PIC Z(5)9.                                   
004300*                                 INRPT ANTAL SOM SKROTATS                
004400        05 RESP-FLTEXT       PIC X.                                       
004500*                                 FINNS TEXTINFORMATION ?                 
004600        05 RESP-IDRADNR      PIC Z(3)9.                                   
004700*                                 RADNUMMER                               
004800*** END OF VILMAII-COPY LENGTH= 31528 BYTES                               

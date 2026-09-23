000100 01  RESP-WL0163O1.                                                       
000200*                                 RESPONS FROM PGM WL0163                 
000300     03 RESP-IDDC-KEY2       PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY2    PIC Z(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY2   PIC Z(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDRAPPNR-KEY2   PIC Z(7).                                    
001000*                                 RAPPORT NUMMER                          
001100     03 RESP-IDARTNR-KEY2    PIC Z(8).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 RESP-IDRADNR-KEY2    PIC Z(4).                                    
001400*                                 RADNUMMER                               
001500     03 RESP-TIRETILL        PIC 9(6).                                    
001600*                                 RETURTILLSTÅNDSDATUM                    
001700     03 RESP-IDANSTNR-RET    PIC Z(4)9.                                   
001800*                                 ANSTÄLLNINGSNUMMER RETURAVDELN.         
001900     03 RESP-KVRADER         PIC Z(4)9.                                   
002000*                                 ANTAL RADER                             
002100     03 RESP-LEVANM-RAD      OCCURS 500 TIMES.                            
002200*                                 LEVERANSANM.RAD                         
002300        05 RESP-IDARTNR      PIC Z(7)9.                                   
002400*                                 ARTIKELNUMMER                           
002500        05 RESP-IDRADNR      PIC Z(3)9.                                   
002600*                                 RADNUMMER                               
002700        05 RESP-KDANMORS     PIC X(2).                                    
002800*                                 ORSAK TILL LEVERANSANMÄRKNING           
002900        05 RESP-FLRETILL     PIC X(2).                                    
003000        05 RESP-KVLEVANM-BEKR                                             
003100                             PIC Z(5)9.                                   
003200*                                 BEKRÄFTAT RETURANTAL                    
003300        05 RESP-KVLEVANM     PIC Z(5)9.                                   
003400*                                 LEVERANSANMÄRKNINGSANTAL                
003500        05 RESP-TIRETANK     PIC 9(6).                                    
003600*                                 ANKOMSTDATUM                            
003700        05 RESP-TIINLINL     PIC 9(6).                                    
003800*                                 RAPPORTERINGSDATUM INLAGD (R32)         
003900        05 RESP-KVRETINL     PIC Z(5)9.                                   
004000*                                 INLAGT ANTAL VID RETUR                  
004100        05 RESP-KVRETINL-SKR PIC Z(5)9.                                   
004200*                                 INRPT ANTAL SOM SKROTATS                
004300        05 RESP-KVAVV-KVANT  PIC Z(6)9.                                   
004400*                                 ANTALSAVVIKELSE KVANTITET               
004500        05 RESP-KVAVV-KVAL   PIC Z(6)9.                                   
004600*                                 ANTALSAVVIKELSE KVALITET                
004700        05 RESP-FLTEXT       PIC X.                                       
004800*                                 FINNS TEXTINFORMATION ?                 
004900*** END OF VILMAII-COPY LENGTH= 33547 BYTES                               

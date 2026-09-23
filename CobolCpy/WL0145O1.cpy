000100 01  RESP-WL0145O1.                                                       
000200*                                 RESPONS FROM PGM WL0145                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDFAKT-KEY      PIC Z(7).                                    
000600*                                 FAKTURANUMMER                           
000700     03 RESP-IDKUNDNR-KEY    PIC Z(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDORDNR-KEY     PIC Z(5).                                    
001000*                                 ORDERNUMMER UTGÅR PD90                  
001100     03 RESP-IDKOLLI-KEY     PIC Z(5).                                    
001200*                                 KOLLINUMMER                             
001300     03 RESP-IDPRODNR-KEY    PIC Z(7).                                    
001400*                                 PRODUKTIONSNUMMER                       
001500     03 RESP-IDARTNR-KEY     PIC Z(8).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 RESP-KVRADER         PIC Z(4)9.                                   
001800*                                 ANTAL RADER                             
001900     03 RESP-RADER           OCCURS 500 TIMES.                            
002000*                                 RADINFORMATION                          
002100        05 RESP-IDDISTR      PIC Z(3)9.                                   
002200*                                 DISTRIKTNUMMER                          
002300        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
002400*                                 KUNDNUMMER                              
002500        05 RESP-IDORDNR7     PIC Z(6)9.                                   
002600*                                 ORDERNUMMER                             
002700        05 RESP-IDPRODNR     PIC Z(6)9.                                   
002800*                                 PRODUKTIONSNUMMER                       
002900        05 RESP-IDDC         PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100        05 RESP-IDKOLLI      PIC Z(4)9.                                   
003200*                                 KOLLINUMMER                             
003300        05 RESP-IDARTNR      PIC Z(7)9.                                   
003400*                                 ARTIKELNUMMER                           
003500        05 RESP-KVBEART      PIC Z(5)9.                                   
003600*                                 BESTÄLLT ANTAL STYCKEN                  
003700        05 RESP-KVAVBART     PIC Z(5)9.                                   
003800*                                 AVBOKAT ANTAL ARTIKLAR                  
003900        05 RESP-KVLEVART     PIC Z(5)9.                                   
004000*                                 LEVERERAT ANTAL STYCK                   
004100        05 RESP-IDLEVNR      PIC X(5).                                    
004200*                                 LEVERANTÖRNUMMER                        
004300*** END OF VILMAII-COPY LENGTH= 31045 BYTES                               

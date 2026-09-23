000100 01  RESP-WL0118O1.                                                       
000200*                                 RESPONS FROM PGM WL0118                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDANSTNR-KEY    PIC Z(4)9.                                   
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000800*                                 DISTRIKTNUMMER                          
000900     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
001000*                                 KUNDNUMMER                              
001100     03 RESP-IDORDNR-KEY     PIC Z(4)9.                                   
001200*                                 ORDERNUMMER UTGÅR PD90                  
001300     03 RESP-IDPURAD-KEY     PIC X(5).                                    
001400     03 RESP-IDPRODNR-KEY    PIC Z(6)9.                                   
001500*                                 PRODUKTIONSNUMMER                       
001600     03 RESP-KVRADER         PIC Z(4)9.                                   
001700*                                 ANTAL RADER                             
001800     03 RESP-RAD             OCCURS 500 TIMES.                            
001900*                                 TABELL INNEHÅLLANDE RADER.              
002000        05 RESP-IDRONR       PIC Z(4)9.                                   
002100*                                 RESTORDERNUMMER                         
002200        05 RESP-ADLAGOMR     PIC 9(2).                                    
002300*                                 LAGEROMRÅDE                             
002400        05 RESP-ADGANG       PIC 9(2).                                    
002500*                                 GÅNG                                    
002600        05 RESP-ADPLATS      PIC 9(5).                                    
002700*                                 LAGERPLATSNUMMER                        
002800        05 RESP-IDPURAD      PIC Z(3)9.                                   
002900*                                 RADNUMMER PÅ PACKUNDERLAG               
003000        05 RESP-IDARTNR      PIC Z(7)9.                                   
003100*                                 ARTIKELNUMMER                           
003200        05 RESP-KDARTURS     PIC X(2).                                    
003300*                                 ARTIKELURSPRUNGSKOD                     
003400        05 RESP-BEART        PIC X(15).                                   
003500        05 RESP-KVAVBART     PIC Z(5)9.                                   
003600*                                 AVBOKAT ANTAL ARTIKLAR                  
003700        05 RESP-KVLEVART     PIC Z(6)9.                                   
003800*                                 LEVERERAT ANTAL STYCK                   
003900        05 RESP-IDKOLLI      PIC Z(4)9.                                   
004000*                                 KOLLINUMMER                             
004100*** END OF VILMAII-COPY LENGTH= 30539 BYTES                               

000100 01  MOD-W4O36701.                                                        
000200*                                 MOD-COPYTEXT FÖR W4036700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-IDKUNDNR-ENTER   PIC 9(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 MOD-IDKUNDNR-NEXT    PIC 9(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 MOD-TABELLRAD        OCCURS 36 TIMES.                             
002400*                                 GRUPP MED TABELLRADER                   
002500        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
002600*                                 KUNDNUMMER                              
002700        05 MOD-IDGMTOMR      PIC Z(3)9.                                   
002800*                                 GODSMOTTAGAREOMRÅDE                     
002900        05 MOD-IDPRCTAB      PIC Z9.                                      
003000*                                 PRCTABELLIDENTITET                      
003100        05 MOD-IDPKLTAB      PIC X(2).                                    
003200*                                 PRODUKTIONSKLASSTABELLSID               
003300     03 MOD-TEMFSINF         PIC X(55).                                   
003400*                                 INFORMATIONSMEDDELANDE                  
003500*** END OF VILMAII-COPY LENGTH= 639 BYTES                                 

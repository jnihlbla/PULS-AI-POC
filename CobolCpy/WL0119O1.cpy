000100 01  RESP-WL0119O1.                                                       
000200*                                 RESPONS FROM PGM WL0119                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-KDPRCGRP-KEY    PIC X(5).                                    
000600*                                 PRODUKTIONSKANALSGRUPP                  
000700     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000800*                                 DISTRIKTNUMMER                          
000900     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
001000*                                 KUNDNUMMER                              
001100     03 RESP-IDORDNR-KEY     PIC Z(4)9.                                   
001200*                                 ORDERNUMMER                             
001300     03 RESP-KDORDKL-KEY     PIC 9.                                       
001400*                                 ORDERKLASS                              
001500     03 RESP-IDPRODNR-KEY    PIC Z(7).                                    
001600*                                 PRODUKTIONSNUMMER                       
001700     03 RESP-KVRADER         PIC Z(4)9.                                   
001800*                                 ANTAL RADER                             
001900     03 RESP-RAD             OCCURS 1000 TIMES.                           
002000*                                                                         
002100        05 RESP-TIBEGPAC     PIC 9(5).                                    
002200*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
002300        05 RESP-KDORDKL      PIC 9.                                       
002400*                                 ORDERKLASS                              
002500        05 RESP-IDDISTR      PIC Z(3)9.                                   
002600*                                 DISTRIKTNUMMER                          
002700        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
002800*                                 KUNDNUMMER                              
002900        05 RESP-IDORDNR      PIC Z(4)9.                                   
003000*                                 ORDERNUMMER                             
003100        05 RESP-KDFRAKT      PIC Z9.                                      
003200*                                 FRAKTSÄTT DC TILL KUND                  
003300        05 RESP-IDPRODNR     PIC Z(6)9.                                   
003400*                                 PRODUKTIONSNUMMER                       
003500        05 RESP-KVORDRAD     PIC Z(4)9.                                   
003600*                                 ANTAL ORDERRADER                        
003700        05 RESP-KVORDRAD-PACK                                             
003800                             PIC Z(4)9.                                   
003900*                                 ANTAL PACKADE ORDERRADER                
004000        05 RESP-KVKOLPAC     PIC Z(3)9.                                   
004100*                                 ANTAL PACK RAPPORTERADE KOLLI           
004200        05 RESP-TIUTSKR      PIC 9(5).                                    
004300*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
004400        05 RESP-IDANSTNR     PIC Z(4)9.                                   
004500*                                 ANSTÄLLNINGSNUMMER                      
004600*** END OF VILMAII-COPY LENGTH= 54035 BYTES                               

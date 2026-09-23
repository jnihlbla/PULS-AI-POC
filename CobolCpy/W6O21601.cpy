000100 01  MOD-W6O21601.                                                        
000200*                                 MOD TILL W60216                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDFKNGRP-IN      PIC X(4).                                    
001200*                                 FUNKTIONSGRUPP                          
001300     03 MOD-IDFKNGRP-UT      PIC X(4).                                    
001400*                                 FUNKTIONSGRUPP                          
001500     03 MOD-KDLEVSP-IN       PIC X(2).                                    
001600*                                 SPÄRRKOD LEVERANS                       
001700     03 MOD-KDLEVSP-UT       PIC X(2).                                    
001800*                                 SPÄRRKOD LEVERANS                       
001900     03 MOD-FL-KVSPARR-KVAL-IN                                            
002000                             PIC X.                                       
002100*                                 ALLMÄN FLAGGA                           
002200     03 MOD-FL-KVSPARR-KVAL-UT                                            
002300                             PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500     03 MOD-IDUSER-SPKVAL-IN PIC X(8).                                    
002600*                                 ANVÄNDAR-ID KVALITETSPÄRR               
002700     03 MOD-IDUSER-SPKVAL-UT PIC X(8).                                    
002800*                                 ANVÄNDAR-ID KVALITETSPÄRR               
002900     03 MOD-IDPERSON-IN      PIC Z(3).                                    
003000*                                 PERSONKOD                               
003100     03 MOD-IDPERSON-UT      PIC Z(3).                                    
003200*                                 PERSONKOD                               
003300     03 MOD-RAD              OCCURS 10 TIMES.                             
003400        05 MOD-IDARTNR       PIC Z(7)9.                                   
003500*                                 ARTIKELNUMMER                           
003600        05 MOD-IDDC          PIC X(2).                                    
003700*                                 IDENTIFIERARE LAGER                     
003800        05 MOD-IDFKNGRP      PIC Z(3)9.                                   
003900*                                 FUNKTIONSGRUPP                          
004000        05 MOD-BEART10       PIC X(10).                                   
004100        05 MOD-KDLEVSP       PIC Z9.                                      
004200*                                 SPÄRRKOD LEVERANS                       
004300        05 MOD-KVSPARR-KVAL  PIC Z(6)9.                                   
004400*                                 SPÄRRAT ANTAL KVALITETSFEL              
004500        05 MOD-KVLS          PIC -(6)9.                                   
004600*                                 LAGERSALDO                              
004700        05 MOD-KVROS         PIC -(6)9.                                   
004800*                                 RESTORDERSALDO                          
004900        05 MOD-TISPARR-KVAL  PIC 9(6).                                    
005000*                                 SPÄRRAD DATUM KVALITETSFEL              
005100        05 MOD-IDUSER-SPKVAL PIC X(8).                                    
005200*                                 ANVÄNDAR-ID KVALITETSPÄRR               
005300     03 MOD-KDSORT1-ATTR     PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-KDSORT1          PIC X.                                       
005600*                                 SORTERINGSKOD                           
005700     03 MOD-IDNODE-ATTR      PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-IDNODE           PIC X(8).                                    
006000*                                 VTAM NODE-NAMN                          
006100     03 MOD-TEMFSINF         PIC X(55).                                   
006200*                                 INFORMATIONSMEDDELANDE                  
006300*** END OF VILMAII-COPY LENGTH= 762 BYTES                                 
